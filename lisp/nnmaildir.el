;;; nnmaildir.el --- maildir backend for Gnus
;; Public domain.

;; Author: Paul Jarc <prj@po.cwru.edu>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Maildir format is documented at <URL:http://cr.yp.to/proto/maildir.html>
;; and in the maildir(5) man page from qmail (available at
;; <URL:http://www.qmail.org/man/man5/maildir.html>).  nnmaildir also stores
;; extra information in the .nnmaildir/ directory within a maildir.
;;
;; Some goals of nnmaildir:
;; * Everything Just Works, and correctly.  E.g., NOV data is automatically
;;   regenerated when stale; no need for manually running
;;   *-generate-nov-databases.
;; * Perfect reliability: [C-g] will never corrupt its data in memory, and
;;   SIGKILL will never corrupt its data in the filesystem.
;; * Allow concurrent operation as much as possible.  If files change out
;;   from under us, adapt to the changes or degrade gracefully.
;; * We use the filesystem as a database, so that, e.g., it's easy to
;;   manipulate marks from outside Gnus.
;; * All information about a group is stored in the maildir, for easy backup,
;;   copying, restoring, etc.
;;
;; Todo:
;; * Merge the information from <URL:http://multivac.cwru.edu./nnmaildir/>
;;   into the Gnus manual.
;; * Allow create-directory = ".", and configurable prefix of maildir names,
;;   stripped off to produce group names.
;; * Add a hook for when moving messages from new/ to cur/, to support
;;   nnmail's duplicate detection.
;; * Allow each mark directory in a group to have its own inode for mark
;;   files, to accommodate AFS.
;; * Improve generated Xrefs, so crossposts are detectable.
;; * Improve readability.

;;; Code:

;; eval this before editing
[(progn
   (put 'nnmaildir--with-nntp-buffer 'lisp-indent-function 0)
   (put 'nnmaildir--with-work-buffer 'lisp-indent-function 0)
   (put 'nnmaildir--with-nov-buffer  'lisp-indent-function 0)
   (put 'nnmaildir--with-move-buffer 'lisp-indent-function 0)
   )
]

(eval-and-compile
  (require 'nnheader)
  (require 'gnus)
  (require 'gnus-util)
  (require 'gnus-range)
  (require 'gnus-start)
  (require 'gnus-int)
  (require 'message))
(eval-when-compile
  (require 'cl)
  (require 'nnmail))

(defconst nnmaildir-version "Gnus")

(defvar nnmaildir-article-file-name nil
  "*The filename of the most recently requested article.  This variable is set
by nnmaildir-request-article.")

;; The filename of the article being moved/copied:
(defvar nnmaildir--file nil)

;; Variables to generate filenames of messages being delivered:
(defvar   nnmaildir--delivery-time "")
(defconst nnmaildir--delivery-pid  (number-to-string (emacs-pid)))
(defvar   nnmaildir--delivery-ct   nil)

;; An obarry containing symbols whose names are server names and whose values
;; are servers:
(defvar nnmaildir--servers (make-vector 3 0))
;; The current server:
(defvar nnmaildir--cur-server nil)

;; A copy of nnmail-extra-headers
(defvar nnmaildir--extra nil)

;; A NOV structure looks like this (must be prin1-able, so no defstruct):
["subject\tfrom\tdate"
 "references\tchars\lines"
 "To: you\tIn-Reply-To: <your.mess@ge>"
 (12345 67890)     ;; modtime of the corresponding article file
 (to in-reply-to)] ;; contemporary value of nnmail-extra-headers
(defconst nnmaildir--novlen 5)
(defmacro nnmaildir--nov-new (beg mid end mtime extra)
  `(vector ,beg ,mid ,end ,mtime ,extra))
(defmacro nnmaildir--nov-get-beg   (nov) `(aref ,nov 0))
(defmacro nnmaildir--nov-get-mid   (nov) `(aref ,nov 1))
(defmacro nnmaildir--nov-get-end   (nov) `(aref ,nov 2))
(defmacro nnmaildir--nov-get-mtime (nov) `(aref ,nov 3))
(defmacro nnmaildir--nov-get-extra (nov) `(aref ,nov 4))
(defmacro nnmaildir--nov-set-beg   (nov value) `(aset ,nov 0 ,value))
(defmacro nnmaildir--nov-set-mid   (nov value) `(aset ,nov 1 ,value))
(defmacro nnmaildir--nov-set-end   (nov value) `(aset ,nov 2 ,value))
(defmacro nnmaildir--nov-set-mtime (nov value) `(aset ,nov 3 ,value))
(defmacro nnmaildir--nov-set-extra (nov value) `(aset ,nov 4 ,value))

(defstruct nnmaildir--art
  (prefix nil :type string)  ;; "time.pid.host"
  (suffix nil :type string)  ;; ":2,flags"
  (num    nil :type natnum)  ;; article number
  (msgid  nil :type string)  ;; "<mess.age@id>"
  (nov    nil :type vector)) ;; cached nov structure, or nil

(defstruct nnmaildir--grp
  (name  nil :type string)  ;; "group.name"
  (new   nil :type list)    ;; new/ modtime
  (cur   nil :type list)    ;; cur/ modtime
  (min   1   :type natnum)  ;; minimum article number
  (count 0   :type natnum)  ;; count of articles
  (nlist nil :type list)    ;; list of articles, ordered descending by number
  (flist nil :type vector)  ;; obarray mapping filename prefix->article
  (mlist nil :type vector)  ;; obarray mapping message-id->article
  (cache nil :type vector)  ;; nov cache
  (index nil :type natnum)  ;; index of next cache entry to replace
  (mmth  nil :type vector)) ;; obarray mapping mark name->dir modtime
					; ("Mark Mod Time Hash")

(defstruct nnmaildir--srv
  (address    nil :type string)         ;; server address string
  (method     nil :type list)           ;; (nnmaildir "address" ...)
  (prefix     nil :type string)         ;; "nnmaildir+address:"
  (dir        nil :type string)         ;; "/expanded/path/to/server/dir/"
  (ls         nil :type function)       ;; directory-files function
  (groups     nil :type vector)         ;; obarray mapping group names->groups
  (curgrp     nil :type nnmaildir--grp) ;; current group, or nil
  (error      nil :type string)         ;; last error message, or nil
  (mtime      nil :type list)           ;; modtime of dir
  (gnm        nil)                      ;; flag: split from mail-sources?
  (create-dir nil :type string))        ;; group creation directory

(defun nnmaildir--expired-article (group article)
  (setf (nnmaildir--art-nov article) nil)
  (let ((flist  (nnmaildir--grp-flist group))
	(mlist  (nnmaildir--grp-mlist group))
	(min    (nnmaildir--grp-min   group))
	(count  (1- (nnmaildir--grp-count group)))
	(prefix (nnmaildir--art-prefix article))
	(msgid  (nnmaildir--art-msgid  article))
	(new-nlist nil)
	(nlist-pre '(nil . nil))
	nlist-post num)
    (unless (zerop count)
      (setq nlist-post (nnmaildir--grp-nlist group)
	    num (nnmaildir--art-num article))
      (if (eq num (caar nlist-post))
	  (setq new-nlist (cdr nlist-post))
	(setq new-nlist nlist-post
	      nlist-pre nlist-post
	      nlist-post (cdr nlist-post))
	(while (/= num (caar nlist-post))
	  (setq nlist-pre nlist-post
		nlist-post (cdr nlist-post)))
	(setq nlist-post (cdr nlist-post))
	(if (eq num min)
	    (setq min (caar nlist-pre)))))
    (let ((inhibit-quit t))
      (setf (nnmaildir--grp-min   group) min)
      (setf (nnmaildir--grp-count group) count)
      (setf (nnmaildir--grp-nlist group) new-nlist)
      (setcdr nlist-pre nlist-post)
      (unintern prefix flist)
      (unintern msgid mlist))))

(defun nnmaildir--nlist-art (group num)
  (let ((entry (assq num (nnmaildir--grp-nlist group))))
    (if entry
	(cdr entry))))
(defmacro nnmaildir--flist-art (list file)
  `(symbol-value (intern-soft ,file ,list)))
(defmacro nnmaildir--mlist-art (list msgid)
  `(symbol-value (intern-soft ,msgid ,list)))

(defun nnmaildir--pgname (server gname)
  (let ((prefix (nnmaildir--srv-prefix server)))
    (if prefix (concat prefix gname)
      (setq gname (gnus-group-prefixed-name gname
					    (nnmaildir--srv-method server)))
      (setf (nnmaildir--srv-prefix server) (gnus-group-real-prefix gname))
      gname)))

(defun nnmaildir--param (pgname param)
  (setq param (gnus-group-find-parameter pgname param 'allow-list))
  (if (vectorp param) (setq param (aref param 0)))
  (eval param))

(defmacro nnmaildir--with-nntp-buffer (&rest body)
  `(save-excursion
     (set-buffer nntp-server-buffer)
     ,@body))
(defmacro nnmaildir--with-work-buffer (&rest body)
  `(save-excursion
     (set-buffer (get-buffer-create " *nnmaildir work*"))
     ,@body))
(defmacro nnmaildir--with-nov-buffer (&rest body)
  `(save-excursion
     (set-buffer (get-buffer-create " *nnmaildir nov*"))
     ,@body))
(defmacro nnmaildir--with-move-buffer (&rest body)
  `(save-excursion
     (set-buffer (get-buffer-create " *nnmaildir move*"))
     ,@body))

(defmacro nnmaildir--subdir (dir subdir)
  `(file-name-as-directory (concat ,dir ,subdir)))
(defmacro nnmaildir--srvgrp-dir (srv-dir gname)
  `(nnmaildir--subdir ,srv-dir ,gname))
(defmacro nnmaildir--tmp       (dir) `(nnmaildir--subdir ,dir "tmp"))
(defmacro nnmaildir--new       (dir) `(nnmaildir--subdir ,dir "new"))
(defmacro nnmaildir--cur       (dir) `(nnmaildir--subdir ,dir "cur"))
(defmacro nnmaildir--nndir     (dir) `(nnmaildir--subdir ,dir ".nnmaildir"))
(defmacro nnmaildir--nov-dir   (dir) `(nnmaildir--subdir ,dir "nov"))
(defmacro nnmaildir--marks-dir (dir) `(nnmaildir--subdir ,dir "marks"))
(defmacro nnmaildir--num-dir   (dir) `(nnmaildir--subdir ,dir "num"))
(defmacro nnmaildir--num-file  (dir) `(concat ,dir ":"))

(defmacro nnmaildir--unlink (file-arg)
  `(let ((file ,file-arg))
     (if (file-attributes file) (delete-file file))))
(defun nnmaildir--mkdir (dir)
  (or (file-exists-p (file-name-as-directory dir))
      (make-directory-internal (directory-file-name dir))))
(defun nnmaildir--delete-dir-files (dir ls)
  (mapcar 'delete-file (funcall ls dir 'full "\\`[^.]" 'nosort))
  (delete-directory dir))

(defun nnmaildir--group-maxnum (server group)
  (if (zerop (nnmaildir--grp-count group)) 0
    (let ((x (nnmaildir--srvgrp-dir (nnmaildir--srv-dir server)
				    (nnmaildir--grp-name group))))
      (setq x (nnmaildir--nndir x)
	    x (nnmaildir--num-dir x)
	    x (nnmaildir--num-file x)
	    x (file-attributes x))
      (if x (1- (nth 1 x)) 0))))

;; Make the given server, if non-nil, be the current server.  Then make the
;; given group, if non-nil, be the current group of the current server.  Then
;; return the group object for the current group.
(defun nnmaildir--prepare (server group)
  (let (x groups)
    (catch 'return
      (if (null server)
	  (unless (setq server nnmaildir--cur-server)
	    (throw 'return nil))
	(unless (setq server (intern-soft server nnmaildir--servers))
	  (throw 'return nil))
	(setq server (symbol-value server)
	      nnmaildir--cur-server server))
      (unless (setq groups (nnmaildir--srv-groups server))
	(throw 'return nil))
      (unless (nnmaildir--srv-method server)
	(setq x (concat "nnmaildir:" (nnmaildir--srv-address server))
	      x (gnus-server-to-method x))
	(unless x (throw 'return nil))
	(setf (nnmaildir--srv-method server) x))
      (if (null group)
	  (unless (setq group (nnmaildir--srv-curgrp server))
	    (throw 'return nil))
	(unless (setq group (intern-soft group groups))
	  (throw 'return nil))
	(setq group (symbol-value group)))
      group)))

(defun nnmaildir--tab-to-space (string)
  (let ((pos 0))
    (while (string-match "\t" string pos)
      (aset string (match-beginning 0) ? )
      (setq pos (match-end 0))))
  string)

(defun nnmaildir--update-nov (server group article)
  (let ((nnheader-file-coding-system 'binary)
	(srv-dir (nnmaildir--srv-dir server))
	(storage-version 1) ;; [version article-number msgid [...nov...]]
	dir gname pgname msgdir prefix suffix file attr mtime novdir novfile
	nov msgid nov-beg nov-mid nov-end field val old-extra num numdir
	deactivate-mark)
    (catch 'return
      (setq gname (nnmaildir--grp-name group)
	    pgname (nnmaildir--pgname server gname)
	    dir (nnmaildir--srvgrp-dir srv-dir gname)
	    msgdir (if (nnmaildir--param pgname 'read-only)
		       (nnmaildir--new dir) (nnmaildir--cur dir))
	    prefix (nnmaildir--art-prefix article)
	    suffix (nnmaildir--art-suffix article)
	    file (concat msgdir prefix suffix)
	    attr (file-attributes file))
      (unless attr
	(nnmaildir--expired-article group article)
	(throw 'return nil))
      (setq mtime (nth 5 attr)
	    attr (nth 7 attr)
	    nov (nnmaildir--art-nov article)
	    dir (nnmaildir--nndir dir)
	    novdir (nnmaildir--nov-dir dir)
	    novfile (concat novdir prefix))
      (unless (equal nnmaildir--extra nnmail-extra-headers)
	(setq nnmaildir--extra (copy-sequence nnmail-extra-headers)))
      (nnmaildir--with-nov-buffer
	;; First we'll check for already-parsed NOV data.
	(cond ((not (file-exists-p novfile))
	       ;; The NOV file doesn't exist; we have to parse the message.
	       (setq nov nil))
	      ((not nov)
	       ;; The file exists, but the data isn't in memory; read the file.
	       (erase-buffer)
	       (nnheader-insert-file-contents novfile)
	       (setq nov (read (current-buffer)))
	       (if (not (and (vectorp nov)
			     (/= 0 (length nov))
			     (equal storage-version (aref nov 0))))
		   ;; This NOV data seems to be in the wrong format.
		   (setq nov nil)
		 (unless (nnmaildir--art-num   article)
		   (setf (nnmaildir--art-num   article) (aref nov 1)))
		 (unless (nnmaildir--art-msgid article)
		   (setf (nnmaildir--art-msgid article) (aref nov 2)))
		 (setq nov (aref nov 3)))))
	;; Now check whether the already-parsed data (if we have any) is
	;; usable: if the message has been edited or if nnmail-extra-headers
	;; has been augmented since this data was parsed from the message,
	;; then we have to reparse.  Otherwise it's up-to-date.
	(when (and nov (equal mtime (nnmaildir--nov-get-mtime nov)))
	  ;; The timestamp matches.  Now check nnmail-extra-headers.
	  (setq old-extra (nnmaildir--nov-get-extra nov))
	  (when (equal nnmaildir--extra old-extra) ;; common case
	    ;; Save memory; use a single copy of the list value.
	    (nnmaildir--nov-set-extra nov nnmaildir--extra)
	    (throw 'return nov))
	  ;; They're not equal, but maybe the new is a subset of the old.
	  (if (null nnmaildir--extra)
	      ;; The empty set is a subset of every set.
	      (throw 'return nov))
	  (if (not (memq nil (mapcar (lambda (e) (memq e old-extra))
				     nnmaildir--extra)))
	      (throw 'return nov)))
	;; Parse the NOV data out of the message.
	(erase-buffer)
	(nnheader-insert-file-contents file)
	(insert "\n")
	(goto-char (point-min))
	(save-restriction
	  (if (search-forward "\n\n" nil 'noerror)
	      (progn
		(setq nov-mid (count-lines (point) (point-max)))
		(narrow-to-region (point-min) (1- (point))))
	    (setq nov-mid 0))
	  (goto-char (point-min))
	  (delete-char 1)
	  (setq nov (nnheader-parse-naked-head)
		field (or (mail-header-lines nov) 0)))
	(unless (or (zerop field) (nnmaildir--param pgname 'distrust-Lines:))
	  (setq nov-mid field))
	(setq nov-mid (number-to-string nov-mid)
	      nov-mid (concat (number-to-string attr) "\t" nov-mid))
	(save-match-data
	  (setq field (or (mail-header-references nov) ""))
	  (nnmaildir--tab-to-space field)
	  (setq nov-mid (concat field "\t" nov-mid)
		nov-beg (mapconcat
			  (lambda (f) (nnmaildir--tab-to-space (or f "")))
			  (list (mail-header-subject nov)
				(mail-header-from nov)
				(mail-header-date nov)) "\t")
		nov-end (mapconcat
			  (lambda (extra)
			    (setq field (symbol-name (car extra))
				  val (cdr extra))
			    (nnmaildir--tab-to-space field)
			    (nnmaildir--tab-to-space val)
			    (concat field ": " val))
			  (mail-header-extra nov) "\t")))
	(setq msgid (mail-header-id nov))
	(if (or (null msgid) (nnheader-fake-message-id-p msgid))
	    (setq msgid (concat "<" prefix "@nnmaildir>")))
	(nnmaildir--tab-to-space msgid)
	;; The data is parsed; create an nnmaildir NOV structure.
	(setq nov (nnmaildir--nov-new nov-beg nov-mid nov-end mtime
				      nnmaildir--extra)
	      num (nnmaildir--art-num article))
	(unless num
	  ;; Allocate a new article number.
	  (erase-buffer)
	  (setq numdir (nnmaildir--num-dir dir)
		file (nnmaildir--num-file numdir)
		num -1)
	  (nnmaildir--mkdir numdir)
	  (write-region "" nil file nil 'no-message)
	  (while file
	    ;; Get the number of links to file.
	    (setq attr (nth 1 (file-attributes file)))
	    (if (= attr num)
		;; We've already tried this number, in the previous loop
		;; iteration, and failed.
		(signal 'error `("Corrupt internal nnmaildir data" ,numdir)))
	    ;; If attr is 123, try to link file to "123".  This atomically
	    ;; increases the link count and creates the "123" link, failing
	    ;; if that link was already created by another Gnus, just after
	    ;; we stat()ed file.
	    (condition-case nil
		(progn
		  (add-name-to-file file (concat numdir (format "%x" attr)))
		  (setq file nil)) ;; Stop looping.
	      (file-already-exists nil))
	    (setq num attr))
	  (setf (nnmaildir--art-num article) num))
	;; Store this new NOV data in a file
	(erase-buffer)
	(prin1 (vector storage-version num msgid nov) (current-buffer))
	(setq file (concat novfile ":"))
	(nnmaildir--unlink file)
	(write-region (point-min) (point-max) file nil 'no-message nil 'excl))
      (rename-file file novfile 'replace)
      (setf (nnmaildir--art-msgid article) msgid)
      nov)))

(defun nnmaildir--cache-nov (group article nov)
  (let ((cache (nnmaildir--grp-cache group))
	(index (nnmaildir--grp-index group))
	goner)
    (unless (nnmaildir--art-nov article)
      (setq goner (aref cache index))
      (if goner (setf (nnmaildir--art-nov goner) nil))
      (aset cache index article)
      (setf (nnmaildir--grp-index group) (% (1+ index) (length cache))))
    (setf (nnmaildir--art-nov article) nov)))

(defun nnmaildir--grp-add-art (server group article)
  (let ((nov (nnmaildir--update-nov server group article))
	count num min nlist nlist-cdr insert-nlist)
    (when nov
      (setq count (1+ (nnmaildir--grp-count group))
	    num (nnmaildir--art-num article)
	    min (if (= count 1) num
		  (min num (nnmaildir--grp-min group)))
	    nlist (nnmaildir--grp-nlist group))
      (if (or (null nlist) (> num (caar nlist)))
	  (setq nlist (cons (cons num article) nlist))
	(setq insert-nlist t
	      nlist-cdr (cdr nlist))
	(while (and nlist-cdr (< num (caar nlist-cdr)))
	  (setq nlist nlist-cdr
		nlist-cdr (cdr nlist))))
      (let ((inhibit-quit t))
	(setf (nnmaildir--grp-count group) count)
	(setf (nnmaildir--grp-min group) min)
	(if insert-nlist
	    (setcdr nlist (cons (cons num article) nlist-cdr))
	  (setf (nnmaildir--grp-nlist group) nlist))
	(set (intern (nnmaildir--art-prefix article)
		     (nnmaildir--grp-flist group))
	     article)
	(set (intern (nnmaildir--art-msgid article)
		     (nnmaildir--grp-mlist group))
	     article)
	(set (intern (nnmaildir--grp-name group)
		     (nnmaildir--srv-groups server))
	     group))
      (nnmaildir--cache-nov group article nov)
      t)))

(defun nnmaildir--group-ls (server pgname)
  (or (nnmaildir--param pgname 'directory-files)
      (nnmaildir--srv-ls server)))

(defun nnmaildir-article-number-to-file-name
  (number group-name server-address-string)
  (let ((group (nnmaildir--prepare server-address-string group-name))
	article dir pgname)
    (catch 'return
      (unless group
	;; The given group or server does not exist.
	(throw 'return nil))
      (setq article (nnmaildir--nlist-art group number))
      (unless article
	;; The given article number does not exist in this group.
	(throw 'return nil))
      (setq pgname (nnmaildir--pgname nnmaildir--cur-server group-name)
	    dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    dir (nnmaildir--srvgrp-dir dir group-name)
	    dir (if (nnmaildir--param pgname 'read-only)
		    (nnmaildir--new dir) (nnmaildir--cur dir)))
      (concat dir (nnmaildir--art-prefix article)
	      (nnmaildir--art-suffix article)))))

(defun nnmaildir-article-number-to-base-name
  (number group-name server-address-string)
  (let ((x (nnmaildir--prepare server-address-string group-name)))
    (when x
      (setq x (nnmaildir--nlist-art x number))
      (and x (cons (nnmaildir--art-prefix x)
		   (nnmaildir--art-suffix x))))))

(defun nnmaildir-base-name-to-article-number
  (base-name group-name server-address-string)
  (let ((x (nnmaildir--prepare server-address-string group-name)))
    (when x
      (setq x (nnmaildir--grp-flist x)
	    x (nnmaildir--flist-art x base-name))
      (and x (nnmaildir--art-num x)))))

(defun nnmaildir--nlist-iterate (nlist ranges func)
  (let (entry high low nlist2)
    (if (eq ranges 'all)
	(setq ranges `((1 . ,(caar nlist)))))
    (while ranges
      (setq entry (car ranges) ranges (cdr ranges))
      (while (and ranges (eq entry (car ranges)))
	(setq ranges (cdr ranges))) ;; skip duplicates
      (if (numberp entry)
	  (setq low entry
		high entry)
	(setq low (car entry)
	      high (cdr entry)))
      (setq nlist2 nlist) ;; Don't assume any sorting of ranges
      (catch 'iterate-loop
	(while nlist2
	  (if (<= (caar nlist2) high) (throw 'iterate-loop nil))
	  (setq nlist2 (cdr nlist2))))
      (catch 'iterate-loop
	(while nlist2
	  (setq entry (car nlist2) nlist2 (cdr nlist2))
	  (if (< (car entry) low) (throw 'iterate-loop nil))
	  (funcall func (cdr entry)))))))

(defun nnmaildir--up2-1 (n)
  (if (zerop n) 1 (1- (lsh 1 (1+ (logb n))))))

(defun nnmaildir-request-type (group &optional article)
  'mail)

(defun nnmaildir-status-message (&optional server)
  (nnmaildir--prepare server nil)
  (nnmaildir--srv-error nnmaildir--cur-server))

(defun nnmaildir-server-opened (&optional server)
  (and nnmaildir--cur-server
       (if server
	   (string-equal server (nnmaildir--srv-address nnmaildir--cur-server))
	 t)
       (nnmaildir--srv-groups nnmaildir--cur-server)
       t))

(defun nnmaildir-open-server (server &optional defs)
  (let ((x server)
	dir size)
    (catch 'return
      (setq server (intern-soft x nnmaildir--servers))
      (if server
	  (and (setq server (symbol-value server))
	       (nnmaildir--srv-groups server)
	       (setq nnmaildir--cur-server server)
	       (throw 'return t))
	(setq server (make-nnmaildir--srv :address x))
	(let ((inhibit-quit t))
	  (set (intern x nnmaildir--servers) server)))
      (setq dir (assq 'directory defs))
      (unless dir
	(setf (nnmaildir--srv-error server)
	      "You must set \"directory\" in the select method")
	(throw 'return nil))
      (setq dir (cadr dir)
	    dir (eval dir)
	    dir (expand-file-name dir)
	    dir (file-name-as-directory dir))
      (unless (file-exists-p dir)
	(setf (nnmaildir--srv-error server) (concat "No such directory: " dir))
	(throw 'return nil))
      (setf (nnmaildir--srv-dir server) dir)
      (setq x (assq 'directory-files defs))
      (if (null x)
	  (setq x (if nnheader-directory-files-is-safe 'directory-files
		    'nnheader-directory-files-safe))
	(setq x (cadr x))
	(unless (functionp x)
	  (setf (nnmaildir--srv-error server)
		(concat "Not a function: " (prin1-to-string x)))
	  (throw 'return nil)))
      (setf (nnmaildir--srv-ls server) x)
      (setq size (length (funcall x dir nil "\\`[^.]" 'nosort))
	    size (nnmaildir--up2-1 size))
      (and (setq x (assq 'get-new-mail defs))
	   (setq x (cdr x))
	   (car x)
	   (setf (nnmaildir--srv-gnm server) t)
	   (require 'nnmail))
      (setq x (assq 'create-directory defs))
      (when x
	(setq x (cadr x)
	      x (eval x))
	(setf (nnmaildir--srv-create-dir server) x))
      (setf (nnmaildir--srv-groups server) (make-vector size 0))
      (setq nnmaildir--cur-server server)
      t)))

(defun nnmaildir--parse-filename (file)
  (let ((prefix (car file))
	timestamp len)
    (if (string-match
	 "\\`\\([0-9]+\\)\\.\\([0-9]+\\)\\(_\\([0-9]+\\)\\)?\\(\\..*\\)\\'"
	 prefix)
	(progn
	  (setq timestamp (concat "0000" (match-string 1 prefix))
		len (- (length timestamp) 4))
	  (vector (string-to-number (substring timestamp 0 len))
		  (string-to-number (substring timestamp len))
		  (string-to-number (match-string 2 prefix))
		  (string-to-number (or (match-string 4 prefix) "-1"))
		  (match-string 5 prefix)
		  file))
      file)))

(defun nnmaildir--sort-files (a b)
  (catch 'return
    (if (consp a)
	(throw 'return (and (consp b) (string-lessp (car a) (car b)))))
    (if (consp b) (throw 'return t))
    (if (< (aref a 0) (aref b 0)) (throw 'return t))
    (if (> (aref a 0) (aref b 0)) (throw 'return nil))
    (if (< (aref a 1) (aref b 1)) (throw 'return t))
    (if (> (aref a 1) (aref b 1)) (throw 'return nil))
    (if (< (aref a 2) (aref b 2)) (throw 'return t))
    (if (> (aref a 2) (aref b 2)) (throw 'return nil))
    (if (< (aref a 3) (aref b 3)) (throw 'return t))
    (if (> (aref a 3) (aref b 3)) (throw 'return nil))
    (string-lessp (aref a 4) (aref b 4))))

(defun nnmaildir--scan (gname scan-msgs groups method srv-dir srv-ls)
  (catch 'return
    (let ((36h-ago (- (car (current-time)) 2))
	  absdir nndir tdir ndir cdir nattr cattr isnew pgname read-only ls
	  files num dir flist group x)
      (setq absdir (nnmaildir--srvgrp-dir srv-dir gname)
	    nndir (nnmaildir--nndir absdir))
      (unless (file-exists-p absdir)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such directory: " absdir))
	(throw 'return nil))
      (setq tdir (nnmaildir--tmp absdir)
	    ndir (nnmaildir--new absdir)
	    cdir (nnmaildir--cur absdir)
	    nattr (file-attributes ndir)
	    cattr (file-attributes cdir))
      (unless (and (file-exists-p tdir) nattr cattr)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Not a maildir: " absdir))
	(throw 'return nil))
      (setq group (nnmaildir--prepare nil gname)
	    pgname (nnmaildir--pgname nnmaildir--cur-server gname))
      (if group
	  (setq isnew nil)
	(setq isnew t
	      group (make-nnmaildir--grp :name gname :index 0))
	(nnmaildir--mkdir nndir)
	(nnmaildir--mkdir (nnmaildir--nov-dir   nndir))
	(nnmaildir--mkdir (nnmaildir--marks-dir nndir))
	(write-region "" nil (concat nndir "markfile") nil 'no-message))
      (setq read-only (nnmaildir--param pgname 'read-only)
	    ls (or (nnmaildir--param pgname 'directory-files) srv-ls))
      (unless read-only
	(setq x (nth 11 (file-attributes tdir)))
	(unless (and (= x (nth 11 nattr)) (= x (nth 11 cattr)))
	  (setf (nnmaildir--srv-error nnmaildir--cur-server)
		(concat "Maildir spans filesystems: " absdir))
	  (throw 'return nil))
	(mapcar
	 (lambda (file)
	   (setq x (file-attributes file))
	   (if (or (> (cadr x) 1) (< (car (nth 4 x)) 36h-ago))
	       (delete-file file)))
	 (funcall ls tdir 'full "\\`[^.]" 'nosort)))
      (or scan-msgs
	  isnew
	  (throw 'return t))
      (setq nattr (nth 5 nattr))
      (if (equal nattr (nnmaildir--grp-new group))
	  (setq nattr nil))
      (if read-only (setq dir (and (or isnew nattr) ndir))
	(when (or isnew nattr)
	  (mapcar
	   (lambda (file)
	     (rename-file (concat ndir file) (concat cdir file ":2,")))
	   (funcall ls ndir nil "\\`[^.]" 'nosort))
	  (setf (nnmaildir--grp-new group) nattr))
	(setq cattr (nth 5 (file-attributes cdir)))
	(if (equal cattr (nnmaildir--grp-cur group))
	    (setq cattr nil))
	(setq dir (and (or isnew cattr) cdir)))
      (unless dir (throw 'return t))
      (setq files (funcall ls dir nil "\\`[^.]" 'nosort)
	    files (save-match-data
		    (mapcar
		     (lambda (f)
		       (string-match "\\`\\([^:]*\\)\\(\\(:.*\\)?\\)\\'" f)
		       (cons (match-string 1 f) (match-string 2 f)))
		     files)))
      (when isnew
	(setq num (nnmaildir--up2-1 (length files)))
	(setf (nnmaildir--grp-flist group) (make-vector num 0))
	(setf (nnmaildir--grp-mlist group) (make-vector num 0))
	(setf (nnmaildir--grp-mmth group) (make-vector 1 0))
	(setq num (nnmaildir--param pgname 'nov-cache-size))
	(if (numberp num) (if (< num 1) (setq num 1))
	  (setq num 16
		cdir (nnmaildir--marks-dir nndir)
		ndir (nnmaildir--subdir cdir "tick")
		cdir (nnmaildir--subdir cdir "read"))
	  (mapcar
	   (lambda (file)
	     (setq file (car file))
	     (if (or (not (file-exists-p (concat cdir file)))
		     (file-exists-p (concat ndir file)))
		 (setq num (1+ num))))
	   files))
	(setf (nnmaildir--grp-cache group) (make-vector num nil))
        (let ((inhibit-quit t))
          (set (intern gname groups) group))
	(or scan-msgs (throw 'return t)))
      (setq flist (nnmaildir--grp-flist group)
	    files (mapcar
		   (lambda (file)
		     (and (null (nnmaildir--flist-art flist (car file)))
			  file))
		   files)
	    files (delq nil files)
	    files (mapcar 'nnmaildir--parse-filename files)
	    files (sort files 'nnmaildir--sort-files))
      (mapcar
       (lambda (file)
	 (setq file (if (consp file) file (aref file 5))
	       x (make-nnmaildir--art :prefix (car file) :suffix (cdr file)))
	 (nnmaildir--grp-add-art nnmaildir--cur-server group x))
       files)
      (if read-only (setf (nnmaildir--grp-new group) nattr)
	(setf (nnmaildir--grp-cur group) cattr)))
    t))

(defun nnmaildir-request-scan (&optional scan-group server)
  (let ((coding-system-for-write nnheader-file-coding-system)
	(output-coding-system nnheader-file-coding-system)
	(buffer-file-coding-system nil)
	(file-coding-system nil)
	(file-coding-system-alist nil)
	(nnmaildir-get-new-mail t)
	(nnmaildir-group-alist nil)
	(nnmaildir-active-file nil)
	x srv-ls srv-dir method groups group dirs grp-dir seen deactivate-mark)
    (nnmaildir--prepare server nil)
    (setq srv-ls (nnmaildir--srv-ls nnmaildir--cur-server)
	  srv-dir (nnmaildir--srv-dir nnmaildir--cur-server)
	  method (nnmaildir--srv-method nnmaildir--cur-server)
	  groups (nnmaildir--srv-groups nnmaildir--cur-server))
    (nnmaildir--with-work-buffer
      (save-match-data
	(if (stringp scan-group)
	    (if (nnmaildir--scan scan-group t groups method srv-dir srv-ls)
		(if (nnmaildir--srv-gnm nnmaildir--cur-server)
		    (nnmail-get-new-mail 'nnmaildir nil nil scan-group))
	      (unintern scan-group groups))
	  (setq x (nth 5 (file-attributes srv-dir))
		scan-group (null scan-group))
	  (if (equal x (nnmaildir--srv-mtime nnmaildir--cur-server))
	      (if scan-group
		  (mapatoms (lambda (sym)
			      (nnmaildir--scan (symbol-name sym) t groups
					       method srv-dir srv-ls))
			    groups))
	    (setq dirs (funcall srv-ls srv-dir nil "\\`[^.]" 'nosort)
		  seen (nnmaildir--up2-1 (length dirs))
		  seen (make-vector seen 0))
	    (mapcar
	     (lambda (grp-dir)
	       (if (nnmaildir--scan grp-dir scan-group groups method srv-dir
				    srv-ls)
		   (intern grp-dir seen)))
	     dirs)
	    (setq x nil)
	    (mapatoms (lambda (group)
			(setq group (symbol-name group))
			(unless (intern-soft group seen)
			  (setq x (cons group x))))
		      groups)
	    (mapcar (lambda (grp) (unintern grp groups)) x)
	    (setf (nnmaildir--srv-mtime nnmaildir--cur-server)
		  (nth 5 (file-attributes srv-dir))))
	  (and scan-group
	       (nnmaildir--srv-gnm nnmaildir--cur-server)
	       (nnmail-get-new-mail 'nnmaildir nil nil))))))
  t)

(defun nnmaildir-request-list (&optional server)
  (nnmaildir-request-scan 'find-new-groups server)
  (let (pgname ro deactivate-mark)
    (nnmaildir--prepare server nil)
    (nnmaildir--with-nntp-buffer
      (erase-buffer)
      (mapatoms (lambda (group)
		  (setq pgname (symbol-name group)
			pgname (nnmaildir--pgname nnmaildir--cur-server pgname)
			group (symbol-value group)
			ro (nnmaildir--param pgname 'read-only))
		  (insert (nnmaildir--grp-name group) " ")
                  (princ (nnmaildir--group-maxnum nnmaildir--cur-server group)
			 nntp-server-buffer)
		  (insert " ")
                  (princ (nnmaildir--grp-min group) nntp-server-buffer)
		  (insert " " (if ro "n" "y") "\n"))
		(nnmaildir--srv-groups nnmaildir--cur-server))))
  t)

(defun nnmaildir-request-newgroups (date &optional server)
  (nnmaildir-request-list server))

(defun nnmaildir-retrieve-groups (groups &optional server)
  (let (group deactivate-mark)
    (nnmaildir--prepare server nil)
    (nnmaildir--with-nntp-buffer
      (erase-buffer)
      (mapcar
       (lambda (gname)
	 (setq group (nnmaildir--prepare nil gname))
	 (if (null group) (insert "411 no such news group\n")
	   (insert "211 ")
	   (princ (nnmaildir--grp-count group) nntp-server-buffer)
	   (insert " ")
	   (princ (nnmaildir--grp-min   group) nntp-server-buffer)
	   (insert " ")
	   (princ (nnmaildir--group-maxnum nnmaildir--cur-server group)
		  nntp-server-buffer)
	   (insert " " gname "\n")))
       groups)))
  'group)

(defun nnmaildir-request-update-info (gname info &optional server)
  (let ((group (nnmaildir--prepare server gname))
	pgname flist all always-marks never-marks old-marks dotfile num dir
	markdirs marks mark ranges markdir article read end new-marks ls
	old-mmth new-mmth mtime mark-sym deactivate-mark)
    (catch 'return
      (unless group
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(throw 'return nil))
      (setq gname (nnmaildir--grp-name group)
	    pgname (nnmaildir--pgname nnmaildir--cur-server gname)
	    flist (nnmaildir--grp-flist group))
      (when (zerop (nnmaildir--grp-count group))
	(gnus-info-set-read info nil)
	(gnus-info-set-marks info nil 'extend)
	(throw 'return info))
      (setq old-marks (cons 'read (gnus-info-read info))
	    old-marks (cons old-marks (gnus-info-marks info))
	    always-marks (nnmaildir--param pgname 'always-marks)
	    never-marks (nnmaildir--param pgname 'never-marks)
	    dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    dir (nnmaildir--srvgrp-dir dir gname)
	    dir (nnmaildir--nndir dir)
	    dir (nnmaildir--marks-dir dir)
            ls (nnmaildir--group-ls nnmaildir--cur-server pgname)
	    markdirs (funcall ls dir nil "\\`[^.]" 'nosort)
	    new-mmth (nnmaildir--up2-1 (length markdirs))
	    new-mmth (make-vector new-mmth 0)
	    old-mmth (nnmaildir--grp-mmth group))
      (mapcar
       (lambda (mark)
	 (setq markdir (nnmaildir--subdir dir mark)
	       mark-sym (intern mark)
	       ranges nil)
	 (catch 'got-ranges
	   (if (memq mark-sym never-marks) (throw 'got-ranges nil))
	   (when (memq mark-sym always-marks)
	     (unless all
	       (setq all (nnmaildir--grp-nlist group)
		     all (mapcar 'car all)
		     all (nreverse all)
		     all (gnus-compress-sequence all 'always-list)
		     all (cons 'dummy-mark-symbol all)))
	     (setq ranges (cdr all))
	     (throw 'got-ranges nil))
	   (setq mtime (nth 5 (file-attributes markdir)))
	   (set (intern mark new-mmth) mtime)
	   (when (equal mtime (symbol-value (intern-soft mark old-mmth)))
	     (setq ranges (assq mark-sym old-marks))
	     (if ranges (setq ranges (cdr ranges)))
	     (throw 'got-ranges nil))
	   (mapcar
	    (lambda (prefix)
	      (setq article (nnmaildir--flist-art flist prefix))
	      (if article
		  (setq ranges
			(gnus-add-to-range ranges
					   `(,(nnmaildir--art-num article))))))
	    (funcall ls markdir nil "\\`[^.]" 'nosort)))
	 (if (eq mark-sym 'read) (setq read ranges)
	   (if ranges (setq marks (cons (cons mark-sym ranges) marks)))))
       markdirs)
      (gnus-info-set-read info read)
      (gnus-info-set-marks info marks 'extend)
      (setf (nnmaildir--grp-mmth group) new-mmth)
      info)))

(defun nnmaildir-request-group (gname &optional server fast)
  (let ((group (nnmaildir--prepare server gname))
	deactivate-mark)
    (catch 'return
      (unless group
	;; (insert "411 no such news group\n")
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(throw 'return nil))
      (setf (nnmaildir--srv-curgrp nnmaildir--cur-server) group)
      (if fast (throw 'return t))
      (nnmaildir--with-nntp-buffer
	(erase-buffer)
	(insert "211 ")
	(princ (nnmaildir--grp-count group) nntp-server-buffer)
	(insert " ")
	(princ (nnmaildir--grp-min   group) nntp-server-buffer)
	(insert " ")
	(princ (nnmaildir--group-maxnum nnmaildir--cur-server group)
	       nntp-server-buffer)
	(insert " " gname "\n")
	t))))

(defun nnmaildir-request-create-group (gname &optional server args)
  (nnmaildir--prepare server nil)
  (catch 'return
    (let ((create-dir (nnmaildir--srv-create-dir nnmaildir--cur-server))
	  srv-dir dir groups)
      (when (zerop (length gname))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Invalid (empty) group name")
	(throw 'return nil))
      (when (eq (aref "." 0) (aref gname 0))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Group names may not start with \".\"")
	(throw 'return nil))
      (when (save-match-data (string-match "[\0/\t]" gname))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Illegal characters (null, tab, or /) in group name: "
		      gname))
	(throw 'return nil))
      (setq groups (nnmaildir--srv-groups nnmaildir--cur-server))
      (when (intern-soft gname groups)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Group already exists: " gname))
	(throw 'return nil))
      (setq srv-dir (nnmaildir--srv-dir nnmaildir--cur-server))
      (if (file-name-absolute-p create-dir)
	  (setq dir (expand-file-name create-dir))
	(setq dir srv-dir
	      dir (file-truename dir)
	      dir (concat dir create-dir)))
      (setq dir (nnmaildir--subdir (file-name-as-directory dir) gname))
      (nnmaildir--mkdir dir)
      (nnmaildir--mkdir (nnmaildir--tmp dir))
      (nnmaildir--mkdir (nnmaildir--new dir))
      (nnmaildir--mkdir (nnmaildir--cur dir))
      (setq create-dir (file-name-as-directory create-dir))
      (make-symbolic-link (concat create-dir gname) (concat srv-dir gname))
      (nnmaildir-request-scan 'find-new-groups))))

(defun nnmaildir-request-rename-group (gname new-name &optional server)
  (let ((group (nnmaildir--prepare server gname))
	(coding-system-for-write nnheader-file-coding-system)
	(output-coding-system nnheader-file-coding-system)
	(buffer-file-coding-system nil)
	(file-coding-system nil)
	(file-coding-system-alist nil)
	srv-dir x groups)
    (catch 'return
      (unless group
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(throw 'return nil))
      (when (zerop (length new-name))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Invalid (empty) group name")
	(throw 'return nil))
      (when (eq (aref "." 0) (aref new-name 0))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Group names may not start with \".\"")
	(throw 'return nil))
      (when (save-match-data (string-match "[\0/\t]" new-name))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Illegal characters (null, tab, or /) in group name: "
		      new-name))
	(throw 'return nil))
      (if (string-equal gname new-name) (throw 'return t))
      (when (intern-soft new-name
			 (nnmaildir--srv-groups nnmaildir--cur-server))
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Group already exists: " new-name))
	(throw 'return nil))
      (setq srv-dir (nnmaildir--srv-dir nnmaildir--cur-server))
      (condition-case err
	  (rename-file (concat srv-dir gname)
		       (concat srv-dir new-name))
	(error
	 (setf (nnmaildir--srv-error nnmaildir--cur-server)
	       (concat "Error renaming link: " (prin1-to-string err)))
	 (throw 'return nil)))
      (setq x (nnmaildir--srv-groups nnmaildir--cur-server)
	    groups (make-vector (length x) 0))
      (mapatoms (lambda (sym)
		  (unless (eq (symbol-value sym) group)
		    (set (intern (symbol-name sym) groups)
			 (symbol-value sym))))
		x)
      (setq group (copy-sequence group))
      (setf (nnmaildir--grp-name group) new-name)
      (set (intern new-name groups) group)
      (setf (nnmaildir--srv-groups nnmaildir--cur-server) groups)
      t)))

(defun nnmaildir-request-delete-group (gname force &optional server)
  (let ((group (nnmaildir--prepare server gname))
	pgname grp-dir dir ls deactivate-mark)
    (catch 'return
      (unless group
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(throw 'return nil))
      (if (eq group (nnmaildir--srv-curgrp nnmaildir--cur-server))
	  (setf (nnmaildir--srv-curgrp nnmaildir--cur-server) nil))
      (setq gname (nnmaildir--grp-name group)
	    pgname (nnmaildir--pgname nnmaildir--cur-server gname))
      (unintern gname (nnmaildir--srv-groups nnmaildir--cur-server))
      (setq grp-dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    grp-dir (nnmaildir--srvgrp-dir grp-dir gname))
      (if (not force) (setq grp-dir (directory-file-name grp-dir))
	(setq ls (nnmaildir--group-ls nnmaildir--cur-server pgname))
	(if (nnmaildir--param pgname 'read-only)
	    (progn (delete-directory  (nnmaildir--tmp grp-dir))
		   (nnmaildir--unlink (nnmaildir--new grp-dir))
		   (delete-directory  (nnmaildir--cur grp-dir)))
	  (nnmaildir--delete-dir-files (nnmaildir--tmp grp-dir) ls)
	  (nnmaildir--delete-dir-files (nnmaildir--new grp-dir) ls)
	  (nnmaildir--delete-dir-files (nnmaildir--cur grp-dir) ls))
	(setq dir (nnmaildir--nndir grp-dir))
	(mapcar (lambda (subdir) (nnmaildir--delete-dir-files subdir ls))
		`(,(nnmaildir--nov-dir dir) ,(nnmaildir--num-dir dir)
		  ,@(funcall ls (nnmaildir--marks-dir dir) 'full "\\`[^.]"
			     'nosort)))
	(setq dir (nnmaildir--nndir grp-dir))
	(nnmaildir--unlink (concat dir "markfile"))
	(nnmaildir--unlink (concat dir "markfile{new}"))
	(delete-directory (nnmaildir--marks-dir dir))
	(delete-directory dir)
	(setq grp-dir (directory-file-name grp-dir)
	      dir (car (file-attributes grp-dir)))
	(unless (eq (aref "/" 0) (aref dir 0))
	  (setq dir (concat (file-truename
			     (nnmaildir--srv-dir nnmaildir--cur-server))
			    dir)))
	(delete-directory dir))
      (nnmaildir--unlink grp-dir)
      t)))

(defun nnmaildir-retrieve-headers (articles &optional gname server fetch-old)
  (let ((group (nnmaildir--prepare server gname))
	srv-dir dir nlist mlist article num start stop nov nlist2 insert-nov
	deactivate-mark)
    (setq insert-nov
	  (lambda (article)
	    (setq nov (nnmaildir--update-nov nnmaildir--cur-server group
					     article))
	    (when nov
	      (nnmaildir--cache-nov group article nov)
	      (setq num (nnmaildir--art-num article))
	      (princ num nntp-server-buffer)
	      (insert "\t" (nnmaildir--nov-get-beg nov) "\t"
		      (nnmaildir--art-msgid article) "\t"
		      (nnmaildir--nov-get-mid nov) "\tXref: nnmaildir "
		      gname ":")
	      (princ num nntp-server-buffer)
	      (insert "\t" (nnmaildir--nov-get-end nov) "\n"))))
    (catch 'return
      (unless group
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (if gname (concat "No such group: " gname) "No current group"))
	(throw 'return nil))
      (nnmaildir--with-nntp-buffer
	(erase-buffer)
	(setq mlist (nnmaildir--grp-mlist group)
	      nlist (nnmaildir--grp-nlist group)
	      gname (nnmaildir--grp-name group)
	      srv-dir (nnmaildir--srv-dir nnmaildir--cur-server)
	      dir (nnmaildir--srvgrp-dir srv-dir gname))
	(cond
	 ((null nlist))
	 ((and fetch-old (not (numberp fetch-old)))
	  (nnmaildir--nlist-iterate nlist 'all insert-nov))
	 ((null articles))
	 ((stringp (car articles))
	  (mapcar
	   (lambda (msgid)
	     (setq article (nnmaildir--mlist-art mlist msgid))
	     (if article (funcall insert-nov article)))
	   articles))
	 (t
	  (if fetch-old
	      ;; Assume the article range list is sorted ascending
	      (setq stop (car articles)
		    start (car (last articles))
		    stop  (if (numberp stop)  stop  (car stop))
		    start (if (numberp start) start (cdr start))
		    stop (- stop fetch-old)
		    stop (if (< stop 1) 1 stop)
		    articles (list (cons stop start))))
	  (nnmaildir--nlist-iterate nlist articles insert-nov)))
	(sort-numeric-fields 1 (point-min) (point-max))
	'nov))))

(defun nnmaildir-request-article (num-msgid &optional gname server to-buffer)
  (let ((group (nnmaildir--prepare server gname))
	(case-fold-search t)
	list article dir pgname deactivate-mark)
    (catch 'return
      (unless group
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (if gname (concat "No such group: " gname) "No current group"))
	(throw 'return nil))
      (if (numberp num-msgid)
	  (setq article (nnmaildir--nlist-art group num-msgid))
	(setq list (nnmaildir--grp-mlist group)
	      article (nnmaildir--mlist-art list num-msgid))
	(if article (setq num-msgid (nnmaildir--art-num article))
	  (catch 'found
	    (mapatoms
              (lambda (group-sym)
                (setq group (symbol-value group-sym)
                      list (nnmaildir--grp-mlist group)
                      article (nnmaildir--mlist-art list num-msgid))
                (when article
                  (setq num-msgid (nnmaildir--art-num article))
                  (throw 'found nil)))
              (nnmaildir--srv-groups nnmaildir--cur-server))))
	(unless article
	  (setf (nnmaildir--srv-error nnmaildir--cur-server) "No such article")
	  (throw 'return nil)))
      (setq gname (nnmaildir--grp-name group)
	    pgname (nnmaildir--pgname nnmaildir--cur-server gname)
	    dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    dir (nnmaildir--srvgrp-dir dir gname)
	    dir (if (nnmaildir--param pgname 'read-only)
		    (nnmaildir--new dir) (nnmaildir--cur dir))
	    nnmaildir-article-file-name
	    (concat dir
		    (nnmaildir--art-prefix article)
		    (nnmaildir--art-suffix article)))
      (unless (file-exists-p nnmaildir-article-file-name)
	(nnmaildir--expired-article group article)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Article has expired")
	(throw 'return nil))
      (save-excursion
	(set-buffer (or to-buffer nntp-server-buffer))
	(erase-buffer)
	(nnheader-insert-file-contents nnmaildir-article-file-name))
      (cons gname num-msgid))))

(defun nnmaildir-request-post (&optional server)
  (let (message-required-mail-headers)
    (funcall message-send-mail-function)))

(defun nnmaildir-request-replace-article (number gname buffer)
  (let ((group (nnmaildir--prepare nil gname))
	(coding-system-for-write nnheader-file-coding-system)
	(output-coding-system nnheader-file-coding-system)
	(buffer-file-coding-system nil)
	(file-coding-system nil)
	(file-coding-system-alist nil)
	dir file article suffix tmpfile deactivate-mark)
    (catch 'return
      (unless group
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(throw 'return nil))
      (when (nnmaildir--param (nnmaildir--pgname nnmaildir--cur-server gname)
			      'read-only)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Read-only group: " group))
	(throw 'return nil))
      (setq dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    dir (nnmaildir--srvgrp-dir dir gname)
	    article (nnmaildir--nlist-art group number))
      (unless article
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such article: " (number-to-string number)))
	(throw 'return nil))
      (setq suffix (nnmaildir--art-suffix article)
	    file (nnmaildir--art-prefix article)
	    tmpfile (concat (nnmaildir--tmp dir) file))
      (when (file-exists-p tmpfile)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "File exists: " tmpfile))
	(throw 'return nil))
      (save-excursion
	(set-buffer buffer)
	(write-region (point-min) (point-max) tmpfile nil 'no-message nil
		      'excl))
      (unix-sync) ;; no fsync :(
      (rename-file tmpfile (concat (nnmaildir--cur dir) file suffix) 'replace)
      t)))

(defun nnmaildir-request-move-article (article gname server accept-form
					       &optional last)
  (let ((group (nnmaildir--prepare server gname))
	pgname suffix result nnmaildir--file deactivate-mark)
    (catch 'return
      (unless group
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(throw 'return nil))
      (setq gname (nnmaildir--grp-name group)
	    pgname (nnmaildir--pgname nnmaildir--cur-server gname)
	    article (nnmaildir--nlist-art group article))
      (unless article
	(setf (nnmaildir--srv-error nnmaildir--cur-server) "No such article")
	(throw 'return nil))
      (setq suffix (nnmaildir--art-suffix article)
	    nnmaildir--file (nnmaildir--srv-dir nnmaildir--cur-server)
	    nnmaildir--file (nnmaildir--srvgrp-dir nnmaildir--file gname)
	    nnmaildir--file (if (nnmaildir--param pgname 'read-only)
				(nnmaildir--new nnmaildir--file)
			      (nnmaildir--cur nnmaildir--file))
	    nnmaildir--file (concat nnmaildir--file
				    (nnmaildir--art-prefix article)
				    suffix))
      (unless (file-exists-p nnmaildir--file)
	(nnmaildir--expired-article group article)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      "Article has expired")
	(throw 'return nil))
      (nnmaildir--with-move-buffer
	(erase-buffer)
	(nnheader-insert-file-contents nnmaildir--file)
	(setq result (eval accept-form)))
      (unless (or (null result) (nnmaildir--param pgname 'read-only))
	(nnmaildir--unlink nnmaildir--file)
	(nnmaildir--expired-article group article))
      result)))

(defun nnmaildir-request-accept-article (gname &optional server last)
  (let ((group (nnmaildir--prepare server gname))
	(coding-system-for-write nnheader-file-coding-system)
	(output-coding-system nnheader-file-coding-system)
	(buffer-file-coding-system nil)
	(file-coding-system nil)
	(file-coding-system-alist nil)
	srv-dir dir file tmpfile curfile 24h article)
    (catch 'return
      (unless group
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(throw 'return nil))
      (setq gname (nnmaildir--grp-name group))
      (when (nnmaildir--param (nnmaildir--pgname nnmaildir--cur-server gname)
			      'read-only)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Read-only group: " gname))
	(throw 'return nil))
      (setq srv-dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    dir (nnmaildir--srvgrp-dir srv-dir gname)
	    file (format-time-string "%s" nil))
      (unless (string-equal nnmaildir--delivery-time file)
	(setq nnmaildir--delivery-time file
	      nnmaildir--delivery-ct 0))
      (setq file (concat file "." nnmaildir--delivery-pid))
      (unless (zerop nnmaildir--delivery-ct)
	(setq file (concat file "_"
			   (number-to-string nnmaildir--delivery-ct))))
      (setq file (concat file "." (system-name))
	    tmpfile (concat (nnmaildir--tmp dir) file)
	    curfile (concat (nnmaildir--cur dir) file ":2,"))
      (when (file-exists-p tmpfile)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "File exists: " tmpfile))
	(throw 'return nil))
      (when (file-exists-p curfile)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "File exists: " curfile))
	(throw 'return nil))
      (setq nnmaildir--delivery-ct (1+ nnmaildir--delivery-ct)
	    24h (run-with-timer 86400 nil
				(lambda ()
				  (nnmaildir--unlink tmpfile)
				  (setf (nnmaildir--srv-error
					  nnmaildir--cur-server)
					"24-hour timer expired")
				  (throw 'return nil))))
      (condition-case nil
	  (add-name-to-file nnmaildir--file tmpfile)
	(error
	 (write-region (point-min) (point-max) tmpfile nil 'no-message nil
		       'excl)
	 (unix-sync))) ;; no fsync :(
      (cancel-timer 24h)
      (condition-case err
	  (add-name-to-file tmpfile curfile)
	(error
	 (setf (nnmaildir--srv-error nnmaildir--cur-server)
	       (concat "Error linking: " (prin1-to-string err)))
	 (nnmaildir--unlink tmpfile)
	 (throw 'return nil)))
      (nnmaildir--unlink tmpfile)
      (setq article (make-nnmaildir--art :prefix file :suffix ":2,"))
      (if (nnmaildir--grp-add-art nnmaildir--cur-server group article)
	  (cons gname (nnmaildir--art-num article))))))

(defun nnmaildir-save-mail (group-art)
  (catch 'return
    (unless group-art
      (throw 'return nil))
    (let (ga gname x groups nnmaildir--file deactivate-mark)
      (save-excursion
	(goto-char (point-min))
	(save-match-data
	  (while (looking-at "From ")
	    (replace-match "X-From-Line: ")
	    (forward-line 1))))
      (setq groups (nnmaildir--srv-groups nnmaildir--cur-server)
	    ga (car group-art) group-art (cdr group-art)
	    gname (car ga))
      (or (intern-soft gname groups)
	  (nnmaildir-request-create-group gname)
	  (throw 'return nil)) ;; not that nnmail bothers to check :(
      (unless (nnmaildir-request-accept-article gname)
	(throw 'return nil))
      (setq nnmaildir--file (nnmaildir--srv-dir nnmaildir--cur-server)
	    nnmaildir--file (nnmaildir--srvgrp-dir nnmaildir--file gname)
	    x (nnmaildir--prepare nil gname)
	    x (nnmaildir--grp-nlist x)
	    x (cdar x)
	    nnmaildir--file (concat nnmaildir--file
				    (nnmaildir--art-prefix x)
				    (nnmaildir--art-suffix x)))
      (delq nil
	    (mapcar
	     (lambda (ga)
	       (setq gname (car ga))
	       (and (or (intern-soft gname groups)
			(nnmaildir-request-create-group gname))
		    (nnmaildir-request-accept-article gname)
		    ga))
	     group-art)))))

(defun nnmaildir-active-number (gname)
  0)

(defun nnmaildir-request-expire-articles (ranges &optional gname server force)
  (let ((no-force (not force))
	(group (nnmaildir--prepare server gname))
	pgname time boundary bound-iter high low target dir nlist nlist2
	stop article didnt nnmaildir--file nnmaildir-article-file-name
	deactivate-mark)
    (catch 'return
      (unless group
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (if gname (concat "No such group: " gname) "No current group"))
	(throw 'return (gnus-uncompress-range ranges)))
      (setq gname (nnmaildir--grp-name group)
	    pgname (nnmaildir--pgname nnmaildir--cur-server gname))
      (if (nnmaildir--param pgname 'read-only)
	  (throw 'return (gnus-uncompress-range ranges)))
      (setq time (nnmaildir--param pgname 'expire-age))
      (unless time
	(setq time (or (and nnmail-expiry-wait-function
			    (funcall nnmail-expiry-wait-function gname))
		       nnmail-expiry-wait))
	(if (eq time 'immediate)
	    (setq time 0)
	  (if (numberp time)
	      (setq time (* time 86400)))))
      (when no-force
	(unless (integerp time) ;; handle 'never
	  (throw 'return (gnus-uncompress-range ranges)))
	(setq boundary (current-time)
	      high (- (car boundary) (/ time 65536))
	      low (- (cadr boundary) (% time 65536)))
	(if (< low 0)
	    (setq low (+ low 65536)
		  high (1- high)))
	(setcar (cdr boundary) low)
	(setcar boundary high))
      (setq dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    dir (nnmaildir--srvgrp-dir dir gname)
	    dir (nnmaildir--cur dir)
	    nlist (nnmaildir--grp-nlist group)
	    ranges (reverse ranges))
      (nnmaildir--with-move-buffer
	(nnmaildir--nlist-iterate
	 nlist ranges
	 (lambda (article)
	   (setq nnmaildir--file (nnmaildir--art-prefix article)
		 nnmaildir--file (concat dir nnmaildir--file
					 (nnmaildir--art-suffix article))
		 time (file-attributes nnmaildir--file))
	   (cond
	    ((null time)
	     (nnmaildir--expired-article group article))
	    ((and no-force
		  (progn
		    (setq time (nth 5 time)
			  bound-iter boundary)
		    (while (and bound-iter time
				(= (car bound-iter) (car time)))
		      (setq bound-iter (cdr bound-iter)
			    time (cdr time)))
		    (and bound-iter time
			 (car-less-than-car bound-iter time))))
	     (setq didnt (cons (nnmaildir--art-num article) didnt)))
	    (t
	     (setq nnmaildir-article-file-name nnmaildir--file
		   target (if force nil
			    (save-excursion
			      (save-restriction
				(nnmaildir--param pgname 'expire-group)))))
	     (when (and (stringp target)
			(not (string-equal target pgname))) ;; Move it.
	       (erase-buffer)
	       (nnheader-insert-file-contents nnmaildir--file)
	       (gnus-request-accept-article target nil nil 'no-encode))
	     (if (equal target pgname)
		 ;; Leave it here.
		 (setq didnt (cons (nnmaildir--art-num article) didnt))
	       (nnmaildir--unlink nnmaildir--file)
	       (nnmaildir--expired-article group article))))))
	(erase-buffer))
      didnt)))

(defun nnmaildir-request-set-mark (gname actions &optional server)
  (let ((group (nnmaildir--prepare server gname))
	(coding-system-for-write nnheader-file-coding-system)
	(output-coding-system nnheader-file-coding-system)
	(buffer-file-coding-system nil)
	(file-coding-system nil)
	(file-coding-system-alist nil)
	del-mark del-action add-action set-action marksdir markfile nlist
	ranges begin end article all-marks todo-marks did-marks mdir mfile
	pgname ls markfilenew deactivate-mark)
    (setq del-mark
	  (lambda (mark)
	    (setq mfile (nnmaildir--subdir marksdir (symbol-name mark))
		  mfile (concat mfile (nnmaildir--art-prefix article)))
	    (nnmaildir--unlink mfile))
	  del-action (lambda (article) (mapcar del-mark todo-marks))
	  add-action
	  (lambda (article)
	    (mapcar
	     (lambda (mark)
	       (setq mdir (nnmaildir--subdir marksdir (symbol-name mark))
		     mfile (concat mdir (nnmaildir--art-prefix article)))
	       (unless (memq mark did-marks)
		 (nnmaildir--mkdir mdir)
		 (setq did-marks (cons mark did-marks)))
	       (unless (file-exists-p mfile)
		 (condition-case nil
		     (add-name-to-file markfile mfile)
		   (file-error
		    (unless (file-exists-p mfile)
		      ;; too many links, maybe
		      (write-region "" nil markfilenew nil 'no-message)
		      (add-name-to-file markfilenew mfile
					'ok-if-already-exists)
		      (rename-file markfilenew markfile 'replace))))))
	     todo-marks))
	  set-action (lambda (article)
		       (funcall add-action)
		       (mapcar (lambda (mark)
				 (unless (memq mark todo-marks)
				   (funcall del-mark mark)))
			       all-marks)))
    (catch 'return
      (unless group
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such group: " gname))
	(mapcar (lambda (action)
		  (setq ranges (gnus-range-add ranges (car action))))
		actions)
	(throw 'return ranges))
      (setq nlist (nnmaildir--grp-nlist group)
	    marksdir (nnmaildir--srv-dir nnmaildir--cur-server)
	    marksdir (nnmaildir--srvgrp-dir marksdir gname)
	    marksdir (nnmaildir--nndir marksdir)
	    markfile (concat marksdir "markfile")
	    markfilenew (concat markfile "{new}")
	    marksdir (nnmaildir--marks-dir marksdir)
	    gname (nnmaildir--grp-name group)
            pgname (nnmaildir--pgname nnmaildir--cur-server gname)
            ls (nnmaildir--group-ls nnmaildir--cur-server pgname)
	    all-marks (funcall ls marksdir nil "\\`[^.]" 'nosort)
	    all-marks (mapcar 'intern all-marks))
      (mapcar
       (lambda (action)
	 (setq ranges (car action)
	       todo-marks (caddr action))
	 (mapcar (lambda (mark) (add-to-list 'all-marks mark)) todo-marks)
	 (if (numberp (cdr ranges)) (setq ranges (list ranges)))
	 (nnmaildir--nlist-iterate nlist ranges
				   (cond ((eq 'del (cadr action)) del-action)
					 ((eq 'add (cadr action)) add-action)
					 (t set-action))))
       actions)
      nil)))

(defun nnmaildir-close-group (gname &optional server)
  (let ((group (nnmaildir--prepare server gname))
	pgname ls dir msgdir files flist dirs)
    (if (null group)
	(progn
	  (setf (nnmaildir--srv-error nnmaildir--cur-server)
		(concat "No such group: " gname))
	  nil)
      (setq pgname (nnmaildir--pgname nnmaildir--cur-server gname)
	    ls (nnmaildir--group-ls nnmaildir--cur-server pgname)
	    dir (nnmaildir--srv-dir nnmaildir--cur-server)
	    dir (nnmaildir--srvgrp-dir dir gname)
	    msgdir (if (nnmaildir--param pgname 'read-only)
		       (nnmaildir--new dir) (nnmaildir--cur dir))
	    dir (nnmaildir--nndir dir)
	    dirs (cons (nnmaildir--nov-dir dir)
		       (funcall ls (nnmaildir--marks-dir dir) 'full "\\`[^.]"
				'nosort))
	    dirs (mapcar
		  (lambda (dir)
		    (cons dir (funcall ls dir nil "\\`[^.]" 'nosort)))
		  dirs)
	    files (funcall ls msgdir nil "\\`[^.]" 'nosort)
	    flist (nnmaildir--up2-1 (length files))
	    flist (make-vector flist 0))
      (save-match-data
	(mapcar
	 (lambda (file)
	   (string-match "\\`\\([^:]*\\)\\(:.*\\)?\\'" file)
	   (intern (match-string 1 file) flist))
	 files))
      (mapcar
       (lambda (dir)
	 (setq files (cdr dir)
	       dir (file-name-as-directory (car dir)))
	 (mapcar
	  (lambda (file)
	    (unless (intern-soft file flist)
	      (setq file (concat dir file))
	      (delete-file file)))
	  files))
       dirs)
      t)))

(defun nnmaildir-close-server (&optional server)
  (let (flist ls dirs dir files file x)
    (nnmaildir--prepare server nil)
    (when nnmaildir--cur-server
      (setq server nnmaildir--cur-server
	    nnmaildir--cur-server nil)
      (unintern (nnmaildir--srv-address server) nnmaildir--servers)))
  t)

(defun nnmaildir-request-close ()
  (let (servers buffer)
    (mapatoms (lambda (server)
		(setq servers (cons (symbol-name server) servers)))
	      nnmaildir--servers)
    (mapcar 'nnmaildir-close-server servers)
    (setq buffer (get-buffer " *nnmaildir work*"))
    (if buffer (kill-buffer buffer))
    (setq buffer (get-buffer " *nnmaildir nov*"))
    (if buffer (kill-buffer buffer))
    (setq buffer (get-buffer " *nnmaildir move*"))
    (if buffer (kill-buffer buffer)))
  t)

(provide 'nnmaildir)

;; Local Variables:
;; indent-tabs-mode: t
;; fill-column: 77
;; End:

;;; nnmaildir.el ends here

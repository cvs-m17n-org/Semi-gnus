;;; nnshimbun.el --- interfacing with web newspapers -*- coding: junet; -*-

;; Authors: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;          Akihiro Arisawa    <ari@atesoft.advantest.co.jp>
;; Keywords: news

;;; Copyright:

;; This file is a part of Semi-Gnus.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Gnus backend to read newspapers on WEB.


;;; Defintinos:

(gnus-declare-backend "nnshimbun" 'address)

(eval-when-compile (require 'cl))
(eval-when-compile (require 'gnus-clfns))
(eval-when-compile (require 'static))

(require 'nnheader)
(require 'nnmail)
(require 'nnoo)
(require 'gnus-bcklg)
(eval-when-compile (ignore-errors (require 'nnweb)))
;; Report failure to find w3 at load time if appropriate.
(eval '(require 'nnweb))
(require 'mcharset)


(nnoo-declare nnshimbun)

(defvar nnshimbun-check-interval 300)

(defconst nnshimbun-mew-groups
  '(("meadow-develop" "meadow-develop" nil t)
    ("meadow-users-jp" "meadow-users-jp")
    ("mule-win32" "mule-win32")
    ("mew-win32" "mew-win32")
    ("mew-dist" "mew-dist/3300" t)
    ("mgp-users-jp" "mgp-users-jp/A" t t)))

(defvar nnshimbun-type-definition
  `(("asahi"
     (url . "http://spin.asahi.com/")
     (groups "national" "business" "politics" "international" "sports" "personal" "feneral")
     (coding-system  . ,(static-if (boundp 'MULE) '*sjis* 'shift_jis))
     (generate-nov   . nnshimbun-generate-nov-for-each-group)
     (get-headers    . nnshimbun-asahi-get-headers)
     (index-url      . (format "%sp%s.html" nnshimbun-url nnshimbun-current-group))
     (from-address   . "webmaster@www.asahi.com")
     (make-contents  . nnshimbun-make-text-or-html-contents)
     (contents-start . "\n<!-- Start of kiji -->\n")
     (contents-end   . "\n<!-- End of kiji -->\n"))
    ("sponichi"
     (url . "http://www.sponichi.co.jp/")
     (groups "baseball" "soccer" "usa" "others" "society" "entertainment" "horseracing")
     (coding-system  . ,(static-if (boundp 'MULE) '*sjis* 'shift_jis))
     (generate-nov   . nnshimbun-generate-nov-for-each-group)
     (get-headers    . nnshimbun-sponichi-get-headers)
     (index-url      . (format "%s%s/index.html" nnshimbun-url nnshimbun-current-group))
     (from-address   . "webmaster@www.sponichi.co.jp")
     (make-contents  . nnshimbun-make-text-or-html-contents)
     (contents-start . "\n<span class=\"text\">　")
     (contents-end   . "\n"))
    ("cnet"
     (url . "http://cnet.sphere.ne.jp/")
     (groups "comp")
     (coding-system  . ,(static-if (boundp 'MULE) '*sjis* 'shift_jis))
     (generate-nov   . nnshimbun-generate-nov-for-each-group)
     (get-headers    . nnshimbun-cnet-get-headers)
     (index-url      . (format "%s/News/Oneweek/" nnshimbun-url))
     (from-address   . "cnet@sphere.ad.jp")
     (make-contents  . nnshimbun-make-html-contents)
     (contents-start . "\n<!--KIJI-->\n")
     (contents-end   . "\n<!--/KIJI-->\n"))
    ("wired"
     (url . "http://www.hotwired.co.jp/")
     (groups "business" "culture" "technology")
     (coding-system  . ,(static-if (boundp 'MULE) '*euc-japan* 'euc-jp))
     (generate-nov   . nnshimbun-generate-nov-for-all-groups)
     (get-headers    . nnshimbun-wired-get-all-headers)
     (index-url)
     (from-address   . "webmaster@www.hotwired.co.jp")
     (make-contents  . nnshimbun-make-html-contents)
     (contents-start . "\n<!-- START_OF_BODY -->\n")
     (contents-end   . "\n<!-- END_OF_BODY -->\n"))
    ("yomiuri"
     (url . "http://www.yomiuri.co.jp/")
     (groups "shakai" "sports" "seiji" "keizai" "kokusai" "fuho")
     (coding-system  . ,(static-if (boundp 'MULE) '*sjis* 'shift_jis))
     (generate-nov   . nnshimbun-generate-nov-for-all-groups)
     (get-headers    . nnshimbun-yomiuri-get-all-headers)
     (index-url      . (concat nnshimbun-url "main.htm"))
     (from-address   . "webmaster@www.yomiuri.co.jp")
     (make-contents  . nnshimbun-make-text-or-html-contents)
     (contents-start . "\n<!--  honbun start  -->\n")
     (contents-end   . "\n<!--  honbun end  -->\n"))
    ("zdnet"
     (url . "http://www.zdnet.co.jp/news/")
     (groups "comp")
     (coding-system  . ,(static-if (boundp 'MULE) '*sjis* 'shift_jis))
     (generate-nov   . nnshimbun-generate-nov-for-each-group)
     (get-headers    . nnshimbun-zdnet-get-headers)
     (index-url      . nnshimbun-url)
     (from-address   . "zdnn@softbank.co.jp")
     (make-contents  . nnshimbun-make-html-contents)
     (contents-start . "\\(<!--BODY-->\\|<!--DATE-->\\)")
     (contents-end   . "\\(<!--BODYEND-->\\|<!--BYLINEEND-->\\)"))
    ("mew"
     (url . "http://www.mew.org/archive/")
     (groups ,@(mapcar #'car nnshimbun-mew-groups))
     (coding-system . ,(static-if (boundp 'MULE) '*iso-2022-jp* 'iso-2022-jp))
     (generate-nov  . nnshimbun-generate-nov-for-each-group)
     (get-headers   . nnshimbun-mew-get-headers)
     (index-url     . (nnshimbun-mew-concat-url "index.html"))
     (make-contents . nnshimbun-make-mhonarc-contents))
    ("xemacs"
     (url . "http://list-archives.xemacs.org/")
     (groups "xemacs-announce" "xemacs-beta-ja" "xemacs-beta"
	     "xemacs-build-reports" "xemacs-cvs" "xemacs-mule"
	     "xemacs-nt" "xemacs-patches" "xemacs-users-ja" "xemacs")
     (coding-system . ,(static-if (boundp 'MULE) '*euc-japan* 'euc-jp))
     (generate-nov  . nnshimbun-generate-nov-for-each-group)
     (get-headers   . nnshimbun-xemacs-get-headers)
     (index-url     . (nnshimbun-xemacs-concat-url nil))
     (make-contents . nnshimbun-make-mhonarc-contents))
    ("netbsd"
     (url . "http://www.jp.netbsd.org/ja/JP/ml/")
     (groups "announce-ja" "junk-ja" "tech-misc-ja" "tech-pkg-ja"
	     "port-arm32-ja" "port-hpcmips-ja" "port-mac68k-ja"
	     "port-mips-ja" "port-powerpc-ja" "hpcmips-changes-ja"
	     "members-ja" "admin-ja" "www-changes-ja")
     (coding-system  . ,(static-if (boundp 'MULE) '*iso-2022-jp* 'iso-2022-jp))
     (generate-nov   . nnshimbun-generate-nov-for-each-group)
     (get-headers    . nnshimbun-netbsd-get-headers)
     (index-url      . (format "%s%s/index.html" nnshimbun-url nnshimbun-current-group))
     (make-contents  . nnshimbun-make-mhonarc-contents))
    ("bbdb-ml"
     (url . "http://www.rc.tutrp.tut.ac.jp/bbdb-ml/")
     (groups "bbdb-ml")
     (coding-system . ,(static-if (boundp 'MULE) '*iso-2022-jp* 'iso-2022-jp))
     (generate-nov . nnshimbun-generate-nov-for-each-group)
     (get-headers . nnshimbun-fml-get-headers)
     (index-url . nnshimbun-url)
     (make-contents . nnshimbun-make-fml-contents))
    ))

(defvar nnshimbun-x-face-alist
  '(("default" .
     (("default" .
       "X-Face: Ygq$6P.,%Xt$U)DS)cRY@k$VkW!7(X'X'?U{{osjjFG\"E]hND;SPJ-J?O?R|a?L
	g2$0rVng=O3Lt}?~IId8Jj&vP^3*o=LKUyk(`t%0c!;t6REk=JbpsEn9MrN7gZ%"))))
  "Alist of server vs. alist of group vs. X-Face field.  It looks like:

\((\"asahi\" . ((\"national\" . \"X-face: ***\")
	     (\"business\" . \"X-Face: ***\")
		;;
		;;
	     (\"default\" . \"X-face: ***\")))
 (\"sponichi\" . ((\"baseball\" . \"X-face: ***\")
		(\"soccer\" . \"X-Face: ***\")
		;;
		;;
		(\"default\" . \"X-face: ***\")))
		;;
 (\"default\" . ((\"default\" . \"X-face: ***\")))")

(defvoo nnshimbun-directory (nnheader-concat gnus-directory "shimbun/")
  "Where nnshimbun will save its files.")

(defvoo nnshimbun-nov-is-evil nil
  "*Non-nil means that nnshimbun will never retrieve NOV headers.")

(defvoo nnshimbun-nov-file-name ".overview")

(defvoo nnshimbun-pre-fetch-article nil
  "*Non nil means that nnshimbun fetch unread articles when scanning groups.")

;; set by nnshimbun-possibly-change-group
(defvoo nnshimbun-buffer nil)
(defvoo nnshimbun-current-directory nil)
(defvoo nnshimbun-current-group nil)

;; set by nnshimbun-open-server
(defvoo nnshimbun-url nil)
(defvoo nnshimbun-coding-system nil)
(defvoo nnshimbun-groups nil)
(defvoo nnshimbun-generate-nov nil)
(defvoo nnshimbun-get-headers nil)
(defvoo nnshimbun-index-url nil)
(defvoo nnshimbun-from-address nil)
(defvoo nnshimbun-make-contents nil)
(defvoo nnshimbun-contents-start nil)
(defvoo nnshimbun-contents-end nil)
(defvoo nnshimbun-server-directory nil)

(defvoo nnshimbun-status-string "")
(defvoo nnshimbun-nov-last-check nil)
(defvoo nnshimbun-nov-buffer-alist nil)
(defvoo nnshimbun-nov-buffer-file-name nil)

(defvoo nnshimbun-keep-backlog 300)
(defvoo nnshimbun-backlog-articles nil)
(defvoo nnshimbun-backlog-hashtb nil)

(defconst nnshimbun-meta-content-type-charset-regexp
  (eval-when-compile
    (concat "<meta[ \t]+http-equiv=\"?Content-type\"?[ \t]+content=\"\\([^;]+\\)"
	    ";[ \t]*charset=\"?\\([^\"]+\\)\"?"
	    ">"))
  "Regexp used in parsing `<META HTTP-EQUIV=\"Content-Type\" content=\"...;charset=...\">
for a charset indication")

(defconst nnshimbun-meta-charset-content-type-regexp
  (eval-when-compile
    (concat "<meta[ \t]+content=\"\\([^;]+\\)"
	    ";[ \t]*charset=\"?\\([^\"]+\\)\"?"
	    "[ \t]+http-equiv=\"?Content-type\"?>"))
  "Regexp used in parsing `<META content=\"...;charset=...\" HTTP-EQUIV=\"Content-Type\">
for a charset indication")



;;; backlog
(defmacro nnshimbun-backlog (&rest form)
  `(let ((gnus-keep-backlog nnshimbun-keep-backlog)
	 (gnus-backlog-buffer (format " *nnshimbun backlog %s*" (nnoo-current-server 'nnshimbun)))
	 (gnus-backlog-articles nnshimbun-backlog-articles)
	 (gnus-backlog-hashtb nnshimbun-backlog-hashtb))
     (unwind-protect
	 (progn ,@form)
       (setq nnshimbun-backlog-articles gnus-backlog-articles
	     nnshimbun-backlog-hashtb gnus-backlog-hashtb))))
(put 'nnshimbun-backlog 'lisp-indent-function 0)
(put 'nnshimbun-backlog 'edebug-form-spec '(form body))



;;; Interface Functions
(nnoo-define-basics nnshimbun)

(deffoo nnshimbun-open-server (server &optional defs)
  ;; Set default values.
  (dolist (default (cdr (assoc server nnshimbun-type-definition)))
    (let ((symbol (intern (concat "nnshimbun-" (symbol-name (car default))))))
      (unless (assq symbol defs)
	(push (list symbol (cdr default)) defs))))
  ;; Set directory for server working files.
  (push (list 'nnshimbun-server-directory
	      (file-name-as-directory
	       (expand-file-name server nnshimbun-directory)))
	defs)
  (nnoo-change-server 'nnshimbun server defs)
  (nnshimbun-possibly-change-group nil server)
  ;; Make directories.
  (unless (file-exists-p nnshimbun-directory)
    (ignore-errors (make-directory nnshimbun-directory t)))
  (cond
   ((not (file-exists-p nnshimbun-directory))
    (nnshimbun-close-server)
    (nnheader-report 'nnshimbun "Couldn't create directory: %s" nnshimbun-directory))
   ((not (file-directory-p (file-truename nnshimbun-directory)))
    (nnshimbun-close-server)
    (nnheader-report 'nnshimbun "Not a directory: %s" nnshimbun-directory))
   (t
    (unless (file-exists-p nnshimbun-server-directory)
      (ignore-errors (make-directory nnshimbun-server-directory t)))
    (cond
     ((not (file-exists-p nnshimbun-server-directory))
      (nnshimbun-close-server)
      (nnheader-report 'nnshimbun "Couldn't create directory: %s" nnshimbun-server-directory))
     ((not (file-directory-p (file-truename nnshimbun-server-directory)))
      (nnshimbun-close-server)
      (nnheader-report 'nnshimbun "Not a directory: %s" nnshimbun-server-directory))
     (t
      (nnheader-report 'nnshimbun "Opened server %s using directory %s"
		       server nnshimbun-server-directory)
      t)))))

(deffoo nnshimbun-close-server (&optional server)
  (and (nnshimbun-server-opened server)
       (gnus-buffer-live-p nnshimbun-buffer)
       (kill-buffer nnshimbun-buffer))
  (nnshimbun-backlog (gnus-backlog-shutdown))
  (nnshimbun-save-nov)
  (nnoo-close-server 'nnshimbun server)
  t)

(static-when (boundp 'MULE)
  (unless (coding-system-p 'euc-japan)
    (copy-coding-system '*euc-japan* 'euc-japan))
  (unless (coding-system-p 'shift_jis)
    (copy-coding-system '*sjis* 'shift_jis))
  (eval-and-compile
    (defalias-maybe 'coding-system-category 'get-code-mnemonic)))

(defun nnshimbun-retrieve-url (url &optional no-cache)
  "Rertrieve URL contents and insert to current buffer."
  (let ((buf (current-buffer))
	(url-working-buffer url-working-buffer))
    (let ((old-asynch (default-value 'url-be-asynchronous))
	  (old-caching (default-value 'url-automatic-caching))
	  (old-mode (default-value 'url-standalone-mode)))
      (setq-default url-be-asynchronous nil)
      (when no-cache
	(setq-default url-automatic-caching nil)
	(setq-default url-standalone-mode nil))
      (unwind-protect
	  (let ((coding-system-for-read 'binary)
		(coding-system-for-write 'binary)
		(input-coding-system 'binary)
		(output-coding-system 'binary)
		(default-enable-multibyte-characters nil))
	    (set-buffer
	     (setq url-working-buffer
		   (cdr (url-retrieve url no-cache))))
	    (url-uncompress))
	(setq-default url-be-asynchronous old-asynch)
	(setq-default url-automatic-caching old-caching)
	(setq-default url-standalone-mode old-mode)))
    (let ((charset
	   (or url-current-mime-charset
	       (let ((case-fold-search t))
		 (goto-char (point-min))
		 (if (or (re-search-forward
			  nnshimbun-meta-content-type-charset-regexp nil t)
			 (re-search-forward
			  nnshimbun-meta-charset-content-type-regexp nil t))
		     (buffer-substring-no-properties (match-beginning 2)
						     (match-end 2)))))))
      (decode-coding-region
       (point-min) (point-max)
       (if charset
	   (let ((mime-charset-coding-system-alist
		  (append '((euc-jp . euc-japan)
			    (shift-jis . shift_jis)
			    (shift_jis . shift_jis)
			    (sjis . shift_jis)
			    (x-euc-jp . euc-japan)
			    (x-shift-jis . shift_jis)
			    (x-shift_jis . shift_jis)
			    (x-sjis . shift_jis))
			  mime-charset-coding-system-alist)))
	     (mime-charset-to-coding-system charset))
	 (let ((default (condition-case nil
			    (coding-system-category nnshimbun-coding-system)
			  (error nil)))
	       (candidate (detect-coding-region (point-min) (point-max))))
	   (unless (listp candidate)
	     (setq candidate (list candidate)))
	   (catch 'coding
	     (dolist (coding candidate)
	       (if (eq default (coding-system-category coding))
		   (throw 'coding coding)))
	     (if (eq (coding-system-category 'binary)
		     (coding-system-category (car candidate)))
		 nnshimbun-coding-system
	       (car candidate)))))))
    (set-buffer-multibyte t)
    (set-buffer buf)
    (insert-buffer url-working-buffer)
    (kill-buffer url-working-buffer)))

(deffoo nnshimbun-request-article (article &optional group server to-buffer)
  (when (nnshimbun-possibly-change-group group server)
    (if (stringp article)
	(setq article (nnshimbun-search-id group article)))
    (if (integerp article)
	(nnshimbun-request-article-1 article group server to-buffer)
      (nnheader-report 'nnml "Couldn't retrieve article: %s" (prin1-to-string article))
      nil)))

(defsubst nnshimbun-header-xref (x)
  (if (and (setq x (mail-header-xref x))
	   (string-match "^Xref: " x))
      (substring x 6)
    x))

(defun nnshimbun-request-article-1 (article &optional group server to-buffer)
  (if (nnshimbun-backlog
	(gnus-backlog-request-article
	 group article (or to-buffer nntp-server-buffer)))
      (cons group article)
    (let (header contents)
      (when (setq header (save-excursion
			   (set-buffer (nnshimbun-open-nov group))
			   (and (nnheader-find-nov-line article)
				(nnheader-parse-nov))))
	(let* ((xref (nnshimbun-header-xref header))
	       (x-faces (cdr (or (assoc (or server
					    (nnoo-current-server 'nnshimbun))
					nnshimbun-x-face-alist)
				 (assoc "default" nnshimbun-x-face-alist))))
	       (x-face (cdr (or (assoc group x-faces)
				(assoc "default" x-faces)))))
	  (save-excursion
	    (set-buffer nnshimbun-buffer)
	    (erase-buffer)
	    (nnshimbun-retrieve-url xref)
	    (nnheader-message 6 "nnshimbun: Make contents...")
	    (goto-char (point-min))
	    (setq contents (funcall nnshimbun-make-contents header x-face))
	    (nnheader-message 6 "nnshimbun: Make contents...done"))))
      (when contents
	(save-excursion
	  (set-buffer (or to-buffer nntp-server-buffer))
	  (erase-buffer)
	  (insert contents)
	  (nnshimbun-backlog
	    (gnus-backlog-enter-article group article (current-buffer)))
	  (nnheader-report 'nnshimbun "Article %s retrieved" (mail-header-id header))
	  (cons group (mail-header-number header)))))))

(deffoo nnshimbun-request-group (group &optional server dont-check)
  (let ((pathname-coding-system 'binary))
    (cond
     ((not (nnshimbun-possibly-change-group group server))
      (nnheader-report 'nnshimbun "Invalid group (no such directory)"))
     ((not (file-exists-p nnshimbun-current-directory))
      (nnheader-report 'nnshimbun "Directory %s does not exist"
		       nnshimbun-current-directory))
     ((not (file-directory-p nnshimbun-current-directory))
      (nnheader-report 'nnshimbun "%s is not a directory" nnshimbun-current-directory))
     (dont-check
      (nnheader-report 'nnshimbun "Group %s selected" group)
      t)
     (t
      (let (beg end lines)
	(save-excursion
	  (set-buffer (nnshimbun-open-nov group))
	  (goto-char (point-min))
	  (setq beg (ignore-errors (read (current-buffer))))
	  (goto-char (point-max))
	  (forward-line -1)
	  (setq end (ignore-errors (read (current-buffer)))
		lines (count-lines (point-min) (point-max))))
	(nnheader-report 'nnshimbunw "Selected group %s" group)
	(nnheader-insert "211 %d %d %d %s\n"
			 lines (or beg 0) (or end 0) group))))))

(deffoo nnshimbun-request-scan (&optional group server)
  (nnshimbun-possibly-change-group group server)
  (nnshimbun-generate-nov-database group))

(deffoo nnshimbun-close-group (group &optional server)
  (nnshimbun-write-nov group)
  t)

(deffoo nnshimbun-request-list (&optional server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (dolist (group nnshimbun-groups)
      (when (nnshimbun-possibly-change-group group server)
	(let (beg end)
	  (save-excursion
	    (set-buffer (nnshimbun-open-nov group))
	    (goto-char (point-min))
	    (setq beg (ignore-errors (read (current-buffer))))
	    (goto-char (point-max))
	    (forward-line -1)
	    (setq end (ignore-errors (read (current-buffer)))))
	  (insert (format "%s %d %d n\n" group (or end 0) (or beg 0)))))))
  t) ; return value

(eval-and-compile
  (if (fboundp 'mime-entity-fetch-field)
      ;; For Semi-Gnus.
      (defun nnshimbun-insert-header (header)
	(insert "Subject: " (or (mime-entity-fetch-field header 'Subject) "(none)") "\n"
		"From: " (or (mime-entity-fetch-field header 'From) "(nobody)") "\n"
		"Date: " (or (mail-header-date header) "") "\n"
		"Message-ID: " (or (mail-header-id header) (nnmail-message-id)) "\n")
	(let ((refs (mail-header-references header)))
	  (and refs
	       (string< "" refs)
	       (insert "References: " refs "\n")))
	(insert "Lines: " (number-to-string (or (mail-header-lines header) 0)) "\n"
		"Xref: " (nnshimbun-header-xref header) "\n"))
    ;; For pure Gnus.
    (defun nnshimbun-insert-header (header)
      (nnheader-insert-header header)
      (delete-char -1)
      (insert "Xref: " (nnshimbun-header-xref header) "\n"))))

(deffoo nnshimbun-retrieve-headers (articles &optional group server fetch-old)
  (when (nnshimbun-possibly-change-group group server)
    (if (nnshimbun-retrieve-headers-with-nov articles fetch-old)
	'nov
      (save-excursion
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(let (header)
	  (dolist (art articles)
	    (if (stringp art)
		(setq art (nnshimbun-search-id group art)))
	    (if (integerp art)
		(when (setq header
			    (save-excursion
			      (set-buffer (nnshimbun-open-nov group))
			      (and (nnheader-find-nov-line art)
				   (nnheader-parse-nov))))
		  (insert (format "220 %d Article retrieved.\n" art))
		  (nnshimbun-insert-header header)
		  (insert ".\n")
		  (delete-region (point) (point-max))))))
	'header))))

(defun nnshimbun-retrieve-headers-with-nov (articles &optional fetch-old)
  (if (or gnus-nov-is-evil nnshimbun-nov-is-evil)
      nil
    (let ((nov (expand-file-name nnshimbun-nov-file-name nnshimbun-current-directory)))
      (when (file-exists-p nov)
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (nnheader-insert-file-contents nov)
	  (if (and fetch-old (not (numberp fetch-old)))
	      t				; Don't remove anything.
	    (nnheader-nov-delete-outside-range
	     (if fetch-old (max 1 (- (car articles) fetch-old))
	       (car articles))
	     (car (last articles)))
	    t))))))



;;; Nov Database Operations

(defun nnshimbun-generate-nov-database (group)
  (prog1 (funcall nnshimbun-generate-nov group)
    (nnshimbun-write-nov group)))

(defun nnshimbun-generate-nov-for-each-group (group)
  (nnshimbun-possibly-change-group group)
  (save-excursion
    (set-buffer (nnshimbun-open-nov group))
    (let (i)
      (goto-char (point-max))
      (forward-line -1)
      (setq i (or (ignore-errors (read (current-buffer))) 0))
      (dolist (header (save-excursion
			(set-buffer nnshimbun-buffer)
			(erase-buffer)
			(nnshimbun-retrieve-url (eval nnshimbun-index-url) t)
			(goto-char (point-min))
			(funcall nnshimbun-get-headers)))
	(unless (nnshimbun-search-id group (mail-header-id header))
	  (mail-header-set-number header (setq i (1+ i)))
	  (goto-char (point-max))
	  (nnheader-insert-nov header)
	  (if nnshimbun-pre-fetch-article
	      (nnshimbun-request-article-1 i group nil nnshimbun-buffer)))))))

(defun nnshimbun-generate-nov-for-all-groups (&rest args)
  (unless (and nnshimbun-nov-last-check
	       (< (nnshimbun-lapse-seconds nnshimbun-nov-last-check)
		  nnshimbun-check-interval))
    (save-excursion
      (dolist (list (funcall nnshimbun-get-headers))
	(let ((group (car list)))
	  (nnshimbun-possibly-change-group group)
	  (when (cdr list)
	    (set-buffer (nnshimbun-open-nov group))
	    (let (i)
	      (goto-char (point-max))
	      (forward-line -1)
	      (setq i (or (ignore-errors (read (current-buffer))) 0))
	      (dolist (header (cdr list))
		(unless (nnshimbun-search-id group (mail-header-id header))
		  (mail-header-set-number header (setq i (1+ i)))
		  (goto-char (point-max))
		  (nnheader-insert-nov header)
		  (if nnshimbun-pre-fetch-article
		      (nnshimbun-request-article-1 i group nil nnshimbun-buffer))))))))
      (nnshimbun-save-nov)
      (setq nnshimbun-nov-last-check (current-time)))))

(defun nnshimbun-search-id (group id &optional nov)
  (save-excursion
    (set-buffer (nnshimbun-open-nov group))
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
		  (search-forward id nil t)) ; We find the ID.
	;; And the id is in the fourth field.
	(if (not (and (search-backward "\t" nil t 4)
		      (not (search-backward "\t" (gnus-point-at-bol) t))))
	    (forward-line 1)
	  (forward-line 0)
	  (setq found t)))
      (unless found
	(goto-char (point-min))
	(when (search-forward (concat "X-Nnshimbun-Id: " id) nil t)
	  (forward-line 0)
	  (setq found t)))
      (if found
	  (if nov
	      (nnheader-parse-nov)
	    ;; We return the article number.
	    (ignore-errors (read (current-buffer))))))))

(defun nnshimbun-nov-fix-header (group header args)
  (save-excursion
    (set-buffer (nnshimbun-open-nov group))
    (when (nnheader-find-nov-line (mail-header-number header))
      (dolist (arg args)
	(if (eq (car arg) 'id)
	    (let ((extra (mail-header-extra header)))
	      (unless (assq 'X-Nnshimbun-Id extra)
		(mail-header-set-extra
		 header
		 (cons (cons 'X-Nnshimbun-Id (mail-header-id header))
		       extra)))
	      (mail-header-set-id header (cdr arg)))
	  (let ((func (intern (concat "mail-header-set-" (symbol-name (car arg))))))
	    (if (cdr arg) (eval (list func header (cdr arg)))))))
      (mail-header-set-xref header (nnshimbun-header-xref header))
      (delete-region (point) (progn (forward-line 1) (point)))
      (nnheader-insert-nov header))))

(defun nnshimbun-open-nov (group)
  (let ((buffer (cdr (assoc group nnshimbun-nov-buffer-alist))))
    (if (buffer-live-p buffer)
	buffer
      (setq buffer (gnus-get-buffer-create
		    (format " *nnshimbun overview %s %s*"
			    (nnoo-current-server 'nnshimbun) group)))
      (save-excursion
	(set-buffer buffer)
	(set (make-local-variable 'nnshimbun-nov-buffer-file-name)
	     (expand-file-name
	      nnshimbun-nov-file-name
	      (nnmail-group-pathname group nnshimbun-server-directory)))
	(erase-buffer)
	(when (file-exists-p nnshimbun-nov-buffer-file-name)
	  (nnheader-insert-file-contents nnshimbun-nov-buffer-file-name))
	(set-buffer-modified-p nil))
      (push (cons group buffer) nnshimbun-nov-buffer-alist)
      buffer)))

(defun nnshimbun-write-nov (group)
  (let ((buffer (cdr (assoc group nnshimbun-nov-buffer-alist))))
    (when (buffer-live-p buffer)
      (save-excursion
	(set-buffer buffer)
	(buffer-modified-p)
	(nnmail-write-region 1 (point-max) nnshimbun-nov-buffer-file-name
			     nil 'nomesg)))))

(defun nnshimbun-save-nov ()
  (save-excursion
    (while nnshimbun-nov-buffer-alist
      (when (buffer-name (cdar nnshimbun-nov-buffer-alist))
	(set-buffer (cdar nnshimbun-nov-buffer-alist))
	(when (buffer-modified-p)
	  (nnmail-write-region 1 (point-max) nnshimbun-nov-buffer-file-name
			       nil 'nomesg))
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer)))
      (setq nnshimbun-nov-buffer-alist (cdr nnshimbun-nov-buffer-alist)))))



;;; Server Initialize
(defun nnshimbun-possibly-change-group (group &optional server)
  (when server
    (unless (nnshimbun-server-opened server)
      (nnshimbun-open-server server)))
  (unless (gnus-buffer-live-p nnshimbun-buffer)
    (setq nnshimbun-buffer
	  (save-excursion
	    (nnheader-set-temp-buffer
	     (format " *nnshimbun %s*" (nnoo-current-server 'nnshimbun))))))
  (if (not group)
      t
    (let ((pathname (nnmail-group-pathname group nnshimbun-server-directory))
	  (pathname-coding-system 'binary))
      (unless (equal pathname nnshimbun-current-directory)
	(setq nnshimbun-current-directory pathname
	      nnshimbun-current-group group))
      (unless (file-exists-p nnshimbun-current-directory)
	(ignore-errors (make-directory nnshimbun-current-directory t)))
      (cond
       ((not (file-exists-p nnshimbun-current-directory))
	(nnheader-report 'nnshimbun "Couldn't create directory: %s" nnshimbun-current-directory))
       ((not (file-directory-p (file-truename nnshimbun-current-directory)))
	(nnheader-report 'nnshimbun "Not a directory: %s" nnshimbun-current-directory))
       (t t)))))



;;; Misc Functions

(eval-and-compile
  (if (fboundp 'eword-encode-string)
      ;; For Semi-Gnus.
      (defun nnshimbun-mime-encode-string (string)
	(mapconcat
	 #'identity
	 (split-string (eword-encode-string (nnweb-decode-entities-string string)) "\n")
	 ""))
    ;; For pure Gnus.
    (defun nnshimbun-mime-encode-string (string)
      (mapconcat
       #'identity
       (split-string
	(with-temp-buffer
	  (insert (nnweb-decode-entities-string string))
	  (rfc2047-encode-region (point-min) (point-max))
	  (buffer-substring (point-min) (point-max)))
	"\n")
       ""))))

(defun nnshimbun-lapse-seconds (time)
  (let ((now (current-time)))
    (+ (* (- (car now) (car time)) 65536)
       (- (nth 1 now) (nth 1 time)))))

(defun nnshimbun-make-date-string (year month day &optional time)
  (format "%02d %s %04d %s +0900"
	  day
	  (aref [nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
		     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
		month)
	  (cond ((< year 69)
		 (+ year 2000))
		((< year 100)
		 (+ year 1900))
		((< year 1000)	; possible 3-digit years.
		 (+ year 1900))
		(t year))
	  (or time "00:00")))

(if (fboundp 'regexp-opt)
    (defalias 'nnshimbun-regexp-opt 'regexp-opt)
  (defun nnshimbun-regexp-opt (strings &optional paren)
    "Return a regexp to match a string in STRINGS.
Each string should be unique in STRINGS and should not contain any regexps,
quoted or not.  If optional PAREN is non-nil, ensure that the returned regexp
is enclosed by at least one regexp grouping construct."
    (let ((open-paren (if paren "\\(" "")) (close-paren (if paren "\\)" "")))
      (concat open-paren (mapconcat 'regexp-quote strings "\\|") close-paren))))


;; Fast fill-region function

(defvar nnshimbun-fill-column (min 80 (- (frame-width) 4)))

(defconst nnshimbun-kinsoku-bol-list
  (append "!)-_~}]:;',.?、。，．・：；？！゛゜´｀¨＾￣＿ヽヾゝゞ〃\
仝々〆〇ー―‐／＼〜‖｜…‥’”）〕］｝〉》」』】°′″℃ぁぃぅぇぉ\
っゃゅょゎァィゥェォッャュョヮヵヶ" nil))

(defconst nnshimbun-kinsoku-eol-list
  (append "({[`‘“（〔［｛〈《「『【°′″§" nil))

(defun nnshimbun-fill-line ()
  (forward-line 0)
  (let ((top (point)) chr)
    (while (if (>= (move-to-column nnshimbun-fill-column)
		   nnshimbun-fill-column)
	       (not (progn
		      (if (memq (preceding-char) nnshimbun-kinsoku-eol-list)
			  (progn
			    (backward-char)
			    (while (memq (preceding-char) nnshimbun-kinsoku-eol-list)
			      (backward-char))
			    (insert "\n"))
			(while (memq (setq chr (following-char)) nnshimbun-kinsoku-bol-list)
			  (forward-char))
			(if (looking-at "\\s-+")
			    (or (eolp) (delete-region (point) (match-end 0)))
			  (or (> (char-width chr) 1)
			      (re-search-backward "\\<" top t)
			      (end-of-line)))
			(or (eolp) (insert "\n"))))))
      (setq top (point))))
  (forward-line 1)
  (not (eobp)))

(defsubst nnshimbun-shallow-rendering ()
  (goto-char (point-min))
  (while (search-forward "<p>" nil t)
    (insert "\n\n"))
  (goto-char (point-min))
  (while (search-forward "<br>" nil t)
    (insert "\n"))
  (nnweb-remove-markup)
  (nnweb-decode-entities)
  (goto-char (point-min))
  (while (nnshimbun-fill-line))
  (goto-char (point-min))
  (when (skip-chars-forward "\n")
    (delete-region (point-min) (point)))
  (while (search-forward "\n\n" nil t)
    (let ((p (point)))
      (when (skip-chars-forward "\n")
	(delete-region p (point)))))
  (goto-char (point-max))
  (when (skip-chars-backward "\n")
    (delete-region (point) (point-max)))
  (insert "\n"))

(defun nnshimbun-make-text-or-html-contents (header &optional x-face)
  (let ((case-fold-search t) (html t) (start))
    (when (and (re-search-forward nnshimbun-contents-start nil t)
	       (setq start (point))
	       (re-search-forward nnshimbun-contents-end nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start)
      (nnshimbun-shallow-rendering)
      (setq html nil))
    (goto-char (point-min))
    (nnshimbun-insert-header header)
    (insert "Content-Type: " (if html "text/html" "text/plain")
	    "; charset=ISO-2022-JP\nMIME-Version: 1.0\n")
    (when x-face
      (insert x-face)
      (unless (bolp)
	(insert "\n")))
    (insert "\n")
    (encode-coding-string (buffer-string)
			  (mime-charset-to-coding-system "ISO-2022-JP"))))

(defun nnshimbun-make-html-contents (header &optional x-face)
  (let (start)
    (when (and (re-search-forward nnshimbun-contents-start nil t)
	       (setq start (point))
	       (re-search-forward nnshimbun-contents-end nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start))
    (goto-char (point-min))
    (nnshimbun-insert-header header)
    (insert "Content-Type: text/html; charset=ISO-2022-JP\n"
	    "MIME-Version: 1.0\n")
    (when x-face
      (insert x-face)
      (unless (bolp)
	(insert "\n")))
    (insert "\n")
    (encode-coding-string (buffer-string)
			  (mime-charset-to-coding-system "ISO-2022-JP"))))

(defun nnshimbun-make-mhonarc-contents (header &rest args)
  (require 'mml)
  (if (search-forward "<!--X-Head-End-->" nil t)
      (progn
	(forward-line 0)
	;; Processing headers.
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (nnweb-decode-entities)
	  (goto-char (point-min))
	  (while (search-forward "\n<!--X-" nil t)
	    (replace-match "\n"))
	  (goto-char (point-min))
	  (while (search-forward " -->\n" nil t)
	    (replace-match "\n"))
	  (goto-char (point-min))
	  (while (search-forward "\t" nil t)
	    (replace-match " "))
	  (goto-char (point-min))
	  (let (buf refs)
	    (while (not (eobp))
	      (cond
	       ((looking-at "<!--")
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Subject: +")
		(push (cons 'subject (nnheader-header-value)) buf)
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "From: +")
		(push (cons 'from (nnheader-header-value)) buf)
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Date: +")
		(push (cons 'date (nnheader-header-value)) buf)
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Message-Id: +")
		(push (cons 'id (concat "<" (nnheader-header-value) ">")) buf)
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Reference: +")
		(push (concat "<" (nnheader-header-value) ">") refs)
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Content-Type: ")
		(unless (search-forward "charset" (gnus-point-at-eol) t)
		  (end-of-line)
		  (insert "; charset=ISO-2022-JP"))
		(forward-line 1))
	       (t (forward-line 1))))
	    (insert "MIME-Version: 1.0\n")
	    (if refs (push (cons 'references (mapconcat 'identity refs " ")) buf))
	    (nnshimbun-nov-fix-header nnshimbun-current-group header buf)
	    (goto-char (point-min))
	    (nnshimbun-insert-header header))
	  (goto-char (point-max)))
	;; Processing body.
	(save-restriction
	  (narrow-to-region (point) (point-max))
	  (delete-region
	   (point)
	   (progn
	     (search-forward "\n<!--X-Body-of-Message-->\n" nil t)
	     (point)))
	  (when (search-forward "\n<!--X-Body-of-Message-End-->\n" nil t)
	    (forward-line -1)
	    (delete-region (point) (point-max)))
	  (nnweb-remove-markup)
	  (nnweb-decode-entities)))
    (goto-char (point-min))
    (nnshimbun-insert-header header)
    (insert "Content-Type: text/html; charset=ISO-2022-JP\nMIME-Version: 1.0\n\n"))
  (encode-coding-string (buffer-string)
			(mime-charset-to-coding-system "ISO-2022-JP")))

(defun nnshimbun-make-fml-contents (header &rest args)
  (require 'mml)
  (catch 'stop
    (if (search-forward "<SPAN CLASS=mailheaders>" nil t)
	(delete-region (point-min) (point))
      (throw 'stop nil))
    (if (search-forward "</PRE>")
	(progn
	  (beginning-of-line)
	  (delete-region (point) (point-max)))
      (throw 'stop nil))
    (if (search-backward "</SPAN>")
	(progn
	  (beginning-of-line)
	  (kill-line))
      (throw 'stop nil))
    (save-restriction
      (narrow-to-region (point-min) (point))
      (subst-char-in-region (point-min) (point-max) ?\t ?  t)
      (nnweb-decode-entities)
      (goto-char (point-min))
      (let (buf field value start value-beg end)
	(while (and (setq start (point))
		    (re-search-forward "<SPAN CLASS=\\(.*\\)>\\(.*\\)</SPAN>:"
				       nil t)
		    (setq field (match-string 2))
		    (re-search-forward 
		     (concat "<SPAN CLASS=" (match-string 1) "-value>") nil t)
		    (setq value-beg (point))
		    (search-forward "</SPAN>" nil t)
		    (setq end (point)))
	  (setq value (buffer-substring value-beg
					(progn (search-backward "</SPAN>")
					       (point))))
	  (delete-region start end)
	  (cond ((string= field "Date")
		 (push (cons 'date value) buf))
		((string= field "From")
		 (push (cons 'from value) buf))
		((string= field "Subject")
		 (push (cons 'subject value) buf))
		((string= field "Message-Id")
		 (push (cons 'id value) buf))
		((string= field "References")
		 (push (cons 'references value) buf))
		(t
		 (insert (concat field ": " value "\n")))))
	(nnshimbun-nov-fix-header nnshimbun-current-group header buf)
	(goto-char (point-min))
	(nnshimbun-insert-header header))
      (goto-char (point-max)))
    ;; Processing body.
    (save-restriction
      (narrow-to-region (point) (point-max))
      (nnweb-remove-markup)
      (nnweb-decode-entities)))
  (encode-coding-string (buffer-string)
			(mime-charset-to-coding-system "ISO-2022-JP")))

;;; www.asahi.com

(defun nnshimbun-asahi-get-headers ()
  (when (search-forward "\n<!-- Start of past -->\n" nil t)
    (delete-region (point-min) (point))
    (when (search-forward "\n<!-- End of past -->\n" nil t)
      (forward-line -1)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (let (headers)
	(while (re-search-forward
		"^■<a href=\"\\(\\([0-9][0-9][0-9][0-9]\\)/past/\\([A-z]*[0-9]*\\)\\.html\\)\"> *"
		nil t)
	  (let ((id (format "<%s%s%%%s>"
			    (match-string 2)
			    (match-string 3)
			    nnshimbun-current-group))
		(url (match-string 1)))
	    (push (make-full-mail-header
		   0
		   (nnshimbun-mime-encode-string
		    (mapconcat 'identity
			       (split-string
				(buffer-substring
				 (match-end 0)
				 (progn (search-forward "<br>" nil t) (point)))
				"\\(<[^>]+>\\|\r\\)")
			       ""))
		   nnshimbun-from-address
		   "" id "" 0 0 (concat nnshimbun-url url))
		  headers)))
	(setq headers (nreverse headers))
	(let ((i 0))
	  (while (and (nth i headers)
		      (re-search-forward
		       "^\\[\\([0-9][0-9]\\)/\\([0-9][0-9]\\) \\([0-9][0-9]:[0-9][0-9]\\)\\]"
		       nil t))
	    (let ((month (string-to-number (match-string 1)))
		  (date (decode-time (current-time))))
	      (mail-header-set-date
	       (nth i headers)
	       (nnshimbun-make-date-string
		(if (and (eq 12 month) (eq 1 (nth 4 date)))
		    (1- (nth 5 date))
		  (nth 5 date))
		month
		(string-to-number (match-string 2))
		(match-string 3))))
	    (setq i (1+ i))))
	(nreverse headers)))))



;;; www.sponichi.co.jp

(defun nnshimbun-sponichi-get-headers ()
  (when (search-forward "ニュースインデックス" nil t)
    (delete-region (point-min) (point))
    (when (search-forward "アドタグ" nil t)
      (forward-line 2)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (let ((case-fold-search t) headers)
	(while (re-search-forward
		"^<a href=\"/\\(\\([A-z]*\\)/kiji/\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)\\.html\\)\">"
		nil t)
	  (let ((url (match-string 1))
		(id (format "<%s%s%s%s%%%s>"
			    (match-string 3)
			    (match-string 4)
			    (match-string 5)
			    (match-string 6)
			    nnshimbun-current-group))
		(date (nnshimbun-make-date-string
		       (string-to-number (match-string 3))
		       (string-to-number (match-string 4))
		       (string-to-number (match-string 5)))))
	    (push (make-full-mail-header
		   0
		   (nnshimbun-mime-encode-string
		    (mapconcat 'identity
			       (split-string
				(buffer-substring
				 (match-end 0)
				 (progn (search-forward "<br>" nil t) (point)))
				"<[^>]+>")
			       ""))
		   nnshimbun-from-address
		   date id "" 0 0 (concat nnshimbun-url url))
		  headers)))
	headers))))



;;; CNET Japan

(defun nnshimbun-cnet-get-headers ()
  (let ((case-fold-search t) headers)
    (while (search-forward "\n<!--*****見出し*****-->\n" nil t)
      (let ((subject (buffer-substring (point) (gnus-point-at-eol)))
	    (point (point)))
	(forward-line -2)
	(when (looking-at "<a href=\"/\\(News/\\([0-9][0-9][0-9][0-9]\\)/Item/\\([0-9][0-9]\\([0-9][0-9]\\)\\([0-9][0-9]\\)-[0-9]+\\).html\\)\">")
	  (let ((url (match-string 1))
		(id  (format "<%s%s%%%s>"
			     (match-string 2)
			     (match-string 3)
			     nnshimbun-current-group))
		(date (nnshimbun-make-date-string
		       (string-to-number (match-string 2))
		       (string-to-number (match-string 4))
		       (string-to-number (match-string 5)))))
	    (push (make-full-mail-header
		   0
		   (nnshimbun-mime-encode-string subject)
		   nnshimbun-from-address
		   date id "" 0 0 (concat nnshimbun-url url))
		  headers)))
	(goto-char point)))
    headers))



;;; Wired

(defun nnshimbun-wired-get-all-headers ()
  (save-excursion
    (set-buffer nnshimbun-buffer)
    (let ((group-header-alist (mapcar (lambda (g) (cons g nil)) nnshimbun-groups))
	  (case-fold-search t)
	  (regexp (format
		   "<a href=\"\\(%s\\|/\\)\\(news/news/\\(%s\\)/story/\\(\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)[0-9]+\\)\\.html\\)\"><b>"
		   (regexp-quote nnshimbun-url)
		   (nnshimbun-regexp-opt nnshimbun-groups))))
      (dolist (xover (list (concat nnshimbun-url "news/news/index.html")
			   (concat nnshimbun-url "news/news/last_seven.html")))
	(erase-buffer)
	(nnshimbun-retrieve-url xover t)
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (let* ((url   (concat nnshimbun-url (match-string 2)))
		 (group (downcase (match-string 3)))
		 (id    (format "<%s%%%s>" (match-string 4) group))
		 (date  (nnshimbun-make-date-string
			 (string-to-number (match-string 5))
			 (string-to-number (match-string 6))
			 (string-to-number (match-string 7))))
		 (header (make-full-mail-header
			  0
			  (nnshimbun-mime-encode-string
			   (mapconcat 'identity
				      (split-string
				       (buffer-substring
					(match-end 0)
					(progn (search-forward "</b>" nil t) (point)))
				       "<[^>]+>")
				      ""))
			  nnshimbun-from-address
			  date id "" 0 0 url))
		 (x (assoc group group-header-alist)))
	    (setcdr x (cons header (cdr x))))))
      group-header-alist)))



;;; www.yomiuri.co.jp

(defun nnshimbun-yomiuri-get-all-headers ()
  (save-excursion
    (set-buffer nnshimbun-buffer)
    (erase-buffer)
    (nnshimbun-retrieve-url (eval nnshimbun-index-url) t)
    (let ((case-fold-search t)
	  (group-header-alist (mapcar (lambda (g) (cons g nil)) nnshimbun-groups)))
      (dolist (group nnshimbun-groups)
	(let (start)
	  (goto-char (point-min))
	  (when (and (search-forward (format "\n<!-- /news/%s=start -->\n" group) nil t)
		     (setq start (point))
		     (search-forward (format "\n<!-- /news/%s=end -->\n" group) nil t))
	    (forward-line -1)
	    (save-restriction
	      (narrow-to-region start (point))
	      (goto-char start)
	      (while (re-search-forward
		      "<a href=\"/\\([0-9]+\\)/\\(\\(\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)[A-z0-9]+\\)\\.htm\\)\"[^>]*>"
		      nil t)
		(let ((url   (concat (match-string 1) "a/" (match-string 2)))
		      (id    (format "<%s%s%%%s>"
				     (match-string 1)
				     (match-string 3)
				     group))
		      (year  (string-to-number (match-string 4)))
		      (month (string-to-number (match-string 5)))
		      (day   (string-to-number (match-string 6)))
		      (subject (mapconcat
				'identity
				(split-string
				 (buffer-substring
				  (match-end 0)
				  (progn (search-forward "<br>" nil t) (point)))
				 "<[^>]+>")
				""))
		      date x)
		  (when (string-match "^◆" subject)
		    (setq subject (substring subject (match-end 0))))
		  (if (string-match "(\\([0-9][0-9]:[0-9][0-9]\\))$" subject)
		      (setq date (nnshimbun-make-date-string
				  year month day (match-string 1 subject))
			    subject (substring subject 0 (match-beginning 0)))
		    (setq date (nnshimbun-make-date-string year month day)))
		  (setcdr (setq x (assoc group group-header-alist))
			  (cons (make-full-mail-header
				 0
				 (nnshimbun-mime-encode-string subject)
				 nnshimbun-from-address
				 date id "" 0 0 (concat nnshimbun-url url))
				(cdr x)))))))))
      group-header-alist)))



;;; Zdnet Japan

(defun nnshimbun-zdnet-get-headers ()
  (let ((case-fold-search t) headers)
    (goto-char (point-min))
    (let (start)
      (while (and (search-forward "<!--" nil t)
		  (setq start (- (point) 4))
		  (search-forward "-->" nil t))
	(delete-region start (point))))
    (goto-char (point-min))
    (while (re-search-forward
	    "<a href=\"\\(/news/\\)?\\(\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([^\\.]+\\).html\\)\"><font size=\"4\"><strong>"
	    nil t)
      (let ((year  (+ 2000 (string-to-number (match-string 3))))
	    (month (string-to-number (match-string 4)))
	    (day   (string-to-number (match-string 5)))
	    (id    (format "<%s%s%s%s%%%s>"
			   (match-string 3)
			   (match-string 4)
			   (match-string 5)
			   (match-string 6)
			   nnshimbun-current-group))
	    (url (match-string 2)))
	(push (make-full-mail-header
	       0
	       (nnshimbun-mime-encode-string
		(mapconcat 'identity
			   (split-string
			    (buffer-substring
			     (match-end 0)
			     (progn (search-forward "</a>" nil t) (point)))
			    "<[^>]+>")
			   ""))
	       nnshimbun-from-address
	       (nnshimbun-make-date-string year month day)
	       id  "" 0 0 (concat nnshimbun-url url))
	      headers)))
    (nreverse headers)))



;;; MLs on www.mew.org

(defmacro nnshimbun-mew-concat-url (url)
  `(concat nnshimbun-url
	   (nth 1 (assoc nnshimbun-current-group nnshimbun-mew-groups))
	   "/"
	   ,url))

(defmacro nnshimbun-mew-reverse-order-p ()
  `(nth 2 (assoc nnshimbun-current-group nnshimbun-mew-groups)))

(defmacro nnshimbun-mew-spew-p ()
  `(nth 3 (assoc nnshimbun-current-group nnshimbun-mew-groups)))

(defsubst nnshimbun-mew-retrieve-xover (aux)
  (erase-buffer)
  (nnshimbun-retrieve-url
   (nnshimbun-mew-concat-url (if (= aux 1) "index.html" (format "mail%d.html" aux)))
   t))

(defconst nnshimbun-mew-regexp "<A[^>]*HREF=\"\\(msg\\([0-9]+\\).html\\)\">\\([^<]+\\)<")

(defmacro nnshimbun-mew-extract-header-values ()
  `(progn
     (setq url (nnshimbun-mew-concat-url (match-string 1))
	   id (format "<%05d%%%s>"
		      (1- (string-to-number (match-string 2)))
		      nnshimbun-current-group)
	   subject (match-string 3))
     (forward-line 1)
     (if (nnshimbun-search-id nnshimbun-current-group id)
	 (throw 'stop headers)
       (push (make-full-mail-header
	      0
	      (nnshimbun-mime-encode-string subject)
	      (if (looking-at "<EM>\\([^<]+\\)<")
		  (nnshimbun-mime-encode-string (match-string 1))
		"")
	      "" id "" 0 0 url)
	     headers))))

(eval-and-compile
  (if (fboundp 'mime-entity-fetch-field)
      ;; For Semi-Gnus.
      (defmacro nnshimbun-mew-mail-header-subject (header)
	`(mime-entity-fetch-field ,header 'Subject))
    ;; For pure Gnus.
    (defalias 'nnshimbun-mew-mail-header-subject 'mail-header-subject)))

(defun nnshimbun-mew-get-headers ()
  (if (nnshimbun-mew-spew-p)
      (let ((headers (nnshimbun-mew-get-headers-1)))
	(erase-buffer)
	(insert-buffer-substring (nnshimbun-open-nov nnshimbun-current-group))
	(delq nil
	      (mapcar
	       (lambda (header)
		 (goto-char (point-min))
		 (let ((subject (nnshimbun-mew-mail-header-subject header))
		       (found))
		   (while (and (not found)
			       (search-forward subject nil t))
		     (if (not (and (search-backward "\t" nil t)
				   (not (search-backward "\t" (gnus-point-at-bol) t))))
			 (forward-line 1)
		       (setq found t)))
		   (if found
		       nil
		     (goto-char (point-max))
		     (nnheader-insert-nov header)
		     header)))
	       headers)))
    (nnshimbun-mew-get-headers-1)))

(defun nnshimbun-mew-get-headers-1 ()
  (let (headers)
    (when (re-search-forward
	   "<A[^>]*HREF=\"mail\\([0-9]+\\)\\.html\">\\[?Last Page\\]?</A>" nil t)
      (let ((limit (string-to-number (match-string 1))))
	(catch 'stop
	  (if (nnshimbun-mew-reverse-order-p)
	      (let ((aux 1))
		(while (let (id url subject)
			 (while (re-search-forward nnshimbun-mew-regexp nil t)
			   (nnshimbun-mew-extract-header-values))
			 (< aux limit))
		  (nnshimbun-mew-retrieve-xover (setq aux (1+ aux)))))
	    (while (> limit 0)
	      (nnshimbun-mew-retrieve-xover limit)
	      (setq limit (1- limit))
	      (let (id url subject)
		(goto-char (point-max))
		(while (re-search-backward nnshimbun-mew-regexp nil t)
		  (nnshimbun-mew-extract-header-values)
		  (forward-line -2)))))
	  headers)))))



;;; MLs on www.xemacs.org

(defmacro nnshimbun-xemacs-concat-url (url)
  `(concat nnshimbun-url nnshimbun-current-group "/" ,url))

(defun nnshimbun-xemacs-get-headers ()
  (let (headers auxs aux)
    (catch 'stop
      (while (re-search-forward
	      (concat "<A HREF=\"/" nnshimbun-current-group
		      "/\\([12][0-9][0-9][0-9][0-1][0-9]\\)/\">\\[Index\\]")
	      nil t)
	(setq auxs (append auxs (list (match-string 1)))))
      (while auxs
	(erase-buffer)
	(nnshimbun-retrieve-url
	 (nnshimbun-xemacs-concat-url (concat (setq aux (car auxs)) "/")))
	(let (id url subject)
	  (goto-char (point-max))
	  (while (re-search-backward
		  "<A[^>]*HREF=\"\\(msg\\([0-9]+\\).html\\)\">\\([^<]+\\)<"
		  nil t)
	    (setq url (nnshimbun-xemacs-concat-url
		       (concat aux "/" (match-string 1)))
		  id (format "<%s%05d%%%s>"
			     aux
			     (string-to-number (match-string 2))
			     nnshimbun-current-group)
		  subject (match-string 3))
	    (forward-line 1)
	    (if (nnshimbun-search-id nnshimbun-current-group id)
		(throw 'stop headers)
	      (push (make-full-mail-header
		     0
		     (nnshimbun-mime-encode-string subject)
		     (if (looking-at "<td><em>\\([^<]+\\)<")
			 (match-string 1)
		       "")
		     "" id "" 0 0 url)
		    headers))
	    (message "%s" id)
	    (forward-line -2)))
	(setq auxs (cdr auxs))))
    headers))

;;; MLs on www.jp.netbsd.org

(defun nnshimbun-netbsd-get-headers ()
  (let ((case-fold-search t) headers months)
    (goto-char (point-min))
    (while (re-search-forward "<A HREF=\"\\([0-9]+\\)/\\(threads.html\\)?\">" nil t)
      (push (match-string 1) months))
    (setq months (nreverse months))
    (catch 'exit
      (dolist (month months)
	(erase-buffer)
	(nnshimbun-retrieve-url
	 (format "%s%s/%s/maillist.html" nnshimbun-url nnshimbun-current-group month)
	 t)
	(let (id url subject)
	  (while (re-search-forward
		  "<A[^>]*HREF=\"\\(msg\\([0-9]+\\)\\.html\\)\">\\([^<]+\\)</A>"
		  nil t)
	    (setq url (format "%s%s/%s/%s"
			      nnshimbun-url
			      nnshimbun-current-group
			      month
			      (match-string 1))
		  id (format "<%s%05d%%%s>"
			     month
			     (string-to-number (match-string 2))
			     nnshimbun-current-group)
		  subject (match-string 3))
	    (if (nnshimbun-search-id nnshimbun-current-group id)
		(throw 'exit headers)
	      (push (make-full-mail-header
		     0
		     (nnshimbun-mime-encode-string subject)
		     (if (looking-at "</STRONG> *<EM>\\([^<]+\\)<")
			 (nnshimbun-mime-encode-string (match-string 1))
		       "")
		     "" id "" 0 0 url)
		    headers)))))
      headers)))

;;; MLs using fml
(defun nnshimbun-fml-get-headers ()
  (let (headers auxs aux)
    (catch 'stop
      (while (re-search-forward "<a href=\"\\([0-9]+\\(\\.week\\|\\.month\\)?\\)/index.html\">" nil t)
	(setq auxs (append auxs (list (match-string 1)))))
      (while auxs
	(erase-buffer)
	(nnshimbun-retrieve-url
	 (concat nnshimbun-url (setq aux (car auxs)) "/"))
	(subst-char-in-region (point-min) (point-max) ?\t ?  t)
	(let (id url date subject from)
	  (goto-char (point-min))
	  (while (re-search-forward
		  "<LI><A HREF=\"\\([0-9]+\\.html\\)\">Article .*</A> <DIV><SPAN CLASS=article>Article <SPAN CLASS=article-value>\\([0-9]+\\)</SPAN></SPAN> at <SPAN CLASS=Date-value>\\([^<]*\\)</SPAN> <SPAN CLASS=Subject>Subject: <SPAN CLASS=Subject-value>\\([^<]*\\)</SPAN></SPAN></DIV><DIV><SPAN CLASS=From>From: <SPAN CLASS=From-value>\\([^<]*\\)</SPAN></SPAN></DIV>"
		  nil t)
	    (setq url (concat nnshimbun-url aux "/" (match-string 1))
		  id (format "<%s%05d%%%s>"
			     aux
			     (string-to-number (match-string 2))
			     nnshimbun-current-group)
		  date (match-string 3)
		  subject (match-string 4)
		  from (match-string 5))
	    (forward-line 1)
	    (if (nnshimbun-search-id nnshimbun-current-group id)
		(throw 'stop headers)
	      (push (make-full-mail-header
		     0
		     (nnshimbun-mime-encode-string subject)
		     from date id "" 0 0 url)
		    headers))
	    ;;(message "%s" id)
	    ))
	(setq auxs (cdr auxs))))
    headers))

(provide 'nnshimbun)
;;; nnshimbun.el ends here.

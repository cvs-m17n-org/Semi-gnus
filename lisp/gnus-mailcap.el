;;; mailcap.el --- Functions for displaying MIME parts
;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: William M. Perry <wmperry@aventail.com>
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news, mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-and-compile
  (require 'cl))
(require 'mail-parse)

(defvar mailcap-parse-args-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?{ "(" table)
    (modify-syntax-entry ?} ")" table)
    table)
  "A syntax table for parsing sgml attributes.")

(defvar mailcap-mime-data
  '(("application"
     ("x-x509-ca-cert"
      (viewer . ssl-view-site-cert)
      (test . (fboundp 'ssl-view-site-cert))
      (type . "application/x-x509-ca-cert"))
     ("x-x509-user-cert"
      (viewer . ssl-view-user-cert)
      (test . (fboundp 'ssl-view-user-cert))
      (type . "application/x-x509-user-cert"))
     ("octet-stream"
      (viewer . mailcap-save-binary-file)
      (type ."application/octet-stream"))
     ("dvi"
      (viewer . "open %s")
      (type   . "application/dvi")
      (test   . (eq (mm-device-type) 'ns)))
     ("dvi"
      (viewer . "xdvi %s")
      (test   . (eq (mm-device-type) 'x))
      ("needsx11")
      (type   . "application/dvi"))
     ("dvi"
      (viewer . "dvitty %s")
      (test   . (not (getenv "DISPLAY")))
      (type   . "application/dvi"))
     ("emacs-lisp"
      (viewer . mailcap-maybe-eval)
      (type   . "application/emacs-lisp"))
     ("x-tar"
      (viewer . mailcap-save-binary-file)
      (type   . "application/x-tar"))
     ("x-latex"
      (viewer . tex-mode)
      (test   . (fboundp 'tex-mode))
      (type   . "application/x-latex"))
     ("x-tex"
      (viewer . tex-mode)
      (test   . (fboundp 'tex-mode))
      (type   . "application/x-tex"))
     ("latex"
      (viewer . tex-mode)
      (test   . (fboundp 'tex-mode))
      (type   . "application/latex"))
     ("tex"
      (viewer . tex-mode)
      (test   . (fboundp 'tex-mode))
      (type   . "application/tex"))
     ("texinfo"
      (viewer . texinfo-mode)
      (test   . (fboundp 'texinfo-mode))
      (type   . "application/tex"))
     ("zip"
      (viewer . mailcap-save-binary-file)
      (type   . "application/zip")
      ("copiousoutput"))
     ("pdf"
      (viewer . "acroread %s")
      (type   . "application/pdf"))
     ("postscript"
      (viewer . "open %s")
      (type   . "application/postscript")
      (test   . (eq (mm-device-type) 'ns)))
     ("postscript"
      (viewer . "ghostview %s")
      (type . "application/postscript")
      (test   . (eq (mm-device-type) 'x))
      ("needsx11"))
     ("postscript"
      (viewer . "ps2ascii %s")
      (type . "application/postscript")
      (test . (not (getenv "DISPLAY")))
      ("copiousoutput")))
    ("audio"
     ("x-mpeg"
      (viewer . "maplay %s")
      (type   . "audio/x-mpeg"))
     (".*"
      (viewer . mailcap-save-binary-file)
      (test   . (or (featurep 'nas-sound)
		      (featurep 'native-sound)))
      (type   . "audio/*"))
     (".*"
      (viewer . "showaudio")
      (type   . "audio/*")))
    ("message"
     ("rfc-*822"
      (viewer . gnus-article-prepare-display)
      (test   . (and (featurep 'gnus)
		     (gnus-alive-p)))
      (type   . "message/rfc-822"))
     ("rfc-*822"
      (viewer . vm-mode)
      (test   . (fboundp 'vm-mode))
      (type   . "message/rfc-822"))
     ("rfc-*822"
      (viewer . w3-mode)
      (test   . (fboundp 'w3-mode))
      (type   . "message/rfc-822"))
     ("rfc-*822"
      (viewer . view-mode)
      (test   . (fboundp 'view-mode))
      (type   . "message/rfc-822"))
     ("rfc-*822"
      (viewer . fundamental-mode)
      (type   . "message/rfc-822")))
    ("image"
     ("x-xwd"
      (viewer  . "xwud -in %s")
      (type    . "image/x-xwd")
      ("compose" . "xwd -frame > %s")
      (test    . (eq (mm-device-type) 'x))
      ("needsx11"))
     ("x11-dump"
      (viewer . "xwud -in %s")
      (type . "image/x-xwd")
      ("compose" . "xwd -frame > %s")
      (test   . (eq (mm-device-type) 'x))
      ("needsx11"))
     ("windowdump"
      (viewer . "xwud -in %s")
      (type . "image/x-xwd")
      ("compose" . "xwd -frame > %s")
      (test   . (eq (mm-device-type) 'x))
      ("needsx11"))
     (".*"
      (viewer . "aopen %s")
      (type   . "image/*")
      (test   . (eq (mm-device-type) 'ns)))
     (".*"
      (viewer . "xv -perfect %s")
      (type . "image/*")
      (test   . (eq (mm-device-type) 'x))
      ("needsx11")))
    ("text"
     ("plain"
      (viewer  . w3-mode)
      (test    . (fboundp 'w3-mode))
      (type    . "text/plain"))
     ("plain"
      (viewer  . view-mode)
      (test    . (fboundp 'view-mode))
      (type    . "text/plain"))
     ("plain"
      (viewer  . fundamental-mode)
      (type    . "text/plain"))
     ("enriched"
      (viewer . enriched-decode-region)
      (test   . (fboundp 'enriched-decode))
      (type   . "text/enriched"))
     ("html"
      (viewer . mm-w3-prepare-buffer)
      (test   . (fboundp 'w3-prepare-buffer))
      (type   . "text/html")))
    ("video"
     ("mpeg"
      (viewer . "mpeg_play %s")
      (type   . "video/mpeg")
      (test   . (eq (mm-device-type) 'x))
      ("needsx11")))
    ("x-world"
     ("x-vrml"
      (viewer  . "webspace -remote %s -URL %u")
      (type    . "x-world/x-vrml")
      ("description"
       "VRML document")))
    ("archive"
     ("tar"
      (viewer . tar-mode)
      (type . "archive/tar")
      (test . (fboundp 'tar-mode)))))
     "*The mailcap structure is an assoc list of assoc lists.
1st assoc list is keyed on the major content-type
2nd assoc list is keyed on the minor content-type (which can be a regexp)

Which looks like:
-----------------
 ((\"application\"
   (\"postscript\" . <info>))
  (\"text\"
   (\"plain\" . <info>)))

Where <info> is another assoc list of the various information
related to the mailcap RFC.  This is keyed on the lowercase
attribute name (viewer, test, etc).  This looks like:
 ((viewer . viewerinfo)
  (test   . testinfo)
  (xxxx   . \"string\"))

Where viewerinfo specifies how the content-type is viewed.  Can be
a string, in which case it is run through a shell, with
appropriate parameters, or a symbol, in which case the symbol is
funcall'd, with the buffer as an argument.

testinfo is a list of strings, or nil.  If nil, it means the
viewer specified is always valid.  If it is a list of strings,
these are used to determine whether a viewer passes the 'test' or
not.")

(defvar mailcap-download-directory nil
  "*Where downloaded files should go by default.")

(defvar mailcap-temporary-directory (or (getenv "TMPDIR") "/tmp")
  "*Where temporary files go.")

;;;
;;; Utility functions
;;;

(defun mailcap-generate-unique-filename (&optional fmt)
  "Generate a unique filename in mailcap-temporary-directory"
  (if (not fmt)
      (let ((base (format "mailcap-tmp.%d" (user-real-uid)))
	    (fname "")
	    (x 0))
	(setq fname (format "%s%d" base x))
	(while (file-exists-p
		(expand-file-name fname mailcap-temporary-directory))
	  (setq x (1+ x)
		fname (concat base (int-to-string x))))
	(expand-file-name fname mailcap-temporary-directory))
    (let ((base (concat "mm" (int-to-string (user-real-uid))))
	  (fname "")
	  (x 0))
      (setq fname (format fmt (concat base (int-to-string x))))
      (while (file-exists-p
	      (expand-file-name fname mailcap-temporary-directory))
	(setq x (1+ x)
	      fname (format fmt (concat base (int-to-string x)))))
      (expand-file-name fname mailcap-temporary-directory))))

(defun mailcap-save-binary-file ()
  (goto-char (point-min))
  (let ((file (read-file-name
	       "Filename to save as: "
	       (or mailcap-download-directory "~/")))
	(require-final-newline nil))
    (write-region (point-min) (point-max) file)
    (kill-buffer (current-buffer))))

(defun mailcap-maybe-eval ()
  "Maybe evaluate a buffer of emacs lisp code"
  (if (yes-or-no-p "This is emacs-lisp code, evaluate it? ")
      (eval-buffer (current-buffer))
    (emacs-lisp-mode)))

;;;
;;; The mailcap parser
;;;

(defun mailcap-replace-regexp (regexp to-string)
  ;; Quiet replace-regexp.
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-string t nil)))

(defvar mailcap-parsed-p nil)

(defun mailcap-parse-mailcaps (&optional path force)
  "Parse out all the mailcaps specified in a unix-style path string PATH.
If FORCE, re-parse even if already parsed."
  (interactive (list nil t))
  (when (or (not mailcap-parsed-p)
	    force)
    (cond
     (path nil)
     ((getenv "MAILCAPS") (setq path (getenv "MAILCAPS")))
     ((memq system-type '(ms-dos ms-windows windows-nt))
      (setq path (mapconcat 'expand-file-name '("~/mail.cap" "~/etc/mail.cap")
			    ";")))
     (t (setq path (mapconcat 'expand-file-name
			      '("~/.mailcap"
				"/etc/mailcap:/usr/etc/mailcap"
				"/usr/local/etc/mailcap") ":"))))
    (let ((fnames (reverse
		   (split-string
		    path (if (memq system-type
				   '(ms-dos ms-windows windows-nt))
			     ";"
			   ":"))))
	  fname)
      (while fnames
	(setq fname (car fnames))
       (if (and (file-exists-p fname) (file-readable-p fname)
                (file-regular-p fname))
	    (mailcap-parse-mailcap (car fnames)))
	(setq fnames (cdr fnames))))
    (setq mailcap-parsed-p t)))

(defun mailcap-parse-mailcap (fname)
  ;; Parse out the mailcap file specified by FNAME
  (let (major				; The major mime type (image/audio/etc)
	minor				; The minor mime type (gif, basic, etc)
	save-pos			; Misc saved positions used in parsing
	viewer				; How to view this mime type
	info				; Misc info about this mime type
	)
    (with-temp-buffer
      (insert-file-contents fname)
      (set-syntax-table mailcap-parse-args-syntax-table)
      (mailcap-replace-regexp "#.*" "")	; Remove all comments
      (mailcap-replace-regexp "\n+" "\n") ; And blank lines
      (mailcap-replace-regexp "\\\\[ \t\n]+" " ") ; And collapse spaces
      (mailcap-replace-regexp (concat (regexp-quote "\\") "[ \t]*\n") "")
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
	(skip-chars-forward " \t\n")
	(setq save-pos (point)
	      info nil)
	(skip-chars-forward "^/;")
	(downcase-region save-pos (point))
	(setq major (buffer-substring save-pos (point)))
	(skip-chars-forward "/ \t\n")
	(setq save-pos (point))
	(skip-chars-forward "^;")
	(downcase-region save-pos (point))
	(setq minor
	      (cond
	       ((= ?* (or (char-after save-pos) 0)) ".*")
	       ((= (point) save-pos) ".*")
	       (t (buffer-substring save-pos (point)))))
	(skip-chars-forward "; \t\n")
	;;; Got the major/minor chunks, now for the viewers/etc
	;;; The first item _must_ be a viewer, according to the
	;;; RFC for mailcap files (#1343)
	(skip-chars-forward "; \t\n")
	(setq save-pos (point))
	(skip-chars-forward "^;\n")
	(if (= (or (char-after save-pos) 0) ?')
	    (setq viewer (progn
			   (narrow-to-region (1+ save-pos) (point))
			   (goto-char (point-min))
			   (prog1
			       (read (current-buffer))
			     (goto-char (point-max))
			     (widen))))
	  (setq viewer (buffer-substring save-pos (point))))
	(setq save-pos (point))
	(end-of-line)
	(setq info (nconc (list (cons 'viewer viewer)
				(cons 'type (concat major "/"
						    (if (string= minor ".*")
							"*" minor))))
			  (mailcap-parse-mailcap-extras save-pos (point))))
	(mailcap-mailcap-entry-passes-test info)
	(mailcap-add-mailcap-entry major minor info)))))

(defun mailcap-parse-mailcap-extras (st nd)
  ;; Grab all the extra stuff from a mailcap entry
  (let (
	name				; From name=
	value				; its value
	results				; Assoc list of results
	name-pos			; Start of XXXX= position
	val-pos				; Start of value position
	done				; Found end of \'d ;s?
	)
    (save-restriction
      (narrow-to-region st nd)
      (goto-char (point-min))
      (skip-chars-forward " \n\t;")
      (while (not (eobp))
	(setq done nil)
	(skip-chars-forward " \";\n\t")
	(setq name-pos (point))
	(skip-chars-forward "^ \n\t=")
	(downcase-region name-pos (point))
	(setq name (buffer-substring name-pos (point)))
	(skip-chars-forward " \t\n")
	(if (/= (or (char-after (point)) 0)  ?=) ; There is no value
	    (setq value nil)
	  (skip-chars-forward " \t\n=")
	  (setq val-pos (point))
	  (if (memq (char-after val-pos) '(?\" ?'))
	      (progn
		(setq val-pos (1+ val-pos))
		(condition-case nil
		    (progn
		      (forward-sexp 1)
		      (backward-char 1))
		  (error (goto-char (point-max)))))
	    (while (not done)
	      (skip-chars-forward "^;")
	      (if (= (or (char-after (1- (point))) 0) ?\\ )
		  (progn
		    (subst-char-in-region (1- (point)) (point) ?\\ ? )
		    (skip-chars-forward ";"))
		(setq done t))))
	  (setq	value (buffer-substring val-pos (point))))
	(setq results (cons (cons name value) results)))
      results)))

(defun mailcap-mailcap-entry-passes-test (info)
  ;; Return t iff a mailcap entry passes its test clause or no test
  ;; clause is present.
  (let (status				; Call-process-regions return value
	(test (assq 'test info))	; The test clause
	)
    (setq status (and test (split-string (cdr test) " ")))
    (if (and (assoc "needsx11" info) (not (getenv "DISPLAY")))
	(setq status nil)
      (cond
       ((and (equal (nth 0 status) "test")
	     (equal (nth 1 status) "-n")
	     (or (equal (nth 2 status) "$DISPLAY")
		 (equal (nth 2 status) "\"$DISPLAY\"")))
	(setq status (if (getenv "DISPLAY") t nil)))
       ((and (equal (nth 0 status) "test")
	     (equal (nth 1 status) "-z")
	     (or (equal (nth 2 status) "$DISPLAY")
		 (equal (nth 2 status) "\"$DISPLAY\"")))
	(setq status (if (getenv "DISPLAY") nil t)))
       (test nil)
       (t nil)))
    (and test (listp test) (setcdr test status))))

;;;
;;; The action routines.
;;;

(defun mailcap-possible-viewers (major minor)
  ;; Return a list of possible viewers from MAJOR for minor type MINOR
  (let ((exact '())
	(wildcard '()))
    (while major
      (cond
       ((equal (car (car major)) minor)
	(setq exact (cons (cdr (car major)) exact)))
       ((and minor (string-match (car (car major)) minor))
	(setq wildcard (cons (cdr (car major)) wildcard))))
      (setq major (cdr major)))
    (nconc (nreverse exact) (nreverse wildcard))))

(defun mailcap-unescape-mime-test (test type-info)
  (let (save-pos save-chr subst)
    (cond
     ((symbolp test) test)
     ((and (listp test) (symbolp (car test))) test)
     ((or (stringp test)
	  (and (listp test) (stringp (car test))
	       (setq test (mapconcat 'identity test " "))))
      (with-temp-buffer
	(insert test)
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward "^%")
	  (if (/= (- (point)
		     (progn (skip-chars-backward "\\\\")
			    (point)))
		  0)			; It is an escaped %
	      (progn
		(delete-char 1)
		(skip-chars-forward "%."))
	    (setq save-pos (point))
	    (skip-chars-forward "%")
	    (setq save-chr (char-after (point)))
	    (cond
	     ((null save-chr) nil)
	     ((= save-chr ?t)
	      (delete-region save-pos (progn (forward-char 1) (point)))
	      (insert (or (cdr (assq 'type type-info)) "\"\"")))
	     ((= save-chr ?M)
	      (delete-region save-pos (progn (forward-char 1) (point)))
	      (insert "\"\""))
	     ((= save-chr ?n)
	      (delete-region save-pos (progn (forward-char 1) (point)))
	      (insert "\"\""))
	     ((= save-chr ?F)
	      (delete-region save-pos (progn (forward-char 1) (point)))
	      (insert "\"\""))
	     ((= save-chr ?{)
	      (forward-char 1)
	      (skip-chars-forward "^}")
	      (downcase-region (+ 2 save-pos) (point))
	      (setq subst (buffer-substring (+ 2 save-pos) (point)))
	      (delete-region save-pos (1+ (point)))
	      (insert (or (cdr (assoc subst type-info)) "\"\"")))
	     (t nil))))
	(buffer-string)))
     (t (error "Bad value to mailcap-unescape-mime-test. %s" test)))))

(defvar mailcap-viewer-test-cache nil)

(defun mailcap-viewer-passes-test (viewer-info type-info)
  ;; Return non-nil iff the viewer specified by VIEWER-INFO passes its
  ;; test clause (if any).
  (let* ((test-info (assq 'test viewer-info))
	 (test (cdr test-info))
	 (otest test)
	 (viewer (cdr (assoc 'viewer viewer-info)))
	 (default-directory (expand-file-name "~/"))
	 status parsed-test cache result)
    (if (setq cache (assoc test mailcap-viewer-test-cache))
	(cadr cache)
      (setq
       result
       (cond
	((not test-info) t)		; No test clause
	((not test) nil)		; Already failed test
	((eq test t) t)			; Already passed test
	((and (symbolp test)		; Lisp function as test
	      (fboundp test))
	 (funcall test type-info))
	((and (symbolp test)		; Lisp variable as test
	      (boundp test))
	 (symbol-value test))
	((and (listp test)		; List to be eval'd
	      (symbolp (car test)))
	 (eval test))
	(t
	 (setq test (mailcap-unescape-mime-test test type-info)
	       test (list shell-file-name nil nil nil
			  shell-command-switch test)
	       status (apply 'call-process test))
	 (= 0 status))))
      (push (list otest result) mailcap-viewer-test-cache)
      result)))

(defun mailcap-add-mailcap-entry (major minor info)
  (let ((old-major (assoc major mailcap-mime-data)))
    (if (null old-major)		; New major area
	(setq mailcap-mime-data
	      (cons (cons major (list (cons minor info)))
		    mailcap-mime-data))
      (let ((cur-minor (assoc minor old-major)))
	(cond
	 ((or (null cur-minor)		; New minor area, or
	      (assq 'test info))	; Has a test, insert at beginning
	  (setcdr old-major (cons (cons minor info) (cdr old-major))))
	 ((and (not (assq 'test info)) ; No test info, replace completely
	       (not (assq 'test cur-minor)))
	  (setcdr cur-minor info))
	 (t
	  (setcdr old-major (cons (cons minor info) (cdr old-major)))))))))

;;;
;;; The main whabbo
;;;

(defun mailcap-viewer-lessp (x y)
  ;; Return t iff viewer X is more desirable than viewer Y
  (let ((x-wild (string-match "[*?]" (or (cdr-safe (assq 'type x)) "")))
	(y-wild (string-match "[*?]" (or (cdr-safe (assq 'type y)) "")))
	(x-lisp (not (stringp (or (cdr-safe (assq 'viewer x)) ""))))
	(y-lisp (not (stringp (or (cdr-safe (assq 'viewer y)) "")))))
    (cond
     ((and x-lisp (not y-lisp))
      t)
     ((and (not y-lisp) x-wild (not y-wild))
      t)
     ((and (not x-wild) y-wild)
      t)
     (t nil))))

(defun mailcap-mime-info (string &optional request)
  "Get the MIME viewer command for STRING, return nil if none found.
Expects a complete content-type header line as its argument.

Second argument REQUEST specifies what information to return.  If it is
nil or the empty string, the viewer (second field of the mailcap
entry) will be returned.  If it is a string, then the mailcap field
corresponding to that string will be returned (print, description,
whatever).  If a number, then all the information for this specific
viewer is returned.  If `all', then all possible viewers for
this type is returned."
  (let (
	major				; Major encoding (text, etc)
	minor				; Minor encoding (html, etc)
	info				; Other info
	save-pos			; Misc. position during parse
	major-info			; (assoc major mailcap-mime-data)
	minor-info			; (assoc minor major-info)
	test				; current test proc.
	viewers				; Possible viewers
	passed				; Viewers that passed the test
	viewer				; The one and only viewer
	ctl)
    (save-excursion
      (setq ctl (mail-header-parse-content-type (or string "text/plain")))
      (setq major (split-string (car ctl) "/"))
      (setq minor (cadr major)
	    major (car major))
      (when (setq major-info (cdr (assoc major mailcap-mime-data)))
	(when (setq viewers (mailcap-possible-viewers major-info minor))
	  (setq info (mapcar (lambda (a) (cons (symbol-name (car a))
					       (cdr a)))
			     (cdr ctl)))
	  (while viewers
	    (if (mailcap-viewer-passes-test (car viewers) info)
		(setq passed (cons (car viewers) passed)))
	    (setq viewers (cdr viewers)))
	  (setq passed (sort (nreverse passed) 'mailcap-viewer-lessp))
	  (setq viewer (car passed))))
      (when (and (stringp (cdr (assq 'viewer viewer)))
		 passed)
	(setq viewer (car passed)))
      (cond
       ((and (null viewer) (not (equal major "default")) request)
	(mailcap-mime-info "default" request))
       ((or (null request) (equal request ""))
	(mailcap-unescape-mime-test (cdr (assq 'viewer viewer)) info))
       ((stringp request)
	(if (or (eq request 'test) (eq request 'viewer))
	    (mailcap-unescape-mime-test
	     (cdr-safe (assoc request viewer)) info)))
       ((eq request 'all)
	passed)
       (t
	;; MUST make a copy *sigh*, else we modify mailcap-mime-data
	(setq viewer (copy-tree viewer))
	(let ((view (assq 'viewer viewer))
	      (test (assq 'test viewer)))
	  (if view (setcdr view (mailcap-unescape-mime-test (cdr view) info)))
	  (if test (setcdr test (mailcap-unescape-mime-test (cdr test) info))))
	viewer)))))

;;;
;;; Experimental MIME-types parsing
;;;

(defvar mailcap-mime-extensions
  '((""          . "text/plain")
    (".abs"      . "audio/x-mpeg")
    (".aif"      . "audio/aiff")
    (".aifc"     . "audio/aiff")
    (".aiff"     . "audio/aiff")
    (".ano"      . "application/x-annotator")
    (".au"       . "audio/ulaw")
    (".avi"      . "video/x-msvideo")
    (".bcpio"    . "application/x-bcpio")
    (".bin"      . "application/octet-stream")
    (".cdf"      . "application/x-netcdr")
    (".cpio"     . "application/x-cpio")
    (".csh"      . "application/x-csh")
    (".dvi"      . "application/x-dvi")
    (".el"       . "application/emacs-lisp")
    (".eps"      . "application/postscript")
    (".etx"      . "text/x-setext")
    (".exe"      . "application/octet-stream")
    (".fax"      . "image/x-fax")
    (".gif"      . "image/gif")
    (".hdf"      . "application/x-hdf")
    (".hqx"      . "application/mac-binhex40")
    (".htm"      . "text/html")
    (".html"     . "text/html")
    (".icon"     . "image/x-icon")
    (".ief"      . "image/ief")
    (".jpg"      . "image/jpeg")
    (".macp"     . "image/x-macpaint")
    (".man"      . "application/x-troff-man")
    (".me"       . "application/x-troff-me")
    (".mif"      . "application/mif")
    (".mov"      . "video/quicktime")
    (".movie"    . "video/x-sgi-movie")
    (".mp2"      . "audio/x-mpeg")
    (".mp3"      . "audio/x-mpeg")
    (".mp2a"     . "audio/x-mpeg2")
    (".mpa"      . "audio/x-mpeg")
    (".mpa2"     . "audio/x-mpeg2")
    (".mpe"      . "video/mpeg")
    (".mpeg"     . "video/mpeg")
    (".mpega"    . "audio/x-mpeg")
    (".mpegv"    . "video/mpeg")
    (".mpg"      . "video/mpeg")
    (".mpv"      . "video/mpeg")
    (".ms"       . "application/x-troff-ms")
    (".nc"       . "application/x-netcdf")
    (".nc"       . "application/x-netcdf")
    (".oda"      . "application/oda")
    (".pbm"      . "image/x-portable-bitmap")
    (".pdf"      . "application/pdf")
    (".pgm"      . "image/portable-graymap")
    (".pict"     . "image/pict")
    (".png"      . "image/png")
    (".pnm"      . "image/x-portable-anymap")
    (".ppm"      . "image/portable-pixmap")
    (".ps"       . "application/postscript")
    (".qt"       . "video/quicktime")
    (".ras"      . "image/x-raster")
    (".rgb"      . "image/x-rgb")
    (".rtf"      . "application/rtf")
    (".rtx"      . "text/richtext")
    (".sh"       . "application/x-sh")
    (".sit"      . "application/x-stuffit")
    (".snd"      . "audio/basic")
    (".src"      . "application/x-wais-source")
    (".tar"      . "archive/tar")
    (".tcl"      . "application/x-tcl")
    (".tcl"      . "application/x-tcl")
    (".tex"      . "application/x-tex")
    (".texi"     . "application/texinfo")
    (".tga"      . "image/x-targa")
    (".tif"      . "image/tiff")
    (".tiff"     . "image/tiff")
    (".tr"       . "application/x-troff")
    (".troff"    . "application/x-troff")
    (".tsv"      . "text/tab-separated-values")
    (".txt"      . "text/plain")
    (".vbs"      . "video/mpeg")
    (".vox"      . "audio/basic")
    (".vrml"     . "x-world/x-vrml")
    (".wav"      . "audio/x-wav")
    (".wrl"      . "x-world/x-vrml")
    (".xbm"      . "image/xbm")
    (".xpm"      . "image/x-pixmap")
    (".xwd"      . "image/windowdump")
    (".zip"      . "application/zip")
    (".ai"       . "application/postscript")
    (".jpe"      . "image/jpeg")
    (".jpeg"     . "image/jpeg"))
  "*An assoc list of file extensions and the MIME content-types they
correspond to.")

(defun mailcap-parse-mimetypes (&optional path)
  ;; Parse out all the mimetypes specified in a unix-style path string PATH
  (cond
   (path nil)
   ((getenv "MIMETYPES") (setq path (getenv "MIMETYPES")))
   ((memq system-type '(ms-dos ms-windows windows-nt))
    (setq path (mapconcat 'expand-file-name
			  '("~/mime.typ" "~/etc/mime.typ") ";")))
   (t (setq path (mapconcat 'expand-file-name
			    '("~/.mime-types"
			      "/etc/mime-types:/usr/etc/mime-types"
			      "/usr/local/etc/mime-types"
			      "/usr/local/www/conf/mime-types") ":"))))
  (let ((fnames (reverse
		 (split-string path
			       (if (memq system-type
					 '(ms-dos ms-windows windows-nt))
				   ";" ":"))))
	fname)
    (while fnames
      (setq fname (car fnames))
      (if (and (file-exists-p fname) (file-readable-p fname))
	  (mailcap-parse-mimetype-file (car fnames)))
      (setq fnames (cdr fnames)))))

(defun mailcap-parse-mimetype-file (fname)
  ;; Parse out a mime-types file
  (let (type				; The MIME type for this line
	extns				; The extensions for this line
	save-pos			; Misc. saved buffer positions
	)
    (with-temp-buffer
      (insert-file-contents fname)
      (mailcap-replace-regexp "#.*" "")
      (mailcap-replace-regexp "\n+" "\n")
      (mailcap-replace-regexp "[ \t]+$" "")
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
	(skip-chars-forward " \t\n")
	(setq save-pos (point))
	(skip-chars-forward "^ \t")
	(downcase-region save-pos (point))
	(setq type (buffer-substring save-pos (point)))
	(while (not (eolp))
	  (skip-chars-forward " \t")
	  (setq save-pos (point))
	  (skip-chars-forward "^ \t\n")
	  (setq extns (cons (buffer-substring save-pos (point)) extns)))
	(while extns
	  (setq mailcap-mime-extensions
		(cons
		 (cons (if (= (string-to-char (car extns)) ?.)
			   (car extns)
			 (concat "." (car extns))) type)
		 mailcap-mime-extensions)
		extns (cdr extns)))))))

(defun mailcap-extension-to-mime (extn)
  "Return the MIME content type of the file extensions EXTN."
  (if (and (stringp extn)
	   (not (eq (string-to-char extn) ?.)))
      (setq extn (concat "." extn)))
  (cdr (assoc (downcase extn) mailcap-mime-extensions)))

(defvar mailcap-binary-suffixes
  (if (memq system-type '(ms-dos windows-nt))
      '(".exe" ".com" ".bat" ".cmd" ".btm" "")
    '("")))

(defun mailcap-command-p (command)
  "Say whether COMMAND is in the exec path.
The path of COMMAND will be returned iff COMMAND is a command."
  (let ((path (if (file-name-absolute-p command) '(nil) exec-path))
 	file dir)
    (catch 'found
      (while (setq dir (pop path))
	(let ((suffixes mailcap-binary-suffixes))
	  (while suffixes
	    (when (and (file-executable-p
			(setq file (expand-file-name
				    (concat command (pop suffixes))
				    dir)))
		       (not (file-directory-p file)))
	      (throw 'found file))))))))

(provide 'mailcap)

;;; mailcap.el ends here

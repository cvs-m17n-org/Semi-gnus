;;; dgnushack.el --- a hack to set the load path for byte-compiling
;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Version: 4.19
;; Keywords: news, path

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

;;; Code:

(defalias 'facep 'ignore)

(require 'cl)

(defvar srcdir (or (getenv "srcdir") "."))

(defun my-getenv (str)
  (let ((val (getenv str)))
    (if (equal val "no") nil val)))

(if (my-getenv "lispdir")
    (push (my-getenv "lispdir") load-path))

(push (or (my-getenv "URLDIR") (expand-file-name "../../url/lisp/" srcdir))
      load-path)

(push (or (my-getenv "W3DIR") (expand-file-name "../../w3/lisp/" srcdir))
      load-path)

;(push "/usr/share/emacs/site-lisp" load-path)

(unless (featurep 'xemacs)
  (define-compiler-macro last (&whole form x &optional n)
    (if (and (fboundp 'last)
	     (subrp (symbol-function 'last)))
	form
      (if n
	  `(let* ((x ,x)
		  (n ,n)
		  (m 0)
		  (p x))
	     (while (consp p)
	       (incf m)
	       (pop p))
	     (if (<= n 0)
		 p
	       (if (< n m)
		   (nthcdr (- m n) x)
		 x)))
	`(let ((x ,x))
	   (while (consp (cdr x))
	     (pop x))
	   x))))

  (define-compiler-macro coerce (&whole form x type)
    (if (and (fboundp 'coerce)
	     (subrp (symbol-function 'coerce)))
	form
      `(let ((x ,x)
	     (type ,type))
	 (cond ((eq type 'list) (if (listp x) x (append x nil)))
	       ((eq type 'vector) (if (vectorp x) x (vconcat x)))
	       ((eq type 'string) (if (stringp x) x (concat x)))
	       ((eq type 'array) (if (arrayp x) x (vconcat x)))
	       ((and (eq type 'character) (stringp x) (= (length x) 1))
		(aref x 0))
	       ((and (eq type 'character) (symbolp x)
		     (= (length (symbol-name x)) 1))
		(aref (symbol-name x) 0))
	       ((eq type 'float) (float x))
	       ((typep x type) x)
	       (t (error "Can't coerce %s to type %s" x type))))))

  (define-compiler-macro merge (&whole form type seq1 seq2 pred &rest keys)
    (if (and (fboundp 'merge)
	     (subrp (symbol-function 'merge)))
	form
      `(let ((type ,type)
	     (seq1 ,seq1)
	     (seq2 ,seq2)
	     (pred ,pred))
	 (or (listp seq1) (setq seq1 (append seq1 nil)))
	 (or (listp seq2) (setq seq2 (append seq2 nil)))
	 (let ((res nil))
	   (while (and seq1 seq2)
	     (if (funcall pred (car seq2) (car seq1))
		 (push (pop seq2) res)
	       (push (pop seq1) res)))
	   (coerce (nconc (nreverse res) seq1 seq2) type)))))

  (define-compiler-macro subseq (&whole form seq start &optional end)
    (if (and (fboundp 'subseq)
	     (subrp (symbol-function 'subseq)))
	form
      (if end
	  `(let ((seq ,seq)
		 (start ,start)
		 (end ,end))
	     (if (stringp seq)
		 (substring seq start end)
	       (let (len)
		 (if (< end 0)
		     (setq end (+ end (setq len (length seq)))))
		 (if (< start 0)
		     (setq start (+ start (or len (setq len (length seq))))))
		 (cond ((listp seq)
			(if (> start 0)
			    (setq seq (nthcdr start seq)))
			(let ((res nil))
			  (while (>= (setq end (1- end)) start)
			    (push (pop seq) res))
			  (nreverse res)))
		       (t
			(let ((res (make-vector (max (- end start) 0) nil))
			      (i 0))
			  (while (< start end)
			    (aset res i (aref seq start))
			    (setq i (1+ i)
				  start (1+ start)))
			  res))))))
	`(let ((seq ,seq)
	       (start ,start))
	   (if (stringp seq)
	       (substring seq start)
	     (let (len)
	       (if (< start 0)
		   (setq start (+ start (or len (setq len (length seq))))))
	       (cond ((listp seq)
		      (if (> start 0)
			  (setq seq (nthcdr start seq)))
		      (copy-sequence seq))
		     (t
		      (let* ((end (or len (length seq)))
			     (res (make-vector (max (- end start) 0) nil))
			     (i 0))
			(while (< start end)
			  (aset res i (aref seq start))
			  (setq i (1+ i)
				start (1+ start)))
			res)))))))))

  (define-compiler-macro copy-list (&whole form list)
    (if (and (fboundp 'copy-list)
	     (subrp (symbol-function 'copy-list)))
	form
      `(let ((list ,list))
	 (if (consp list)
	     (let ((res nil))
	       (while (consp list) (push (pop list) res))
	       (prog1 (nreverse res) (setcdr res list)))
	   (car list)))))
  )

;; If we are building w3 in a different directory than the source
;; directory, we must read *.el from source directory and write *.elc
;; into the building directory.  For that, we define this function
;; before loading bytecomp.  Bytecomp doesn't overwrite this function.
(defun byte-compile-dest-file (filename)
  "Convert an Emacs Lisp source file name to a compiled file name.
 In addition, remove directory name part from FILENAME."
  (setq filename (byte-compiler-base-file-name filename))
  (setq filename (file-name-sans-versions filename))
  (setq filename (file-name-nondirectory filename))
  (if (memq system-type '(win32 w32 mswindows windows-nt))
      (setq filename (downcase filename)))
  (cond ((eq system-type 'vax-vms)
	 (concat (substring filename 0 (string-match ";" filename)) "c"))
	((string-match emacs-lisp-file-regexp filename)
	 (concat (substring filename 0 (match-beginning 0)) ".elc"))
	(t (concat filename ".elc"))))

(require 'bytecomp)
;; To avoid having defsubsts and inlines happen.
;(if (featurep 'xemacs)
;    (require 'byte-optimize)
;  (require 'byte-opt))
;(defun byte-optimize-inline-handler (form)
;  "byte-optimize-handler for the `inline' special-form."
;  (cons 'progn (cdr form)))
;(defalias 'byte-compile-file-form-defsubst 'byte-compile-file-form-defun)

(push srcdir load-path)
(load (expand-file-name "lpath.el" srcdir) nil t)

(defalias 'device-sound-enabled-p 'ignore)
(defalias 'play-sound-file 'ignore)
(defalias 'nndb-request-article 'ignore)
(defalias 'efs-re-read-dir 'ignore)
(defalias 'ange-ftp-re-read-dir 'ignore)
(defalias 'define-mail-user-agent 'ignore)

(eval-and-compile
  (unless (featurep 'xemacs)
    (defalias 'get-popup-menu-response 'ignore)
    (defalias 'event-object 'ignore)
    (defalias 'x-defined-colors 'ignore)
    (defalias 'read-color 'ignore)))

(defun dgnushack-compile (&optional warn)
  ;;(setq byte-compile-dynamic t)
  (unless warn
    (setq byte-compile-warnings
	  '(free-vars unresolved callargs redefine)))
  (unless (locate-library "cus-edit")
    (error "You do not seem to have Custom installed.
Fetch it from <URL:http://www.dina.kvl.dk/~abraham/custom/>.
You also then need to add the following to the lisp/dgnushack.el file:

     (push \"~/lisp/custom\" load-path)

Modify to suit your needs."))
  (let ((files (directory-files srcdir nil "^[^=].*\\.el$"))
	;;(byte-compile-generate-call-tree t)
	file elc)
    ;; Avoid barfing (from gnus-xmas) because the etc directory is not yet
    ;; installed.
    (when (featurep 'xemacs)
      (setq gnus-xmas-glyph-directory "dummy"))
    (dolist (file '("dgnushack.el" "lpath.el"))
      (setq files (delete file files)))
    (when (featurep 'base64)
      (setq files (delete "base64.el" files)))
    (condition-case code
	(require 'w3-parse)
      (error
       (message "No w3: %s %s" (cadr code) (or (locate-library "w3-parse") ""))
       (dolist (file '("nnultimate.el" "webmail.el" "nnwfm.el"))
	 (setq files (delete file files)))))
    (condition-case code
	(require 'mh-e)
      (error
       (message "No mh-e: %s %s" (cadr code) (or (locate-library "mh-e") ""))
       (setq files (delete "gnus-mh.el" files))))
    (condition-case code
	(require 'xml)
      (error
       (message "No xml: %s %s" (cadr code) (or (locate-library "xml") ""))
       (setq files (delete "nnrss.el" files))))
    (dolist (file
	     (if (featurep 'xemacs)
		 '("md5.el")
	       '("gnus-xmas.el" "messagexmas.el" "nnheaderxm.el")))
      (setq files (delete file files)))

    (dolist (file files)
      (setq file (expand-file-name file srcdir))
      (when (and (file-exists-p
		  (setq elc (concat (file-name-nondirectory file) "c")))
		 (file-newer-than-file-p file elc))
	(delete-file elc)))

    (while (setq file (pop files))
      (setq file (expand-file-name file srcdir))
      (when (or (not (file-exists-p
		      (setq elc (concat (file-name-nondirectory file) "c"))))
		(file-newer-than-file-p file elc))
	(ignore-errors
	  (byte-compile-file file))))))

(defun dgnushack-recompile ()
  (require 'gnus)
  (byte-recompile-directory "." 0))

(defvar dgnushack-gnus-load-file (expand-file-name "gnus-load.el"))
(defvar	dgnushack-cus-load-file (expand-file-name "cus-load.el"))

(defun dgnushack-make-cus-load ()
  (load "cus-dep")
  (let ((cusload-base-file dgnushack-cus-load-file))
    (if (fboundp 'custom-make-dependencies)
	(custom-make-dependencies)
      (Custom-make-dependencies))))

(defun dgnushack-make-auto-load ()
  (require 'autoload)
  (unless (make-autoload '(define-derived-mode child parent name
			    "docstring" body)
			 "file")
    (defadvice make-autoload (around handle-define-derived-mode activate)
      "Handle `define-derived-mode'."
      (if (eq (car-safe (ad-get-arg 0)) 'define-derived-mode)
	  (setq ad-return-value
		(list 'autoload
		      (list 'quote (nth 1 (ad-get-arg 0)))
		      (ad-get-arg 1)
		      (nth 4 (ad-get-arg 0))
		      t nil))
	ad-do-it))
    (put 'define-derived-mode 'doc-string-elt 3))
  (let ((generated-autoload-file dgnushack-gnus-load-file)
	(make-backup-files nil)
	(autoload-package-name "gnus"))
    (if (featurep 'xemacs)
	(if (file-exists-p generated-autoload-file)
	    (delete-file generated-autoload-file))
      (with-temp-file generated-autoload-file
	(insert ?\014)))
    (batch-update-autoloads)))

(defun dgnushack-make-load ()
  (message (format "Generating %s..." dgnushack-gnus-load-file))
  (with-temp-file dgnushack-gnus-load-file
    (insert-file-contents dgnushack-cus-load-file)
    (delete-file dgnushack-cus-load-file)
    (goto-char (point-min))
    (search-forward ";;; Code:")
    (forward-line)
    (delete-region (point-min) (point))
    (insert "\
;;; gnus-load.el --- automatically extracted custom dependencies and autoload
;;
;;; Code:
")
    (goto-char (point-max))
    (if (search-backward "custom-versions-load-alist" nil t)
	(forward-line -1)
      (forward-line -1)
      (while (eq (char-after) ?\;)
	(forward-line -1))
      (forward-line))
    (delete-region (point) (point-max))
    (insert "\n")
    ;; smiley-* are duplicated. Remove them all.
    (let ((point (point)))
      (insert-file-contents dgnushack-gnus-load-file)
      (goto-char point)
      (while (search-forward "smiley-" nil t)
	(beginning-of-line)
	(if (looking-at "(autoload ")
	    (delete-region (point) (progn (forward-sexp) (point)))
	  (forward-line))))
    ;;
    (goto-char (point-max))
    (when (search-backward "\n(provide " nil t)
      (forward-line -1)
      (delete-region (point) (point-max)))
    (insert "\

\(provide 'gnus-load)

;;; Local Variables:
;;; version-control: never
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:
;;; gnus-load.el ends here
")
    ;; Workaround the bug in some version of XEmacs.
    (when (featurep 'xemacs)
      (condition-case nil
	  (require 'cus-load)
	(error nil))
      (goto-char (point-min))
      (when (and (fboundp 'custom-add-loads)
		 (not (search-forward "\n(autoload 'custom-add-loads " nil t)))
	(search-forward "\n;;; Code:" nil t)
	(forward-line 1)
	(insert "\n(autoload 'custom-add-loads \"cus-load\")\n"))))
  (message (format "Compiling %s..." dgnushack-gnus-load-file))
  (byte-compile-file dgnushack-gnus-load-file))

;;; dgnushack.el ends here

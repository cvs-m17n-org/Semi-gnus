;;; dgnushack.el --- a hack to set the load path for byte-compiling
;; Copyright (C) 1994,95,96,97,98,99 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	Katsumi Yamaoka <yamaoka@jpl.org>
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

;; Set coding priority of Shift-JIS to the bottom.
(defvar *predefined-category*)
(defvar coding-category-list)
(if (featurep 'xemacs)
    (fset 'set-coding-priority 'ignore)
  (fset 'coding-priority-list 'ignore)
  (fset 'set-coding-priority-list 'ignore))
(cond ((and (featurep 'xemacs) (featurep 'mule))
       (if (memq 'shift-jis (coding-priority-list))
	   (set-coding-priority-list
	    (nconc (delq 'shift-jis (coding-priority-list)) '(shift-jis)))))
      ((boundp 'MULE)
       (put '*coding-category-sjis* 'priority (length *predefined-category*)))
      ((featurep 'mule)
       (if (memq 'coding-category-sjis coding-category-list)
	   (set-coding-priority
	    (nconc (delq 'coding-category-sjis coding-category-list)
		   '(coding-category-sjis))))))

(fset 'facep 'ignore)

(require 'cl)

;; cl functions.
(define-compiler-macro mapc (&whole form fn seq &rest rest)
  (if (and (fboundp 'mapc)
	   (subrp (symbol-function 'mapc)))
      form
    (if rest
	`(let* ((fn ,fn)
		(seq ,seq)
		(args (cons seq ,rest))
		(m (apply (function min) (mapcar (function length) args)))
		(n 0))
	   (while (< n m)
	     (apply fn (mapcar (function (lambda (arg) (nth n arg))) args))
	     (setq n (1+ n)))
	   seq)
      `(let ((seq ,seq))
	 (mapcar ,fn seq)
	 seq))))

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

(defvar srcdir (or (getenv "srcdir") "."))

(push srcdir load-path)

;; Attempt to pickup the additional load-path(s).
(load (expand-file-name "dgnuspath.el" srcdir) nil nil t)
(condition-case err
    (load "~/.lpath.el" t nil t)
  (error (message "Error in \"~/.lpath.el\" file: %s" err)))

(condition-case nil
    (char-after)
  (wrong-number-of-arguments
   ;; Optimize byte code for `char-after'.
;;;   (put 'char-after 'byte-optimizer 'byte-optimize-char-after)
;;;   (defun byte-optimize-char-after (form)
;;;     (if (null (cdr form))
;;;	 '(char-after (point))
;;;       form))
   (byte-defop-compiler char-after 0-1)))

(condition-case nil
    (char-before)
  (wrong-number-of-arguments
   (define-compiler-macro char-before (&whole form &optional pos)
     (if (null pos)
	 '(char-before (point))
       form))))

;; `char-after' and `char-before' must be well-behaved before lpath.el
;; is loaded.  Because it requires `poe' via `path-util'.
(load (expand-file-name "lpath.el" srcdir) nil t t)

(unless (fboundp 'byte-compile-file-form-custom-declare-variable)
  ;; Bind defcustom'ed variables.
  (put 'custom-declare-variable 'byte-hunk-handler
       'byte-compile-file-form-custom-declare-variable)
  (defun byte-compile-file-form-custom-declare-variable (form)
    (if (memq 'free-vars byte-compile-warnings)
	(setq byte-compile-bound-variables
	      (cons (nth 1 (nth 1 form)) byte-compile-bound-variables)))
    form))

;; Bind functions defined by `defun-maybe'.
(put 'defun-maybe 'byte-hunk-handler 'byte-compile-file-form-defun-maybe)
(defun byte-compile-file-form-defun-maybe (form)
  (if (and (not (fboundp (nth 1 form)))
	   (memq 'unresolved byte-compile-warnings))
      (setq byte-compile-function-environment
	    (cons (cons (nth 1 form)
			(cons 'lambda (cdr (cdr form))))
		  byte-compile-function-environment)))
  form)

(condition-case nil
    :symbol-for-testing-whether-colon-keyword-is-available-or-not
  (void-variable
   ;; Bind keywords.
   (mapcar (lambda (keyword) (set keyword keyword))
	   '(:button-keymap :data :file :mime-handle))))

;; Unknown variables and functions.
(unless (boundp 'buffer-file-coding-system)
  (defvar buffer-file-coding-system (symbol-value 'file-coding-system)))
(autoload 'font-lock-set-defaults "font-lock")
(unless (fboundp 'coding-system-get)
  (defalias 'coding-system-get 'ignore))
(when (boundp 'MULE)
  (defalias 'find-coding-system 'ignore))
(unless (fboundp 'get-charset-property)
  (defalias 'get-charset-property 'ignore))
(unless (featurep 'xemacs)
  (defalias 'Custom-make-dependencies 'ignore)
  (defalias 'toolbar-gnus 'ignore)
  (defalias 'update-autoloads-from-directory 'ignore))
(autoload 'texinfo-parse-line-arg "texinfmt")

(unless (fboundp 'with-temp-buffer)
  ;; Pickup some macros.
  (require 'emu))

(defalias 'device-sound-enabled-p 'ignore)
(defalias 'play-sound-file 'ignore)
(defalias 'nndb-request-article 'ignore)
(defalias 'efs-re-read-dir 'ignore)
(defalias 'ange-ftp-re-read-dir 'ignore)
(defalias 'define-mail-user-agent 'ignore)

(eval-and-compile
  (unless (string-match "XEmacs" emacs-version)
    (fset 'get-popup-menu-response 'ignore)
    (fset 'event-object 'ignore)
    (fset 'x-defined-colors 'ignore)
    (fset 'read-color 'ignore)))

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
  (let ((files (delete "dgnuspath.el"
		       (directory-files srcdir nil "^[^=].*\\.el$")))
	(xemacs (string-match "XEmacs" emacs-version))
	;;(byte-compile-generate-call-tree t)
	file elc)
    (condition-case ()
	(require 'w3-forms)
      (error
       (dolist (file '("nnweb.el" "nnlistserv.el" "nnultimate.el"
		       "nnslashdot.el" "nnwarchive.el" "webmail.el"))
	 (setq files (delete file files)))))
    (condition-case ()
	(require 'bbdb)
      (error (setq files (delete "gnus-bbdb.el" files))))
    (while (setq file (pop files))
      (unless (or (and (not xemacs)
		       (member file
			       '("gnus-xmas.el" "gnus-picon.el"
				 "messagexmas.el" "nnheaderxm.el"
				 "smiley.el" "x-overlay.el")))
		  (and (string-equal file "md5.el")
		       (not (and (fboundp 'md5)
				 (subrp (symbol-function 'md5))))))
	(setq file (expand-file-name file srcdir))
	(when (or (not (file-exists-p (setq elc (concat file "c"))))
		  (file-newer-than-file-p file elc))
	  (ignore-errors
	    (byte-compile-file file)))))))

(defun dgnushack-recompile ()
  (require 'gnus)
  (byte-recompile-directory "." 0))


;; Avoid byte-compile warnings.
(defvar gnus-product-name)
(defvar early-package-load-path)
(defvar early-packages)
(defvar last-package-load-path)
(defvar last-packages)
(defvar late-package-load-path)
(defvar late-packages)

(defconst dgnushack-info-file-regexp
  (concat "^\\(gnus\\|message\\|emacs-mime\\|gnus-ja\\|message-ja\\)"
	  "\\.info\\(-[0-9]+\\)?$"))

(defconst dgnushack-texi-file-regexp
  "^\\(gnus\\|message\\|emacs-mime\\|gnus-ja\\|message-ja\\)\\.texi$")

(defun dgnushack-make-package ()
  (require 'gnus)
  (let* ((product-name (downcase gnus-product-name))
	 (lisp-dir (concat "lisp/" product-name "/"))
	 make-backup-files)

    (message "Updating autoloads for directory %s..." default-directory)
    (let ((generated-autoload-file "auto-autoloads.el")
	  noninteractive
	  (omsg (symbol-function 'message)))
      (defun message (fmt &rest args)
	(cond ((and (string-equal "Generating autoloads for %s..." fmt)
		    (file-exists-p (file-name-nondirectory (car args))))
	       (funcall omsg fmt (file-name-nondirectory (car args))))
	      ((string-equal "No autoloads found in %s" fmt))
	      ((string-equal "Generating autoloads for %s...done" fmt))
	      (t (apply omsg fmt args))))
      (unwind-protect
	  (update-autoloads-from-directory default-directory)
	(fset 'message omsg)))
    (byte-compile-file "auto-autoloads.el")

    (with-temp-buffer
      (let ((standard-output (current-buffer)))
	(Custom-make-dependencies "."))
      (message (buffer-string)))
    (require 'cus-load)
    (byte-compile-file "custom-load.el")

    (message "Generating MANIFEST.%s for the package..." product-name)
    (with-temp-buffer
      (insert "pkginfo/MANIFEST." product-name "\n"
	      lisp-dir
	      (mapconcat
	       'identity
	       (sort (delete "dgnuspath.el"
			     (delete "patchs.elc"
				     (directory-files "." nil "\\.elc?$")))
		     'string-lessp)
	       (concat "\n" lisp-dir))
	      "\ninfo/"
	      (mapconcat
	       'identity
	       (sort (directory-files "../texi/"
				      nil dgnushack-info-file-regexp)
		     'string-lessp)
	       "\ninfo/")
	      "\n")
      (write-file (concat "../MANIFEST." product-name)))))

(defun dgnushack-install-package ()
  (let ((package-dir (car command-line-args-left))
	dirs info-dir pkginfo-dir product-name lisp-dir manifest files)
    (unless package-dir
      (when (boundp 'early-packages)
	(setq dirs (delq nil (append (when early-package-load-path
				       early-packages)
				     (when late-package-load-path
				       late-packages)
				     (when last-package-load-path
				       last-packages))))
	(while (and dirs (not package-dir))
	  (when (file-exists-p (car dirs))
	    (setq package-dir (car dirs)
		  dirs (cdr dirs))))))
    (unless package-dir
      (error "%s" "
You must specify the name of the package path as follows:

% make install-package PACKAGEDIR=/usr/local/lib/xemacs/xemacs-packages
"
	     ))
    (setq info-dir (expand-file-name "info/" package-dir)
	  pkginfo-dir (expand-file-name "pkginfo/" package-dir))
    (require 'gnus)
    (setq product-name (downcase gnus-product-name)
	  lisp-dir (expand-file-name (concat "lisp/" product-name "/")
				     package-dir)
	  manifest (concat "MANIFEST." product-name))

    (unless (file-directory-p lisp-dir)
      (make-directory lisp-dir t))
    (unless (file-directory-p info-dir)
      (make-directory info-dir))
    (unless (file-directory-p pkginfo-dir)
      (make-directory pkginfo-dir))

    (setq files
	  (sort (delete "dgnuspath.el"
			(delete "dgnuspath.elc"
				(directory-files "." nil "\\.elc?$")))
		'string-lessp))
    (mapcar
     (lambda (file)
       (unless (member file files)
	 (setq file (expand-file-name file lisp-dir))
	 (message "Removing %s..." file)
	 (condition-case nil
	     (delete-file file)
	   (error nil))))
     (directory-files lisp-dir nil nil nil t))
    (mapcar
     (lambda (file)
       (message "Copying %s to %s..." file lisp-dir)
       (copy-file file (expand-file-name file lisp-dir) t t))
     files)

    (mapcar
     (lambda (file)
       (message "Copying ../texi/%s to %s..." file info-dir)
       (copy-file (expand-file-name file "../texi/")
		  (expand-file-name file info-dir)
		  t t))
     (sort (directory-files "../texi/" nil dgnushack-info-file-regexp)
	   'string-lessp))

    (message "Copying ../%s to %s..." manifest pkginfo-dir)
    (copy-file (expand-file-name manifest "../")
	       (expand-file-name manifest pkginfo-dir) t t)

    (message "Done")))

(defun dgnushack-texi-add-suffix-and-format ()
  (dgnushack-texi-format t))

(defun dgnushack-texi-format (&optional addsuffix)
  (if (not noninteractive)
      (error "batch-texinfo-format may only be used -batch."))
  (require 'texinfmt)
  (let ((auto-save-default nil)
	(find-file-run-dired nil)
	coding-system-for-write)
    (let ((error 0)
	  file
	  (files ()))
      (while command-line-args-left
	(setq file (expand-file-name (car command-line-args-left)))
	(cond ((not (file-exists-p file))
	       (message ">> %s does not exist!" file)
	       (setq error 1
		     command-line-args-left (cdr command-line-args-left)))
	      ((file-directory-p file)
	       (setq command-line-args-left
		     (nconc (directory-files file)
			    (cdr command-line-args-left))))
	      (t
	       (setq files (cons file files)
		     command-line-args-left (cdr command-line-args-left)))))
      (while files
	(setq file (car files)
	      files (cdr files))
	(condition-case err
	    (progn
	      (if buffer-file-name (kill-buffer (current-buffer)))
	      (find-file file)
	      (setq coding-system-for-write buffer-file-coding-system)
	      (when (and addsuffix
			 (re-search-forward
			  "^@setfilename[\t ]+\\([^\t\n ]+\\)" nil t)
			 (not (string-match "\\.info$" (match-string 1))))
		(insert ".info"))
	      (buffer-disable-undo (current-buffer))
	      ;; process @include before updating node
	      ;; This might produce some problem if we use @lowersection or
	      ;; such.
	      (let ((input-directory default-directory)
		    (texinfo-command-end))
		(while (re-search-forward "^@include" nil t)
		  (setq texinfo-command-end (point))
		  (let ((filename (concat input-directory
					  (texinfo-parse-line-arg))))
		    (re-search-backward "^@include")
		    (delete-region (point) (save-excursion
					     (forward-line 1)
					     (point)))
		    (message "Reading included file: %s" filename)
		    (save-excursion
		      (save-restriction
			(narrow-to-region
			 (point)
			 (+ (point)
			    (car (cdr (insert-file-contents filename)))))
			(goto-char (point-min))
			;; Remove `@setfilename' line from included file,
			;; if any, so @setfilename command not duplicated.
			(if (re-search-forward "^@setfilename"
					       (save-excursion
						 (forward-line 100)
						 (point))
					       t)
			    (progn
			      (beginning-of-line)
			      (delete-region (point) (save-excursion
						       (forward-line 1)
						       (point))))))))))
	      (texinfo-mode)
	      (texinfo-every-node-update)
	      (set-buffer-modified-p nil)
	      (message "texinfo formatting %s..." file)
	      (texinfo-format-buffer nil)
	      (if (buffer-modified-p)
		  (progn (message "Saving modified %s" (buffer-file-name))
			 (save-buffer))))
	  (error
	   (message ">> Error: %s" (prin1-to-string err))
	   (message ">>  point at")
	   (let ((s (buffer-substring (point)
				      (min (+ (point) 100)
					   (point-max))))
		 (tem 0))
	     (while (setq tem (string-match "\n+" s tem))
	       (setq s (concat (substring s 0 (match-beginning 0))
			       "\n>>  "
			       (substring s (match-end 0)))
		     tem (1+ tem)))
	     (message ">>  %s" s))
	   (setq error 1))))
      (kill-emacs error))))

;;; dgnushack.el ends here

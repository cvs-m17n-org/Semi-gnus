;;; dgnushack.el --- a hack to set the load path for byte-compiling
;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002
;;        Free Software Foundation, Inc.

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
(if (featurep 'xemacs)
    (defalias 'set-coding-priority 'ignore)
  (defalias 'coding-priority-list 'ignore)
  (defalias 'set-coding-priority-list 'ignore))
(cond ((and (featurep 'xemacs) (featurep 'mule))
       (if (memq 'shift-jis (coding-priority-list))
	   (set-coding-priority-list
	    (append (delq 'shift-jis (coding-priority-list)) '(shift-jis)))))
      ((boundp 'MULE)
       (put '*coding-category-sjis* 'priority (length *predefined-category*)))
      ((featurep 'mule)
       (if (memq 'coding-category-sjis coding-category-list)
	   (set-coding-priority
	    (append (delq 'coding-category-sjis
			  (copy-sequence coding-category-list))
		    '(coding-category-sjis))))))

(defalias 'facep 'ignore)

(require 'cl)

(unless (and
	 ;; `dolist' might not be available because of ``-no-autoloads''.
	 (fboundp 'dolist)
	 ;; It may have been defined in egg.el.
	 (dolist (var nil t)))
  (load "cl-macs" nil t))

(defvar srcdir (or (getenv "srcdir") "."))

(defvar dgnushack-w3-directory (let ((w3dir (getenv "W3DIR")))
				 (unless (zerop (length w3dir))
				   (file-name-as-directory w3dir))))

(let ((urldir (getenv "URLDIR")))
  (unless (zerop (length urldir))
    (setq urldir (file-name-as-directory urldir))
    (push (file-name-as-directory urldir) load-path))
  (when (and dgnushack-w3-directory
	     (not (string-equal urldir dgnushack-w3-directory)))
    (push dgnushack-w3-directory load-path)))

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

(when (boundp 'MULE)
  (let (current-load-list)
    ;; Make the function to be silent at compile-time.
    (defun locate-library (library &optional nosuffix)
      "Show the full path name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY (a la calling `load' instead of `load-library')."
      (interactive "sLocate library: ")
      (catch 'answer
	(mapcar
	 '(lambda (dir)
	    (mapcar
	     '(lambda (suf)
		(let ((try (expand-file-name (concat library suf) dir)))
		  (and (file-readable-p try)
		       (null (file-directory-p try))
		       (progn
			 (or noninteractive
			     (message "Library is file %s" try))
			 (throw 'answer try)))))
	     (if nosuffix '("") '(".elc" ".el" ""))))
	 load-path)
	(or noninteractive
	    (message "No library %s in search path" library))
	nil))
    (byte-compile 'locate-library)))

(setq max-specpdl-size 3000)

(when (equal
       (cadr
	(byte-optimize-form
	 '(and
	   (< 0 1)
	   (message "The subform `(< 0 1)' should be optimized to t"))
	 'for-effect))
       '(< 0 1))
  (defadvice byte-optimize-form-code-walker
    (around fix-bug-in-and/or-forms (form for-effect) activate)
    "Fix a bug in the optimizing and/or forms.
It has already been fixed in XEmacs since 1999-12-06."
    (if (and for-effect (memq (car-safe form) '(and or)))
	(let ((fn (car form))
	      (backwards (reverse (cdr form))))
	  (while (and backwards
		      (null (setcar backwards
				    (byte-optimize-form (car backwards) t))))
	    (setq backwards (cdr backwards)))
	  (if (and (cdr form) (null backwards))
	      (byte-compile-log
	       "  all subforms of %s called for effect; deleted" form))
	  (when backwards
	    (setcdr backwards
		    (mapcar 'byte-optimize-form (cdr backwards))))
	  (setq ad-return-value (cons fn (nreverse backwards))))
      ad-do-it)))

(condition-case nil
    (char-after)
  (wrong-number-of-arguments
   ;; Optimize byte code for `char-after'.
   (put 'char-after 'byte-optimizer 'byte-optimize-char-after)
   (defun byte-optimize-char-after (form)
     (if (null (cdr form))
	 '(char-after (point))
       form))))

(condition-case nil
    (char-before)
  (wrong-number-of-arguments
   ;; Optimize byte code for `char-before'.
   (put 'char-before 'byte-optimizer 'byte-optimize-char-before)
   (defun byte-optimize-char-before (form)
     (if (null (cdr form))
	 '(char-before (point))
       form))))

(load (expand-file-name "dgnuspath.el" srcdir) nil nil t)

(condition-case err
    (load "~/.lpath.el" t nil t)
  (error (message "Error in \"~/.lpath.el\" file: %s" err)))

;; Don't load path-util until `char-after' and `char-before' have been
;; optimized because it requires `poe' and then modify the functions.

;; If the APEL modules are installed under the non-standard directory,
;; for example "/var/home/john/lisp/apel-VERSION/", you should add that
;; name using the configure option "--with-addpath=".
;; And also the directory where the EMU modules are installed, for
;; example "/usr/local/share/mule/19.34/site-lisp/", it should be
;; included in the standard `load-path' or added by the configure
;; option "--with-addpath=".
(let ((path (or (locate-library "path-util")
		(locate-library "apel/path-util")));; backward compat.
      parent lpath)
  (if path
      (progn
	(when (string-match "/$" (setq path (file-name-directory path)))
	  (setq path (substring path 0 (match-beginning 0))))
	;; path == "/var/home/john/lisp/apel-VERSION"
	(when (string-match "/$" (setq parent (file-name-directory path)))
	  (setq parent (substring path 0 (match-beginning 0))))
	;; parent == "/var/home/john/lisp"
	(if (setq lpath (or (member path load-path)
			    (member (file-name-as-directory path) load-path)))
	    (unless (or (member parent load-path)
			(member (file-name-as-directory parent) load-path))
	      (push parent (cdr lpath)))
	  (push path load-path)
	  (unless (or (member parent load-path)
		      (member (file-name-as-directory parent) load-path))
	    (push parent (cdr load-path))))
	(require 'advice)
	(require 'path-util))
    (error "
APEL modules are not found in %s.
Try to re-configure with --with-addpath=APEL_PATH and run make again.
"
	   load-path)))

(unless (locate-library "mel")
  (add-path "flim"))
(unless (module-installed-p 'mel)
  ;; FLIM 1.14 may have installed in two "flim" subdirectories.
  (push (expand-file-name "flim"
			  (file-name-directory (get-latest-path "^apel$" t)))
	load-path)
  (unless (module-installed-p 'mel)
    (error "
FLIM modules does not found in %s.
Try to re-configure with --with-addpath=FLIM_PATH and run make again.
"
	   load-path)))
(add-path "semi")

(push srcdir load-path)
(load (expand-file-name "lpath.el" srcdir) nil t t)

(load (expand-file-name "gnus-clfns.el" srcdir) nil t t)

(when (boundp 'MULE)
  ;; Bind the function `base64-encode-string' before loading canlock.
  ;; Since canlock will bind it as an autoloaded function, it causes
  ;; damage to define the function by MEL.
  (load (expand-file-name "base64.el" srcdir) nil t t)
  ;; Load special macros for compiling canlock.el.
  (load (expand-file-name "canlock-om.el" srcdir) nil t t))

(require 'custom)

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
   (defun dgnushack-bind-colon-keywords ()
     "Bind all the colon keywords for old Emacsen."
     (let ((cache (expand-file-name "dgnuskwds.el" srcdir))
	   (makefile (expand-file-name "Makefile" srcdir))
	   (buffer (get-buffer-create " *colon keywords*"))
	   keywords ignores files file dirs dir form elem make-backup-files)
       (save-excursion
	 (set-buffer buffer)
	 (let (buffer-file-format
	       format-alist
	       insert-file-contents-post-hook
	       insert-file-contents-pre-hook
	       jam-zcat-filename-list
	       jka-compr-compression-info-list)
	   (if (and (file-exists-p cache)
		    (file-exists-p makefile)
		    (file-newer-than-file-p cache makefile))
	       (progn
		 (insert-file-contents cache nil nil nil t)
		 (setq keywords (read buffer)))
	     (setq
	      ignores
	      '(:symbol-for-testing-whether-colon-keyword-is-available-or-not
		;; The following keywords will be bound by CUSTOM.
		:get :group :initialize :link :load :options :prefix
		:require :set :tag :type)
	      files (list (locate-library "semi-def")
			  (locate-library "mailcap")
			  (locate-library "mime-def")
			  (locate-library "path-util")
			  (locate-library "poem"))
	      dirs (list (file-name-as-directory (expand-file-name srcdir))))
	     (while files
	       (when (setq file (pop files))
		 (setq dir (file-name-directory file))
		 (unless (member dir dirs)
		   (push dir dirs))))
	     (message "Searching for all the colon keywords in:")
	     (while dirs
	       (setq dir (pop dirs))
	       (message " %s..." dir)
	       (setq files (directory-files dir t
					    "\\.el\\(\\.gz\\|\\.bz2\\)?$"))
	       (while files
		 (setq file (pop files))
		 (if (string-match "\\(\\.gz$\\)\\|\\.bz2$" file)
		     (let ((temp (expand-file-name "dgnustemp.el" srcdir)))
		       (when
			   (let* ((binary (if (boundp 'MULE)
					      '*noconv*
					    'binary))
				  (coding-system-for-read binary)
				  (coding-system-for-write binary)
				  (input-coding-system binary)
				  (output-coding-system binary)
				  (default-process-coding-system
				    (cons binary binary))
				  call-process-hook)
			     (insert-file-contents file nil nil nil t)
			     (when
				 (condition-case code
				     (progn
				       (if (match-beginning 1)
					   (call-process-region
					    (point-min) (point-max)
					    "gzip" t buffer nil "-cd")
					 (call-process-region
					  (point-min) (point-max)
					  "bzip2" t buffer nil "-d"))
				       t)
				   (error
				    (erase-buffer)
				    (message "In file %s: %s" file code)
				    nil))
			       (write-region (point-min) (point-max) temp
					     nil 'silent)
			       t))
			 (unwind-protect
			     (insert-file-contents temp nil nil nil t)
			   (delete-file temp))))
		   (insert-file-contents file nil nil nil t))
		 (while (setq form (condition-case nil
				       (read buffer)
				     (error nil)))
		   (when (listp form)
		     (while form
		       (setq elem (car-safe form)
			     form (cdr-safe form))
		       (unless (memq (car-safe elem)
				     '(defcustom defface defgroup
				       define-widget quote))
			 (while (consp elem)
			   (push (car elem) form)
			   (setq elem (cdr elem)))
			 (when (and elem
				    (symbolp elem)
				    (not (eq ': elem))
				    (eq ?: (aref (symbol-name elem) 0))
				    (not (memq elem ignores))
				    (not (memq elem keywords)))
			   (push elem keywords))))))))
	     (setq keywords (sort keywords
				  (lambda (a b)
				    (string-lessp (symbol-name a)
						  (symbol-name b)))))
	     (erase-buffer)
	     (insert (format "%s" keywords))
	     (write-region (point-min) (point) cache nil 'silent)
	     (message
	      "The following colon keywords will be bound at run-time:\n %s"
	      keywords))))
       (kill-buffer buffer)
       (defconst dgnushack-colon-keywords keywords)
       (while keywords
	 (set (car keywords) (car keywords))
	 (setq keywords (cdr keywords)))))
   (byte-compile 'dgnushack-bind-colon-keywords)
   (dgnushack-bind-colon-keywords)))

(when (boundp 'MULE)
  (setq :version ':version
	:set-after ':set-after)
  (require 'custom)
  (defadvice custom-handle-keyword
    (around dont-signal-an-error-even-if-unsupported-keyword-is-given
	    activate)
    "Don't signal an error even if unsupported keyword is given."
    (if (not (memq (ad-get-arg 1) '(:version :set-after)))
	ad-do-it)))

(when (boundp 'MULE)
  (put 'custom-declare-face 'byte-optimizer
       'byte-optimize-ignore-unsupported-custom-keywords)
  (put 'custom-declare-group 'byte-optimizer
       'byte-optimize-ignore-unsupported-custom-keywords)
  (defun byte-optimize-ignore-unsupported-custom-keywords (form)
    (if (or (memq ':version (nthcdr 4 form))
	    (memq ':set-after (nthcdr 4 form)))
	(let ((newform (list (car form) (nth 1 form)
			     (nth 2 form) (nth 3 form)))
	      (args (nthcdr 4 form)))
	  (while args
	    (or (memq (car args) '(:version :set-after))
		(setq newform (nconc newform (list (car args)
						   (car (cdr args))))))
	    (setq args (cdr (cdr args))))
	  newform)
      form))

  (put 'custom-declare-variable 'byte-hunk-handler
       'byte-compile-file-form-custom-declare-variable)
  (defun byte-compile-file-form-custom-declare-variable (form)
    ;; Bind defcustom'ed variables.
    (if (memq 'free-vars byte-compile-warnings)
	(setq byte-compile-bound-variables
	      (cons (nth 1 (nth 1 form)) byte-compile-bound-variables)))
    (if (memq ':version (nthcdr 4 form))
	;; Make the variable uncustomizable.
	`(defvar ,(nth 1 (nth 1 form)) ,(nth 1 (nth 2 form))
	   ,(substring (nth 3 form) (if (string-match "^[\t *]+" (nth 3 form))
					(match-end 0)
				      0)))
      ;; Ignore unsupported keyword(s).
      (if (memq ':set-after (nthcdr 4 form))
	  (let ((newform (list (car form) (nth 1 form)
			       (nth 2 form) (nth 3 form)))
		(args (nthcdr 4 form)))
	    (while args
	      (or (eq (car args) ':set-after)
		  (setq newform (nconc newform (list (car args)
						     (car (cdr args))))))
	      (setq args (cdr (cdr args))))
	    newform)
	form)))

  (defadvice byte-compile-inline-expand (around ignore-built-in-functions
						(form) activate)
    "Ignore built-in functions."
    (let* ((name (car form))
	   (fn (and (fboundp name)
		    (symbol-function name))))
      (if (subrp fn)
	  ;; Give up on inlining.
	  (setq ad-return-value form)
	ad-do-it))))

;; Unknown variables and functions.
(unless (boundp 'buffer-file-coding-system)
  (defvar buffer-file-coding-system (symbol-value 'file-coding-system)))
(unless (featurep 'xemacs)
  (defalias 'Custom-make-dependencies 'ignore)
  (defalias 'update-autoloads-from-directory 'ignore))

(defalias 'device-sound-enabled-p 'ignore)
(defalias 'play-sound-file 'ignore)
(defalias 'nndb-request-article 'ignore)
(defalias 'efs-re-read-dir 'ignore)
(defalias 'ange-ftp-re-read-dir 'ignore)
(defalias 'define-mail-user-agent 'ignore)

(eval-and-compile
  (when (featurep 'xemacs)
    (autoload 'Info-directory "info" nil t)
    (autoload 'Info-menu "info" nil t)
    (autoload 'annotations-at "annotations")
    (autoload 'apropos "apropos" nil t)
    (autoload 'apropos-command "apropos" nil t)
    (autoload 'bbdb-complete-name "bbdb-com" nil t)
    (autoload 'browse-url "browse-url" nil t)
    (autoload 'c-mode "cc-mode" nil t)
    (autoload 'customize-apropos "cus-edit" nil t)
    (autoload 'customize-save-variable "cus-edit" nil t)
    (autoload 'customize-variable "cus-edit" nil t)
    (autoload 'delete-annotation "annotations")
    (autoload 'dolist "cl-macs" nil nil 'macro)
    (autoload 'enriched-decode "enriched")
    (autoload 'info "info" nil t)
    (autoload 'make-annotation "annotations")
    (autoload 'make-display-table "disp-table")
    (autoload 'pp "pp")
    (autoload 'ps-despool "ps-print" nil t)
    (autoload 'ps-spool-buffer "ps-print" nil t)
    (autoload 'ps-spool-buffer-with-faces "ps-print" nil t)
    (autoload 'read-kbd-macro "edmacro" nil t)
    (autoload 'read-passwd "passwd")
    (autoload 'regexp-opt "regexp-opt")
    (autoload 'reporter-submit-bug-report "reporter")
    (if (emacs-version>= 21 5)
	(autoload 'setenv "process" nil t)
      (autoload 'setenv "env" nil t))
    (autoload 'smtpmail-send-it "smtpmail")
    (autoload 'sort-numeric-fields "sort" nil t)
    (autoload 'sort-subr "sort")
    (autoload 'trace-function-background "trace" nil t)
    (autoload 'w3-do-setup "w3")
    (autoload 'w3-prepare-buffer "w3-display")
    (autoload 'w3-region "w3-display" nil t)
    (defalias 'frame-char-height 'frame-height)
    (defalias 'frame-char-width 'frame-width)
    (defalias 'frame-parameter 'frame-property)
    (defalias 'make-overlay 'ignore)
    (defalias 'overlay-end 'ignore)
    (defalias 'overlay-get 'ignore)
    (defalias 'overlay-put 'ignore)
    (defalias 'overlay-start 'ignore)
    (defalias 'overlays-in 'ignore)
    (defalias 'replace-dehighlight 'ignore)
    (defalias 'replace-highlight 'ignore)
    (defalias 'run-with-idle-timer 'ignore)
    (defalias 'w3-coding-system-for-mime-charset 'ignore)))

(defconst dgnushack-unexporting-files
  (append '("dgnushack.el" "dgnuspath.el" "dgnuskwds.el" "lpath.el")
	  (condition-case nil
	      (progn (require 'shimbun) nil)
	    (error '("nnshimbun.el")))
	  (unless (or (condition-case code
			  (require 'w3-parse)
			(error
			 (message "No w3: %s%s, retrying..."
				  (error-message-string code)
				  (if (setq code (locate-library "w3-parse"))
				      (concat " (" code ")")
				    ""))
			 nil))
		      ;; Maybe mis-configured Makefile is used (e.g.
		      ;; configured for FSFmacs but XEmacs is running).
		      (let ((lp (delete dgnushack-w3-directory
					(copy-sequence load-path))))
			(if (let ((load-path lp))
			      (condition-case nil
				  (require 'w3-parse)
				(error nil)))
			    ;; If success, fix `load-path' for compiling.
			    (progn
			      (setq load-path lp)
			      (message " => fixed; W3DIR=%s"
				       (file-name-directory
					(locate-library "w3-parse")))
			      t)
			  (message " => ignored")
			  nil)))
	    '("nnultimate.el" "webmail.el" "nnwfm.el"))
	  (condition-case code
	      (progn (require 'mh-e) nil)
	    (error
	     (message "No mh-e: %s%s (ignored)"
		      (error-message-string code)
		      (if (setq code (locate-library "mh-e"))
			  (concat " (" code ")")
			""))
	     '("gnus-mh.el")))
	  (condition-case code
	      (progn (require 'xml) nil)
	    (error
	     (message "No xml: %s%s (ignored)"
		      (error-message-string code)
		      (if (setq code (locate-library "xml"))
			  (concat " (" code ")")
			""))
	     '("nnrss.el")))
	  (condition-case code
	      (progn (require 'bbdb) nil)
	    (error
	     (message "No bbdb: %s%s (ignored)"
		      (error-message-string code)
		      (if (setq code (locate-library "bbdb"))
			  (concat " (" code ")")
			""))
	     '("gnus-bbdb.el")))
	  (unless (featurep 'xemacs)
	    '("gnus-xmas.el" "messagexmas.el" "nnheaderxm.el"))
	  (when (and (not (featurep 'xemacs))
		     (<= emacs-major-version 20))
	    '("smiley.el"))
	  (when (and (fboundp 'base64-decode-string)
		     (subrp (symbol-function 'base64-decode-string)))
	    '("base64.el"))
	  (when (and (fboundp 'md5) (subrp (symbol-function 'md5)))
	    '("md5.el"))
	  (unless (boundp 'MULE)
	    '("canlock-om.el")))
  "Files which will not be installed.")

(defconst dgnushack-exporting-files
  (let ((files (directory-files srcdir nil "^[^=].*\\.el$" t)))
    (dolist (file dgnushack-unexporting-files)
      (setq files (delete file files)))
    (sort files 'string-lessp))
  "Files which will be compiled and installed.")

(defun dgnushack-exporting-files ()
  "Print name of files which will be installed."
  (princ (mapconcat 'identity dgnushack-exporting-files " ")))

(defconst dgnushack-dont-compile-files
  '("mm-bodies.el" "mm-decode.el" "mm-encode.el" "mm-extern.el"
    "mm-partial.el" "mm-url.el" "mm-uu.el" "mm-view.el" "mml-sec.el"
    "mml-smime.el" "mml.el" "mml1991.el" "mml2015.el")
  "Files which should not be byte-compiled.")

(defun dgnushack-compile-verbosely ()
  "Call dgnushack-compile with warnings ENABLED.  If you are compiling
patches to gnus, you should consider modifying make.bat to call
dgnushack-compile-verbosely.  All other users should continue to use
dgnushack-compile."
  (dgnushack-compile t))

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

  ;; Show `load-path'.
  (message "load-path=(\"%s\")"
	   (mapconcat 'identity load-path "\"\n           \""))

  (dolist (file dgnushack-exporting-files)
    (setq file (expand-file-name file srcdir))
    (when (and (file-exists-p
		(setq elc (concat (file-name-nondirectory file) "c")))
	       (file-newer-than-file-p file elc))
      (delete-file elc)))

  ;; Avoid barfing (from gnus-xmas) because the etc directory is not yet
  ;; installed.
  (when (featurep 'xemacs)
    (setq gnus-xmas-glyph-directory "dummy"))

  (let ((files dgnushack-exporting-files)
	;;(byte-compile-generate-call-tree t)
	file elc)
    (while (setq file (pop files))
      (unless (member file dgnushack-dont-compile-files)
	(setq file (expand-file-name file srcdir))
	(when (or (not (file-exists-p
			(setq elc (concat (file-name-nondirectory file) "c"))))
		  (file-newer-than-file-p file elc))
	  (ignore-errors
	    (byte-compile-file file)))))))

(defun dgnushack-recompile ()
  (require 'gnus)
  (byte-recompile-directory "." 0))

(defvar dgnushack-gnus-load-file (expand-file-name "gnus-load.el" srcdir))
(defvar dgnushack-cus-load-file (expand-file-name "cus-load.el" srcdir))
(defvar dgnushack-auto-load-file (expand-file-name "auto-autoloads.el" srcdir))

(defun dgnushack-make-cus-load ()
  (when (condition-case nil
	    (load "cus-dep")
	  (error nil))
    (let ((cusload-base-file dgnushack-cus-load-file))
      (if (fboundp 'custom-make-dependencies)
	  (custom-make-dependencies)
	(Custom-make-dependencies)))))

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
	(progn
	  (if (file-exists-p generated-autoload-file)
	      (delete-file generated-autoload-file))
	  (if (file-exists-p dgnushack-auto-load-file)
	      (delete-file dgnushack-auto-load-file)))
      (with-temp-file generated-autoload-file
	(insert ?\014)))
    (if (featurep 'xemacs)
	(let ((si:message (symbol-function 'message)))
	  (defun message (fmt &rest args)
	    (cond ((and (string-equal "Generating autoloads for %s..." fmt)
			(file-exists-p (file-name-nondirectory (car args))))
		   (funcall si:message
			    fmt (file-name-nondirectory (car args))))
		  ((string-equal "No autoloads found in %s" fmt))
		  ((string-equal "Generating autoloads for %s...done" fmt))
		  (t (apply si:message fmt args))))
	  (unwind-protect
	      (batch-update-autoloads)
	    (fset 'message si:message)))
      (batch-update-autoloads))))

(defun dgnushack-make-load ()
  (message (format "Generating %s..." dgnushack-gnus-load-file))
  (with-temp-file dgnushack-gnus-load-file
    (if (file-exists-p dgnushack-cus-load-file)
	(progn
	  (insert-file-contents dgnushack-cus-load-file)
	  (delete-file dgnushack-cus-load-file)
	  (goto-char (point-min))
	  (search-forward ";;; Code:")
	  (forward-line)
	  (delete-region (point-min) (point))
	  (unless (re-search-forward "\
^[\t ]*(autoload[\t\n ]+\\('\\|(quote[\t\n ]+\\)custom-add-loads[\t\n ]"
				     nil t)
	    (insert "\n(autoload 'custom-add-loads \"cus-load\")\n"))
	  (goto-char (point-min))
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
	  (insert "\n"))
      (insert "\
;;; gnus-load.el --- automatically extracted autoload
;;
;;; Code:
"))
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


(defun dgnushack-compose-package ()
  "Re-split the file gnus-load.el into custom-load.el and
auto-autoloads.el.  It is silly, should be improved!"
  (message "
Re-splitting gnus-load.el into custom-load.el and auto-autoloads.el...")
  (let ((customload (expand-file-name "custom-load.el" srcdir))
	(autoloads (expand-file-name "auto-autoloads.el" srcdir))
	start)
    (with-temp-buffer
      (insert-file-contents dgnushack-gnus-load-file)
      (delete-file dgnushack-gnus-load-file)
      (when (file-exists-p (concat dgnushack-gnus-load-file "c"))
	(delete-file (concat dgnushack-gnus-load-file "c")))
      (while (prog1
		 (looking-at "[\t ;]")
	       (forward-line 1)))
      (setq start (point))
      (insert "\
;;; custom-load.el --- automatically extracted custom dependencies\n
;;; Code:\n\n")
      (goto-char (point-max))
      (while (progn
	       (forward-line -1)
	       (not (looking-at "[\t ]*(custom-add-loads[\t\n ]"))))
      (forward-list 1)
      (forward-line 1)
      (insert "\n;;; custom-load.el ends here\n")
      (write-region start (point) customload)
      (while (looking-at "[\t ]*$")
	(forward-line 1))
      (setq start (point))
      (if (re-search-forward "^[\t\n ]*(if[\t\n ]+(featurep[\t\n ]" nil t)
	  (let ((from (goto-char (match-beginning 0))))
	    (delete-region from (progn
				  (forward-list 1)
				  (forward-line 1)
				  (point))))
	(while (looking-at "[\t ;]")
	  (forward-line 1)))
      (insert "(if (featurep 'gnus-autoloads) (error \"Already loaded\"))\n")
      (goto-char (point-max))
      (while (progn
	       (forward-line -1)
	       (not (looking-at "[\t ]*(provide[\t\n ]"))))
      (insert "(provide 'gnus-autoloads)\n")
      (write-region start (point) autoloads))
    (byte-compile-file customload)
    (byte-compile-file autoloads))
  (message "\
Re-splitting gnus-load.el into custom-load.el and auto-autoloads.el...done
\n"))


(defconst dgnushack-info-file-regexp-en
  (let ((names '("gnus" "message" "emacs-mime"))
	regexp name)
    (while (setq name (pop names))
      (setq regexp (concat regexp "^" name "\\.info\\(-[0-9]+\\)?$"
			   (when names "\\|"))))
    regexp)
  "Regexp matching English info files.")

(defconst dgnushack-info-file-regexp-ja
  (let ((names '("gnus-ja" "message-ja"))
	regexp name)
    (while (setq name (pop names))
      (setq regexp (concat regexp "^" name "\\.info\\(-[0-9]+\\)?$"
			   (when names "\\|"))))
    regexp)
  "Regexp matching Japanese info files.")

(defun dgnushack-remove-extra-files-in-package ()
  "Remove extra files in the lisp directory of the XEmacs package."
  (let ((lisp-dir (expand-file-name (concat "lisp/"
					    ;; GNUS_PRODUCT_NAME
					    (cadr command-line-args-left)
					    "/")
				    ;; PACKAGEDIR
				    (car command-line-args-left))))
    (when (file-directory-p lisp-dir)
      (let (files)
	(dolist (file dgnushack-exporting-files)
	  (setq files (nconc files (list file (concat file "c")))))
	(dolist (file (directory-files lisp-dir nil nil t t))
	  (unless (member file files)
	    (setq file (expand-file-name file lisp-dir))
	    (message "Removing %s..." file)
	    (condition-case nil
		(delete-file file)
	      (error nil))))))))

(defun dgnushack-install-package-manifest ()
  "Install MANIFEST file as an XEmacs package."
  (let* ((package-dir (car command-line-args-left))
	 (product-name (cadr command-line-args-left))
	 (pkginfo-dir (expand-file-name "pkginfo" package-dir))
	 (name (expand-file-name (concat "MANIFEST." product-name)
				 pkginfo-dir))
	 make-backup-files)
    (unless (file-directory-p pkginfo-dir)
      (message "Creating directory %s/..." pkginfo-dir)
      (make-directory pkginfo-dir))
    (message "Generating %s..." name)
    (with-temp-file name
      (insert "pkginfo/MANIFEST." product-name "\n")
      (let ((lisp-dir (concat "lisp/" product-name "/"))
	    (files (sort (directory-files "." nil "\\.elc?$" t) 'string-lessp))
	    file)
	(while (setq file (pop files))
	  (unless (member file dgnushack-unexporting-files)
	    (insert lisp-dir file "\n")))
	(setq files
	      (sort (directory-files "../texi/" nil
				     (concat dgnushack-info-file-regexp-en
					     "\\|"
					     dgnushack-info-file-regexp-ja)
				     t)
		    'string-lessp))
	(while (setq file (pop files))
	  (insert "info/" file "\n"))))))


(define-compiler-macro describe-key-briefly (&whole form key &optional insert)
  (if (condition-case nil
	  (progn
	    (describe-key-briefly '((())) nil)
	    t)
	(wrong-number-of-arguments nil);; Old Emacsen.
	(error t))
      form
    (if insert
	`(if ,insert
	     (insert (funcall 'describe-key-briefly ,key))
	   (funcall 'describe-key-briefly ,key))
      `(funcall 'describe-key-briefly ,key))))

;;; dgnushack.el ends here

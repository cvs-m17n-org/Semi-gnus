;;; dgnushack.el --- a hack to set the load path for byte-compiling
;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
;; 2004, 2005
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
      ((featurep 'mule)
       (if (memq 'coding-category-sjis coding-category-list)
	   (set-coding-priority
	    (append (delq 'coding-category-sjis
			  (copy-sequence coding-category-list))
		    '(coding-category-sjis))))))

(defvar dgnushack-default-load-path (copy-sequence load-path))

(defalias 'facep 'ignore)

(require 'cl)

(unless (and
	 ;; `dolist' might not be available because of ``-no-autoloads''.
	 (fboundp 'dolist)
	 ;; It may have been defined in egg.el.
	 (dolist (var nil t)))
  (load "cl-macs" nil t))

(defvar srcdir (or (getenv "srcdir") "."))
(defvar loaddir (and load-file-name (file-name-directory load-file-name)))

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

(when (and (not (featurep 'xemacs))
	   (= emacs-major-version 21)
	   (>= emacs-minor-version 3)
	   (condition-case code
	       (let ((byte-compile-error-on-warn t))
		 (byte-optimize-form (quote (pop x)) t)
		 nil)
	     (error (string-match "called for effect"
				  (error-message-string code)))))
  (defadvice byte-optimize-form-code-walker (around silence-warn-for-pop
						    (form for-effect)
						    activate)
    "Silence the warning \"...called for effect\" for the `pop' form.
It is effective only when the `pop' macro is defined by cl.el rather
than subr.el."
    (let (tmp)
      (if (and (eq (car-safe form) 'car)
	       for-effect
	       (setq tmp (get 'car 'side-effect-free))
	       (not byte-compile-delete-errors)
	       (not (eq tmp 'error-free))
	       (eq (car-safe (cadr form)) 'prog1)
	       (let ((var (cadr (cadr form)))
		     (last (nth 2 (cadr form))))
		 (and (symbolp var)
		      (null (nthcdr 3 (cadr form)))
		      (eq (car-safe last) 'setq)
		      (eq (cadr last) var)
		      (eq (car-safe (nth 2 last)) 'cdr)
		      (eq (cadr (nth 2 last)) var))))
	  (progn
	    (put 'car 'side-effect-free 'error-free)
	    (unwind-protect
		ad-do-it
	      (put 'car 'side-effect-free tmp)))
	ad-do-it))))

(when (and (not (featurep 'xemacs))
	   (byte-optimize-form '(and (> 0 1) foo) t))
  (defadvice byte-optimize-form-code-walker
    (around fix-bug-in-and/or-forms (form for-effect) activate)
    "Optimize the rest of the and/or forms.
It has been fixed in XEmacs before releasing 21.4 and also has been
fixed in Emacs after 21.3."
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

;; Add `configure-package-path' to `load-path' for XEmacs.  Those paths
;; won't appear in `load-path' when XEmacs starts with the `-no-autoloads'
;; option or the `-vanilla' option because of a bug. :<
(when (and (featurep 'xemacs)
	   (boundp 'configure-package-path)
	   (listp configure-package-path))
  (let ((paths
	 (apply 'nconc
		(mapcar
		 (lambda (path)
		   (when (and (stringp path)
			      (not (string-equal path ""))
			      (file-directory-p
			       (setq path (expand-file-name "lisp" path))))
		     (directory-files path t)))
		 configure-package-path)))
	path adds)
    (while paths
      (setq path (car paths)
	    paths (cdr paths))
      (when (and path
		 (not (or (string-match "/\\.\\.?\\'" path)
			  (member (file-name-as-directory path) load-path)
			  (member path load-path)))
		 (file-directory-p path))
	(push (file-name-as-directory path) adds)))
    (setq load-path (nconc (nreverse adds) load-path))))

(if (file-exists-p (expand-file-name "dgnuspath.el" srcdir))
    (load (expand-file-name "dgnuspath.el" srcdir) nil nil t))

(condition-case err
    (load "~/.lpath.el" t nil t)
  (error (message "Error in \"~/.lpath.el\" file: %s" err)))

(when (featurep 'xemacs)
  (condition-case nil
      (require 'timer-funcs)
    (error "
You should upgrade your XEmacs packages, especially xemacs-base.\n"))

  ;; The reason that to load `advice' is necessary is:
  ;; 1. `path-util' loads poe.elc.
  ;; 2. poe.elc requires the `ad-add-advice' function which is expanded
  ;;    from `defadvice'.
  ;; 3. XEmacs is running with the -no-autoloads option.
  (require 'advice))

;; Don't load path-util until `char-after' and `char-before' have been
;; optimized because it requires `poe' and then modify the functions.
(condition-case nil
    (require 'path-util)
  (error "\nIn %s,
APEL was not found or an error occurred.  You will need to run the
configure script again adding the --with-addpath=APEL_PATH option.\n"
	 load-path))

(unless (locate-library "mel")
  (add-path "flim"))
(unless (module-installed-p 'mel)
  ;; FLIM 1.14 may have installed in two "flim" subdirectories.
  (push (expand-file-name "flim"
			  (file-name-directory (get-latest-path "^apel$" t)))
	load-path)
  (unless (module-installed-p 'mel)
    (error "In %s,
FLIM was not found.  You will need to run the configure script again
adding the --with-addpath=FLIM_PATH option.\n"
	   load-path)))
(add-path "semi")

;; Work around for an incompatibility (XEmacs 21.4 vs. 21.5), see the
;; following threads:
;;
;; http://thread.gmane.org/gmane.emacs.gnus.general/56414
;; Subject: attachment problems found but not fixed
;;
;; http://thread.gmane.org/gmane.emacs.gnus.general/56459
;; Subject: Splitting mail -- XEmacs 21.4 vs 21.5
;;
;; http://thread.gmane.org/gmane.emacs.xemacs.beta/20519
;; Subject: XEmacs 21.5 and Gnus fancy splitting.
(when (and (featurep 'xemacs)
	   (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
	     (modify-syntax-entry ?= " " table)
	     (with-temp-buffer
	       (with-syntax-table table
		 (insert "foo=bar")
		 (goto-char (point-min))
		 (forward-sexp 1)
		 (eolp)))))
  ;; The original `with-syntax-table' uses `copy-syntax-table' which
  ;; doesn't seem to copy modified syntax entries in XEmacs 21.5.
  (defmacro with-syntax-table (syntab &rest body)
    "Evaluate BODY with the SYNTAB as the current syntax table."
    `(let ((stab (syntax-table)))
       (unwind-protect
	   (progn
	     ;;(set-syntax-table (copy-syntax-table ,syntab))
	     (set-syntax-table ,syntab)
	     ,@body)
	 (set-syntax-table stab)))))

(push srcdir load-path)
(push loaddir load-path)
(load (expand-file-name "lpath.el" loaddir) nil t)

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

;; Unknown variables and functions.
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
    (unless (fboundp 'defadvice)
      (autoload 'defadvice "advice" nil nil 'macro))
    (autoload 'Info-directory "info" nil t)
    (autoload 'Info-menu "info" nil t)
    (autoload 'annotations-at "annotations")
    (autoload 'apropos "apropos" nil t)
    (autoload 'apropos-command "apropos" nil t)
    (autoload 'bbdb-complete-name "bbdb-com" nil t)
    (autoload 'browse-url "browse-url" nil t)
    (autoload 'c-mode "cc-mode" nil t)
    (autoload 'customize-apropos "cus-edit" nil t)
    (autoload 'customize-group "cus-edit" nil t)
    (autoload 'customize-save-variable "cus-edit" nil t)
    (autoload 'customize-set-variable "cus-edit" nil t)
    (autoload 'customize-variable "cus-edit" nil t)
    (autoload 'delete-annotation "annotations")
    (autoload 'dolist "cl-macs" nil nil 'macro)
    (autoload 'enriched-decode "enriched")
    (autoload 'executable-find "executable")
    (autoload 'font-lock-fontify-buffer "font-lock" nil t)
    (autoload 'info "info" nil t)
    (autoload 'mail-extract-address-components "mail-extr")
    (autoload 'mail-fetch-field "mail-utils")
    (autoload 'make-annotation "annotations")
    (autoload 'make-display-table "disp-table")
    (autoload 'pp "pp")
    (autoload 'ps-despool "ps-print" nil t)
    (autoload 'ps-spool-buffer "ps-print" nil t)
    (autoload 'ps-spool-buffer-with-faces "ps-print" nil t)
    (autoload 'read-passwd "passwd")
    (autoload 'regexp-opt "regexp-opt")
    (autoload 'reporter-submit-bug-report "reporter")
    (if (and (emacs-version>= 21 5)
	     (not (featurep 'sxemacs)))
	(autoload 'setenv "process" nil t)
      (autoload 'setenv "env" nil t))
    (autoload 'sgml-mode "psgml" nil t)
    (autoload 'sha1 "sha1")
    (autoload 'sha1-binary "sha1")
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
    (defalias 'w3-coding-system-for-mime-charset 'ignore)))

;; T-gnus stuff.
(eval-and-compile
  (when (featurep 'xemacs)
    (autoload 'c-mode "cc-mode" nil t)
    (autoload 'font-lock-mode "font-lock" nil t)
    (autoload 'read-kbd-macro "edmacro" nil t)
    (autoload 'turn-on-font-lock "font-lock" nil t))
  (autoload 'nnheader-detect-coding-region "nnheader")
  (autoload 'std11-extract-addresses-components "nnheader")
  (autoload 'std11-fold-region "nnheader")
  (autoload 'std11-narrow-to-field "nnheader")
  (autoload 'std11-unfold-region "nnheader"))

(defconst dgnushack-unexporting-files
  (append '("dgnushack.el" "dgnuspath.el" "lpath.el" "legacy-gnus-agent.el")
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
	  (when (and (fboundp 'base64-decode-string)
		     (subrp (symbol-function 'base64-decode-string)))
	    '("base64.el"))
	  (when (and (fboundp 'md5) (subrp (symbol-function 'md5)))
	    '("md5.el")))
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
  '("gnus-load.el"
    "mm-bodies.el" "mm-decode.el" "mm-encode.el" "mm-extern.el"
    "mm-partial.el" "mm-uu.el" "mm-view.el" "mml-sec.el" "mml-smime.el"
    "mml.el" "mml1991.el" "mml2015.el")
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

(defvar dgnushack-gnus-load-file
  (if (featurep 'xemacs)
      (expand-file-name "auto-autoloads.el" srcdir)
    (expand-file-name "gnus-load.el" srcdir)))

(defvar	dgnushack-cus-load-file
  (if (featurep 'xemacs)
      (expand-file-name "custom-load.el" srcdir)
    (expand-file-name "cus-load.el" srcdir)))

(defun dgnushack-make-cus-load ()
  (load "cus-dep")
  (let ((cusload-base-file dgnushack-cus-load-file))
    (if (fboundp 'custom-make-dependencies)
	(custom-make-dependencies)
      (Custom-make-dependencies))
    (when (featurep 'xemacs)
      (message "Compiling %s..." dgnushack-cus-load-file)
      (byte-compile-file dgnushack-cus-load-file))))

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
  (unless (featurep 'xemacs)
    (message "Generating %s..." dgnushack-gnus-load-file)
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
      ))
  (message "Compiling %s..." dgnushack-gnus-load-file)
  (byte-compile-file dgnushack-gnus-load-file)
  (when (featurep 'xemacs)
    (message "Creating dummy gnus-load.el...")
    (with-temp-file (expand-file-name "gnus-load.el")
      (insert "\

\(provide 'gnus-load)

;;; Local Variables:
;;; version-control: never
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:
;;; gnus-load.el ends here"))))


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
    (setq command-line-args-left nil)
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
  (let* ((package-dir (pop command-line-args-left))
	 (product-name (pop command-line-args-left))
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
	  (insert "info/" file "\n"))
	(insert "etc/gnus-tut.txt\n")
	(setq files
	      (sort (directory-files "../etc/images/gnus/" nil
				     "\\.\\(pbm\\|xbm\\|xpm\\)\\'"
				     t)
		    'string-lessp))
	(while (setq file (pop files))
	  (insert "etc/images/gnus/" file "\n"))
	(insert "etc/images/gnus/x-splash\n")
	(setq files
	      (sort (directory-files "../etc/images/smilies/" nil
				     "\\.\\(pbm\\|xpm\\)\\'"
				     t)
		    'string-lessp))
	(while (setq file (pop files))
	  (insert "etc/images/smilies/" file "\n"))))))

(defun dgnushack-find-lisp-shadows (&optional lispdir)
  "Return a list of directories in which other Gnus installations exist.
This function looks for the other Gnus installations which will shadow
the new Gnus Lisp modules which have been installed in LISPDIR, using
the default `load-path'.  The return value will make sense only when
LISPDIR is existent and is listed in the default `load-path'.  Assume
LISPDIR will be prepended to `load-path' by a user if the default
`load-path' does not contain it."
  (unless lispdir
    (setq lispdir (getenv "lispdir")))
  (when (and lispdir (file-directory-p lispdir))
    (setq lispdir (file-truename (directory-file-name lispdir)))
    (let ((indices '("gnus.elc" "gnus.el" "gnus.el.bz2" "gnus.el.gz"
		     "message.elc" "message.el" "message.el.bz2"
		     "message.el.gz"))
	  (path (delq nil (mapcar
			   (lambda (p)
			     (condition-case nil
				 (when (and p (file-directory-p p))
				   (file-truename (directory-file-name p)))
			       (error nil)))
			   dgnushack-default-load-path)))
	  rest elcs)
      (while path
	(setq rest (cons (car path) rest)
	      path (delete (car rest) (cdr path))))
      (setq path (nreverse (cdr (member lispdir rest)))
	    rest nil)
      (while path
	(setq elcs indices)
	(while elcs
	  (when (file-exists-p (expand-file-name (pop elcs) (car path)))
	    (setq rest (cons (car path) rest)
		  elcs nil)))
	(setq path (cdr path)))
      (prog1
	  (setq path (nreverse rest))
	(when path
	  (let (print-level print-length)
	    (princ (concat "\n\
WARNING: The other gnus installation" (if (cdr path) "s have" " has") "\
 been detected in:\n\n  " (mapconcat 'identity path "\n  ") "\n\n\
You will need to modify the run-time `load-path', remove them manually,
or remove them using `make remove-installed-shadows'.\n\n"))))))))

(defun dgnushack-remove-lisp-shadows (&optional lispdir)
  "Remove the other Gnus installations which shadow the recent one."
  (let ((path (with-temp-buffer
		(let ((standard-output (current-buffer)))
		  (dgnushack-find-lisp-shadows lispdir))))
	elcs files shadows file)
    (when path
      (unless (setq elcs (directory-files srcdir nil "\\.elc\\'"))
	(error "You should build .elc files first."))
      (setq files
	    (apply
	     'append
	     (mapcar
	      (lambda (el)
		(list (concat el "c") el (concat el ".bz2") (concat el ".gz")))
	      (append
	       (list (file-name-nondirectory dgnushack-gnus-load-file)
		     (file-name-nondirectory dgnushack-cus-load-file))
	       (mapcar (lambda (elc) (substring elc 0 -1)) elcs)))))
      (while path
	(setq shadows files)
	(while shadows
	  (setq file (expand-file-name (pop shadows) (car path)))
	  (when (file-exists-p file)
	    (princ (concat "  Removing " file "..."))
	    (condition-case nil
		(progn
		  (delete-file file)
		  (princ "done\n"))
	      (error (princ "failed\n")))))
	(setq path (cdr path))))))

;;; dgnushack.el ends here

;;; dgnushack.el --- a hack to set the load path for byte-compiling
;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000
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

(defvar srcdir (or (getenv "srcdir") "."))

(push (or (getenv "W3DIR") (expand-file-name "../../w3/lisp/" srcdir))
      load-path)
(load (expand-file-name "dgnuspath.el" srcdir) nil nil t)

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

(unless (fboundp 'si:byte-optimize-form-code-walker)
  (byte-optimize-form nil);; Load `byte-opt' or `byte-optimize'.
  (setq max-specpdl-size 3000)
  (defalias 'si:byte-optimize-form-code-walker
    (symbol-function 'byte-optimize-form-code-walker))
  (defun byte-optimize-form-code-walker (form for-effect)
    (if (and for-effect (memq (car-safe form) '(and or)))
	;; Fix bug in and/or forms.
	(let ((fn (car form))
	      (backwards (reverse (cdr form))))
	  (while (and backwards
		      (null (setcar backwards
				    (byte-optimize-form (car backwards) t))))
	    (setq backwards (cdr backwards)))
	  (if (and (cdr form) (null backwards))
	      (byte-compile-log
	       "  all subforms of %s called for effect; deleted" form))
	  (if backwards
	      (let ((head backwards))
		(while (setq backwards (cdr backwards))
		  (setcar backwards (byte-optimize-form (car backwards)
							nil)))
		(cons fn (nreverse head)))))
      (si:byte-optimize-form-code-walker form for-effect)))
  (byte-compile 'byte-optimize-form-code-walker))

(load (expand-file-name "gnus-clfns.el" srcdir) nil t t)

(condition-case err
    (load "~/.lpath.el" t nil t)
  (error (message "Error in \"~/.lpath.el\" file: %s" err)))

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

(push srcdir load-path)

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
   (dolist (keyword '(:button-keymap :data :file :mime-handle
				     :key-type :value-type))
     (set keyword keyword))))

;; Unknown variables and functions.
(unless (boundp 'buffer-file-coding-system)
  (defvar buffer-file-coding-system (symbol-value 'file-coding-system)))
(unless (featurep 'xemacs)
  (defalias 'Custom-make-dependencies 'ignore)
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

(defconst dgnushack-tool-files
  '("dgnushack.el" "dgnuspath.el" "lpath.el" "ptexinfmt.el"))
(defconst dgnushack-unexported-files
  '("dgnuspath.el" "ptexinfmt.el"))

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
  (let ((files (directory-files srcdir nil "^[^=].*\\.el$")))
    (dolist (file
	     (append
	      dgnushack-tool-files
	      (condition-case nil
		  (progn (require 'w3-forms) nil)
		(error '("nnweb.el" "nnlistserv.el" "nnultimate.el"
			 "nnslashdot.el" "nnwarchive.el" "webmail.el")))
	      (condition-case nil
		  (progn (require 'bbdb) nil)
		(error '("gnus-bbdb.el")))
	      (unless (featurep 'xemacs)
		'("gnus-xmas.el" "gnus-picon.el" "messagexmas.el"
		  "nnheaderxm.el" "smiley.el"))
	      (when (or (featurep 'xemacs) (<= emacs-major-version 20))
		'("smiley-ems.el"))
	      (when (and (fboundp 'md5) (subrp (symbol-function 'md5)))
		'("md5.el"))))
      (setq files (delete file files)))

    (dolist (file files)
      (setq file (expand-file-name file srcdir))
      (when (and (file-exists-p (setq elc (concat file "c")))
		 (file-newer-than-file-p file elc))
	(delete-file elc)))

    (let (;;(byte-compile-generate-call-tree t)
	  file elc)
      (while (setq file (pop files))
	(setq file (expand-file-name file srcdir))
	(when (or (not (file-exists-p (setq elc (concat file "c"))))
		  (file-newer-than-file-p file elc))
	  (ignore-errors
	    (byte-compile-file file)))))))

(defun dgnushack-recompile ()
  (require 'gnus)
  (byte-recompile-directory "." 0))


(defun dgnushack-texi-add-suffix-and-format ()
  (dgnushack-texi-format t))

(defun dgnushack-texi-format (&optional addsuffix)
  (if (not noninteractive)
      (error "batch-texinfo-format may only be used -batch."))
  (require 'ptexinfmt)
  (let ((auto-save-default nil)
	(find-file-run-dired nil)
	coding-system-for-write
	output-coding-system)
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
      (while (setq file (pop files))
	(condition-case err
	    (progn
	      (if buffer-file-name (kill-buffer (current-buffer)))
	      (find-file file)
	      (buffer-disable-undo (current-buffer))
	      (if (boundp 'MULE)
		  (setq output-coding-system (symbol-value
					      'file-coding-system))
		(setq coding-system-for-write buffer-file-coding-system))
	      ;; Remove ignored areas first.
	      (while (re-search-forward "^@ignore[\t\r ]*$" nil t)
		(delete-region (match-beginning 0)
			       (if (re-search-forward
				    "^@end[\t ]+ignore[\t\r ]*$" nil t)
				   (1+ (match-end 0))
				 (point-max))))
	      (goto-char (point-min))
	      ;; Add suffix if it is needed.
	      (when (and addsuffix
			 (re-search-forward
			  "^@setfilename[\t ]+\\([^\t\n ]+\\)" nil t)
			 (not (string-match "\\.info$" (match-string 1))))
		(insert ".info")
		(goto-char (point-min)))
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

(defun dgnushack-make-autoloads ()
  "Make auto-autoloads.el, custom-load.el and then compile them."
  (let (make-backup-files)
    (message "Updating autoloads for directory %s..." default-directory)
    (let ((generated-autoload-file "auto-autoloads.el")
	  (si:message (symbol-function 'message))
	  noninteractive)
      (defun message (fmt &rest args)
	(cond ((and (string-equal "Generating autoloads for %s..." fmt)
		    (file-exists-p (file-name-nondirectory (car args))))
	       (funcall si:message fmt (file-name-nondirectory (car args))))
	      ((string-equal "No autoloads found in %s" fmt))
	      ((string-equal "Generating autoloads for %s...done" fmt))
	      (t (apply si:message fmt args))))
      (unwind-protect
	  (update-autoloads-from-directory default-directory)
	(fset 'message si:message)))
    (byte-compile-file "auto-autoloads.el")
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
	(Custom-make-dependencies "."))
      (message "%s" (buffer-string)))
    (require 'cus-load)
    (byte-compile-file "custom-load.el")))

(defun dgnushack-examine-package-dir ()
  "Examine PACKAGEDIR."
  (let ((package-dir (car command-line-args-left)))
    (unless package-dir
      (let (dirs)
	(when (boundp 'early-packages)
	  (setq dirs (delq nil (append (when early-package-load-path
					 early-packages)
				       (when late-package-load-path
					 late-packages)
				       (when last-package-load-path
					 last-packages))))
	  (while (and dirs (not package-dir))
	    (when (file-directory-p (car dirs))
	      (setq package-dir (car dirs)
		    dirs (cdr dirs)))))))
    (or package-dir
	(error "%s" "
You must specify the name of the package path as follows:

% make install-package PACKAGEDIR=/usr/local/lib/xemacs/xemacs-packages/"))))

(defun dgnushack-gnus-product-name ()
  "Return a product name of this gnus."
  (require 'gnus)
  (downcase gnus-product-name))

(defun dgnushack-make-manifest (product-name)
  "Make MANIFEST file for XEmacs package."
  (let (make-backup-files)
    (message "Generating MANIFEST.%s for the package..." product-name)
    (with-temp-file (concat "../MANIFEST." product-name)
      (insert "pkginfo/MANIFEST." product-name "\n")
      (let ((lisp-dir (concat "lisp/" product-name "/"))
	    (files (sort (directory-files "." nil "\\.elc?$") 'string-lessp))
	    file)
	(while (setq file (pop files))
	  (unless (member file dgnushack-unexported-files)
	    (insert lisp-dir file "\n")))
	(setq files
	      (sort (directory-files "../texi/" nil
				     (concat dgnushack-info-file-regexp-en
					     "\\|"
					     dgnushack-info-file-regexp-ja))
		    'string-lessp))
	(while (setq file (pop files))
	  (insert "info/" file "\n"))))))

(defun dgnushack-install-package-info-files (package-dir regexp)
  "Install info files as an XEmacs package."
  (let ((files (sort (directory-files "../texi/" nil regexp) 'string-lessp))
	(info-dir (expand-file-name "info/" package-dir))
	file)
    (unless (file-directory-p info-dir)
      (make-directory info-dir))
    (while (setq file (pop files))
      (message "Copying ../texi/%s to %s..." file info-dir)
      (copy-file (expand-file-name file "../texi/")
		 (expand-file-name file info-dir)
		 t t))))

(defun dgnushack-install-package-pkginfo (package-dir product-name)
  "Install MANIFEST file."
  (let ((manifest (concat "MANIFEST." product-name))
	(pkginfo-dir (expand-file-name "pkginfo/" package-dir)))
    (unless (file-directory-p pkginfo-dir)
      (make-directory pkginfo-dir))
    (message "Copying ../%s to %s..." manifest pkginfo-dir)
    (copy-file (expand-file-name manifest "../")
	       (expand-file-name manifest pkginfo-dir) t t)))

(defun dgnushack-install-package-lick ()
  "Install lisp files and MANIFEST file as an XEmacs package."
  (dgnushack-make-autoloads)
  (let ((package-dir (dgnushack-examine-package-dir))
	(product-name (dgnushack-gnus-product-name)))
    (dgnushack-make-manifest product-name)
    ;; Install lisp files.
    (let ((lisp-dir (expand-file-name (concat "lisp/" product-name "/")
				      package-dir))
	  (files (sort (directory-files "." nil "\\.elc?$") 'string-lessp)))
      (unless (file-directory-p lisp-dir)
	(make-directory lisp-dir t))
      (dolist (file dgnushack-unexported-files)
	(setq files (delete file files)))
      ;; Remove extra files.
      (dolist (file (directory-files lisp-dir nil nil nil t))
	(unless (or (member file files)
		    (not (string-match "\\.elc?$" file)))
	  (setq file (expand-file-name file lisp-dir))
	  (message "Removing %s..." file)
	  (condition-case nil
	      (delete-file file)
	    (error nil))))
      (while (setq file (pop files))
	(message "Copying %s to %s..." file lisp-dir)
	(copy-file file (expand-file-name file lisp-dir) t t)))
    (dgnushack-install-package-pkginfo package-dir product-name)))

(defun dgnushack-install-package-info ()
  "Install English info files and MANIFEST file as an XEmacs package."
  (let ((package-dir (dgnushack-examine-package-dir))
	(product-name (dgnushack-gnus-product-name)))
    (dgnushack-make-manifest product-name)
    (dgnushack-install-package-info-files package-dir
					  dgnushack-info-file-regexp-en)
    (dgnushack-install-package-pkginfo package-dir product-name)))

(defun dgnushack-install-package-info-ja ()
  "Install Japanese info files and MANIFEST file as an XEmacs package."
  (let ((package-dir (dgnushack-examine-package-dir))
	(product-name (dgnushack-gnus-product-name)))
    (dgnushack-make-manifest product-name)
    (dgnushack-install-package-info-files package-dir
					  dgnushack-info-file-regexp-ja)
    (dgnushack-install-package-pkginfo package-dir product-name)))

(defun dgnushack-compose-package ()
  "Make auto-autoloads.el(c), custom-load.el(c) and MANIFEST file."
  (dgnushack-make-autoloads)
  (dgnushack-make-manifest (dgnushack-gnus-product-name)))

;;; dgnushack.el ends here

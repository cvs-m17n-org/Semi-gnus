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

(fset 'facep 'ignore)

(require 'cl)
(require 'bytecomp)
(push "~/lisp/custom" load-path)
(push "." load-path)
(load "./lpath.el" nil t)

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
  (let ((files (directory-files "." nil "^[^=].*\\.el$"))
	(xemacs (string-match "XEmacs" emacs-version))
	;;(byte-compile-generate-call-tree t)
	file elc)
    (condition-case ()
	(require 'w3-forms)
      (error (setq files (delete "nnweb.el" (delete "nnlistserv.el" files)))))
    (condition-case ()
	(require 'bbdb)
      (error (setq files (delete "gnus-bbdb.el" files))))
    (while (setq file (pop files))
      (when (or (and (not xemacs)
		     (not (member file '("gnus-xmas.el" "gnus-picon.el"
					 "messagexmas.el" "nnheaderxm.el"
					 "smiley.el" "x-overlay.el"))))
		(and xemacs
		     (not (member file '("md5.el")))))
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
  "^\\(gnus\\|message\\|gnus-ja\\|message-ja\\)\\.info\\(-[0-9]+\\)?$")

(defconst dgnushack-texi-file-regexp
  "^\\(gnus\\|message\\|gnus-ja\\|message-ja\\)\\.texi$")

(defun dgnushack-make-package ()
  (require 'gnus)
  (let* ((product-name (downcase gnus-product-name))
	 (lisp-dir (concat "lisp/" product-name "/"))
	 make-backup-files)

    (message "Updating autoloads for directory %s..." default-directory)
    (let ((generated-autoload-file "auto-autoloads.el")
	  noninteractive)
      (update-autoloads-from-directory default-directory))
    (byte-compile-file "auto-autoloads.el")

    (with-temp-buffer
      (let ((standard-output (current-buffer)))
	(Custom-make-dependencies "."))
      (message (buffer-substring (point-min) (point-max))))
    (require 'cus-load)
    (byte-compile-file "custom-load.el")

    (message "Generating MANIFEST.%s for the package..." product-name)
    (with-temp-buffer
      (insert "pkginfo/MANIFEST." product-name "\n"
	      lisp-dir
	      (mapconcat
	       'identity
	       (sort (directory-files "." nil "\\.elc?$")
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

    (setq files (sort (directory-files "." nil "\\.elc?$") 'string-lessp))
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

(defun dgnushack-add-info-suffix-maybe ()
  ;; This function must be invoked from lisp directory.
  (setq default-directory "../texi/")
  (let ((coding-system-for-read 'raw-text)
	(coding-system-for-write 'raw-text)
	(files (directory-files "." nil dgnushack-texi-file-regexp))
	file make-backup-files)
    (while (setq file (pop files))
      (find-file file)
      (when (and (re-search-forward
		  "^@setfilename[\t ]+\\([^\t\n ]+\\)" nil t)
		 (not (string-match "\\.info$" (match-string 1))))
	(copy-file file (concat file "_") nil t)
	(insert ".info")
	(save-buffer))
      (kill-buffer (current-buffer)))))

;;; dgnushack.el ends here

;;; infohack.el --- a hack to format info file.
;; Copyright (C)  2001  Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: info

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

(require 'texinfmt)

(defun infohack-remove-unsupported ()
  (goto-char (point-min))
  (while (re-search-forward "@\\(end \\)?ifnottex" nil t) 
    (replace-match "")))

(defun infohack (file)
  (let ((dest-directory default-directory)
	(max-lisp-eval-depth (max max-lisp-eval-depth 600)))
    (find-file file)
    (infohack-remove-unsupported)
    (texinfo-every-node-update) 
    (texinfo-format-buffer t) ;; Don't save any file.
    (setq default-directory dest-directory)
    (setq buffer-file-name 
	  (expand-file-name (file-name-nondirectory buffer-file-name)
			    default-directory))
    (if (> (buffer-size) 100000)
	(Info-split))
    (save-buffer)))

(defun batch-makeinfo ()
  "Emacs makeinfo in batch mode."
  (infohack-texi-format (car command-line-args-left)
			(car (cdr command-line-args-left)))
  (setq command-line-args-left nil))


(let ((default-directory (expand-file-name "../lisp/"))
      (features (cons 'w3-forms (copy-sequence features))))
  ;; Adjust `load-path' for APEL.
  (load-file "dgnushack.el"))
(load-file (expand-file-name "ptexinfmt.el" "./"))

(defun infohack-texi-format (file &optional addsuffix)
  (let ((auto-save-default nil)
	(find-file-run-dired nil)
	coding-system-for-write
	output-coding-system
	(error 0))
    (condition-case err
	(progn
	  (find-file file)
	  (buffer-disable-undo (current-buffer))
	  (if (boundp 'MULE)
	      (setq output-coding-system file-coding-system)
	    (setq coding-system-for-write buffer-file-coding-system))
	  ;; Remove ignored areas first.
	  (while (re-search-forward "^@ignore[\t\r ]*$" nil t)
	    (delete-region (match-beginning 0)
			   (if (re-search-forward
				"^@end[\t ]+ignore[\t\r ]*$" nil t)
			       (1+ (match-end 0))
			     (point-max))))
	  (infohack-remove-unsupported)
	  (goto-char (point-min))
	  ;; Add suffix if it is needed.
	  (when (and addsuffix
		     (re-search-forward "^@setfilename[\t ]+\\([^\t\n ]+\\)"
					nil t)
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
		     (point) (+ (point)
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
       (let ((s (buffer-substring (point) (min (+ (point) 100) (point-max))))
	     (tem 0))
	 (while (setq tem (string-match "\n+" s tem))
	   (setq s (concat (substring s 0 (match-beginning 0))
			   "\n>>  "
			   (substring s (match-end 0)))
		 tem (1+ tem)))
	 (message ">>  %s" s))
       (setq error 1)))
    (kill-emacs error)))

;;; infohack.el ends here

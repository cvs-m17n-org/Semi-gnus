;;; gnus-vm.el --- vm interface for Gnus

;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001
;;	Free Software Foundation, Inc.

;; Author: Per Persson <pp@gnu.ai.mit.edu>
;;         Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: news, mail

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

;; Major contributors:
;;	Christian Limpach <Christian.Limpach@nice.ch>
;; Some code stolen from:
;;	Rick Sladkey <jrs@world.std.com>

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus-art)

(eval-when-compile
  (autoload 'vm-mode "vm")
  (autoload 'vm-read-file-name "vm")
  (autoload 'vm-save-message "vm"))

(when (not (featurep 'vm))
  (load "vm"))

(defvar vm-folder-directory)
(defvar vm-folder-history)
(defvar vm-primary-inbox)
(defvar vm-use-toolbar)

(defun gnus-vm-make-folder (&optional buffer)
  (let ((article (or buffer (current-buffer)))
	(tmp-folder (generate-new-buffer " *tmp-folder*"))
	(start (point-min))
	(end (point-max)))
    (set-buffer tmp-folder)
    (insert-buffer-substring article start end)
    (goto-char (point-min))
    (if (looking-at "^\\(From [^ ]+ \\).*$")
	(replace-match (concat "\\1" (current-time-string)))
      (insert "From " gnus-newsgroup-name " "
	      (current-time-string) "\n"))
    (while (re-search-forward "\n\nFrom " nil t)
      (replace-match "\n\n>From "))
    ;; insert a newline, otherwise the last line gets lost
    (goto-char (point-max))
    (insert "\n")
    (let (mime-display-header-hook
	  mime-display-text/plain-hook mime-text-decode-hook
	  mime-view-define-keymap-hook mime-view-mode-hook)
      (vm-mode))
    tmp-folder))

(defvar gnus-summary-save-article-vm-folder nil)
(defvar gnus-summary-save-article-vm-count nil)

(defun gnus-summary-save-article-vm (&optional arg folder)
  "Append the current article to a vm folder.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive
   (let ((prefix-arg current-prefix-arg)
	 articles marks default-folder)
     (setq default-folder (or (car vm-folder-history) vm-primary-inbox))
     (if (numberp prefix-arg)
	 (setq articles prefix-arg)
       (setq marks (delq nil (gnus-summary-work-articles nil))
	     articles (length marks)))
     (list
      prefix-arg
      (unless (zerop articles)
	(vm-read-file-name
	 (format
	  "Save %s in VM folder: "
	  (cond ((eq 1 articles)
		 (if (or (not marks) (eq gnus-current-article (car marks)))
		     "this article"
		   "the marked article"))
		((< 0 articles)
		 (if marks
		     (format "the marked %d articles" articles)
		   (format "the %d next articles" articles)))
		((> 0 articles)
		 (format "the %d previous articles" (- articles)))))
	 (if default-folder "" vm-folder-directory)
	 nil nil default-folder 'vm-folder-history)))))
  (if (interactive-p)
      (unless folder
	(error "No articles to be saved"))
    (unless (setq folder (or folder gnus-summary-save-article-vm-folder))
      (error "No VM folder is specified")))
  (unwind-protect
      (progn
	(setq gnus-summary-save-article-vm-folder folder
	      gnus-summary-save-article-vm-count 0)
	(let ((gnus-default-article-saver 'gnus-summary-save-in-vm)
	      mime-display-header-hook mime-display-text/plain-hook
	      mime-text-decode-hook mime-view-define-keymap-hook
	      mime-view-mode-hook)
	  (gnus-summary-save-article arg))
	(cond ((eq 1 gnus-summary-save-article-vm-count)
	       (message "One article is saved in %s" folder))
	      ((< 0 gnus-summary-save-article-vm-count)
	       (message "%d articles are saved in %s"
			gnus-summary-save-article-vm-count folder))
	      (t
	       (message "Maybe no articles are saved in %s" folder))))
    (setq gnus-summary-save-article-vm-folder nil
	  gnus-summary-save-article-vm-count nil)))

(defun gnus-summary-save-in-vm (&optional folder)
  (interactive
   (let (default-folder)
     (setq default-folder (or (car vm-folder-history) vm-primary-inbox))
     (list (vm-read-file-name "Save this article in VM folder: "
			      (if default-folder "" vm-folder-directory)
			      nil nil default-folder 'vm-folder-history))))
  (unless (interactive-p)
    (setq folder (or folder gnus-summary-save-article-vm-folder)))
  (unless folder
    (error "No VM folder is specified"))
  (unless (interactive-p)
    (message "Saving the article %d in %s..." gnus-current-article folder)
    (when (numberp gnus-summary-save-article-vm-count)
      (incf gnus-summary-save-article-vm-count)))
  (save-window-excursion
    (apply 'gnus-summary-select-article gnus-show-all-headers
	   (unless (interactive-p)
	     (list nil nil gnus-current-article)))
    (gnus-eval-in-buffer-window gnus-original-article-buffer
      (save-excursion
	(save-restriction
	  (widen)
	  (let* ((vm-use-toolbar nil)
		 (vm-folder (gnus-vm-make-folder)))
	    (vm-save-message folder)
	    (when (interactive-p)
	      (message "This article is saved in %s" folder))
	    (kill-buffer vm-folder)))))))

(provide 'gnus-vm)

;;; gnus-vm.el ends here

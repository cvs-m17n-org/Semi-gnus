;;; gnus-draft.el --- draft message support for Gnus
;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

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

(require 'gnus)
(require 'gnus-sum)
(require 'message)
(require 'gnus-msg)
(require 'nndraft)
(eval-when-compile (require 'cl))

;;; Draft minor mode

(defvar gnus-draft-mode nil
  "Minor mode for providing a draft summary buffers.")

(defvar gnus-draft-mode-map nil)

(unless gnus-draft-mode-map
  (setq gnus-draft-mode-map (make-sparse-keymap))

  (gnus-define-keys gnus-draft-mode-map
    "Dt" gnus-draft-toggle-sending
    "De" gnus-draft-edit-message
    "Ds" gnus-draft-send-message
    "DS" gnus-draft-send-all-messages))

(defun gnus-draft-make-menu-bar ()
  (unless (boundp 'gnus-draft-menu)
    (easy-menu-define
     gnus-draft-menu gnus-draft-mode-map ""
     '("Drafts"
       ["Toggle whether to send" gnus-draft-toggle-sending t]))))

(defun gnus-draft-mode (&optional arg)
  "Minor mode for providing a draft summary buffers.

\\{gnus-draft-mode-map}"
  (interactive "P")
  (when (eq major-mode 'gnus-summary-mode)
    (when (set (make-local-variable 'gnus-draft-mode)
		  (if (null arg) (not gnus-draft-mode)
		    (> (prefix-numeric-value arg) 0)))
      ;; Set up the menu.
      (when (gnus-visual-p 'draft-menu 'menu)
	(gnus-draft-make-menu-bar))
      (gnus-add-minor-mode 'gnus-draft-mode " Draft" gnus-draft-mode-map)
      (run-hooks 'gnus-draft-mode-hook))))

;;; Commands

(defun gnus-draft-toggle-sending (article)
  "Toggle whether to send an article or not."
  (interactive (list (gnus-summary-article-number)))
  (if (gnus-draft-article-sendable-p article)
      (progn
	(push article gnus-newsgroup-unsendable)
	(gnus-summary-mark-article article gnus-unsendable-mark))
    (setq gnus-newsgroup-unsendable
	  (delq article gnus-newsgroup-unsendable))
    (gnus-summary-mark-article article gnus-unread-mark))
  (gnus-summary-position-point))

(defun gnus-draft-edit-message ()
  "Enter a mail/post buffer to edit and send the draft."
  (interactive)
  (let ((article (gnus-summary-article-number)))
    (gnus-summary-mark-as-read article gnus-canceled-mark)
    (gnus-draft-setup article gnus-newsgroup-name)
    (push
     `((lambda ()
	 (when (buffer-name (get-buffer ,gnus-summary-buffer))
	   (save-excursion
	     (set-buffer (get-buffer ,gnus-summary-buffer))
	     (gnus-cache-possibly-remove-article ,article nil nil nil t)))))
     message-send-actions)))

(defun gnus-draft-send-message (&optional n)
  "Send the current draft."
  (interactive "P")
  (let ((articles (gnus-summary-work-articles n))
	article)
    (while (setq article (pop articles))
      (gnus-summary-remove-process-mark article)
      (unless (memq article gnus-newsgroup-unsendable)
	(gnus-draft-send article gnus-newsgroup-name)
	(gnus-summary-mark-article article gnus-canceled-mark)))))

(defun gnus-draft-send (article &optional group)
  "Send message ARTICLE."
  (gnus-draft-setup article (or group "nndraft:queue"))
  (let ((message-syntax-checks 'dont-check-for-anything-just-trust-me)
	message-send-hook)
    (message-send-and-exit)))

(defun gnus-draft-send-all-messages ()
  "Send all the sendable drafts."
  (interactive)
  (gnus-uu-mark-buffer)
  (gnus-draft-send-message))

(defun gnus-group-send-drafts ()
  "Send all sendable articles from the queue group."
  (interactive)
  (gnus-activate-group "nndraft:queue")
  (save-excursion
    (let ((articles (nndraft-articles))
	  (unsendable (gnus-uncompress-range
		       (cdr (assq 'unsend
				  (gnus-info-marks
				   (gnus-get-info "nndraft:queue"))))))
	  article)
      (while (setq article (pop articles))
	(unless (memq article unsendable)
	  (gnus-draft-send article))))))

;;; Utility functions

;;;!!!If this is byte-compiled, it fails miserably.
;;;!!!I have no idea why.

(progn
(defun gnus-draft-setup (narticle group)
  (gnus-setup-message 'forward
    (let ((article narticle))
      (message-mail)
      (erase-buffer)
      (if (not (gnus-request-restore-buffer article group))
	  (error "Couldn't restore the article")
	;; Insert the separator.
	(goto-char (point-min))
	(search-forward "\n\n")
	(forward-char -1)
	(insert mail-header-separator)
	(forward-line 1)
	(message-set-auto-save-file-name))))))

(defun gnus-draft-article-sendable-p (article)
  "Say whether ARTICLE is sendable."
  (not (memq article gnus-newsgroup-unsendable)))

(provide 'gnus-draft)

;;; gnus-draft.el ends here

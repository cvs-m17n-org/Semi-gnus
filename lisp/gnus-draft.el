;;; gnus-draft.el --- draft message support for Semi-gnus
;; Copyright (C) 1997, 1998, 1999, 2000
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;	Tatsuya Ichikawa <t-ichi@po.shiojiri.ne.jp>
;; Keywords: mail, news, MIME, offline

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
(require 'gnus-agent)
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
       ["Toggle whether to send" gnus-draft-toggle-sending t]
       ["Edit" gnus-draft-edit-message t]
       ["Send selected message(s)" gnus-draft-send-message t]
       ["Send all messages" gnus-draft-send-all-messages t]
       ["Delete draft" gnus-summary-delete-article t]))))

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
      (gnus-run-hooks 'gnus-draft-mode-hook))))

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
    (gnus-draft-setup-for-editing article gnus-newsgroup-name)
    (message-save-drafts)
    (let ((gnus-verbose-backends nil))
      (gnus-request-expire-articles (list article) gnus-newsgroup-name t))
    (push
     `((lambda ()
	 (when (gnus-buffer-exists-p ,gnus-summary-buffer)
	   (save-excursion
	     (set-buffer ,gnus-summary-buffer)
	     (gnus-cache-possibly-remove-article ,article nil nil nil t)))))
     message-send-actions)))

(defun gnus-draft-send-message (&optional n)
  "Send the current draft."
  (interactive "P")
  (let* ((articles (gnus-summary-work-articles n))
	 (total (length articles))
	 article)
    (while (setq article (pop articles))
      (gnus-summary-remove-process-mark article)
      (unless (memq article gnus-newsgroup-unsendable)
	(let ((message-sending-message 
	       (format "Sending message %d of %d..." 
		       (- total (length articles)) total)))
	  (gnus-draft-send article gnus-newsgroup-name t))
	(gnus-summary-mark-article article gnus-canceled-mark)))))

(defun gnus-draft-send (article &optional group interactive)
  "Send message ARTICLE."
  (let ((message-syntax-checks (if interactive nil
				 'dont-check-for-anything-just-trust-me))
	(message-inhibit-body-encoding (or (not group) 
					   (equal group "nndraft:queue")
					   message-inhibit-body-encoding))
	(message-send-hook (and group (not (equal group "nndraft:queue"))
				message-send-hook))
	(message-setup-hook (and group (not (equal group "nndraft:queue"))
				 message-setup-hook))
	type method)
    (gnus-draft-setup-for-sending article (or group "nndraft:queue"))
    ;; We read the meta-information that says how and where
    ;; this message is to be sent.
    (save-restriction
      (message-narrow-to-head)
      (when (re-search-forward
	     (concat "^" (regexp-quote gnus-agent-meta-information-header) ":")
	     nil t)
	(setq type (ignore-errors (read (current-buffer)))
	      method (ignore-errors (read (current-buffer))))
	(message-remove-header gnus-agent-meta-information-header)))
    ;; Let Agent restore any GCC lines and have message.el perform them.
    (gnus-agent-restore-gcc)
    ;; Then we send it.  If we have no meta-information, we just send
    ;; it and let Message figure out how.
    (when (let ((mail-header-separator ""))
	    (cond ((eq type 'news)
		   (mime-edit-maybe-split-and-send
		    (function
		     (lambda ()
		       (interactive)
		       (funcall message-send-news-function method)
		       )))
		   (funcall message-send-news-function method)
		   )
		  ((eq type 'mail)
		   (mime-edit-maybe-split-and-send
		    (function
		     (lambda ()
		       (interactive)
		       (funcall message-send-mail-function)
		       )))
		   (funcall message-send-mail-function)
		   t)))
      (let ((gnus-verbose-backends nil))
	(gnus-request-expire-articles
	 (list article) (or group "nndraft:queue") t)))))

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
    (let* ((articles (nndraft-articles))
	   (unsendable (gnus-uncompress-range
			(cdr (assq 'unsend
				   (gnus-info-marks
				    (gnus-get-info "nndraft:queue"))))))
	   (total (length articles))
	   article)
      (while (setq article (pop articles))
	(unless (memq article unsendable)
	  (let ((message-sending-message
		 (format "Sending message %d of %d..."
			 (- total (length articles)) total)))
	    (gnus-draft-send article)))))))

;;;###autoload
(defun gnus-draft-reminder ()
  "Reminder user if there are unsent drafts."
  (interactive)
  (if (gnus-alive-p)
      (let (active)
	(catch 'continue
	  (dolist (group '("nndraft:drafts" "nndraft:queue"))
	    (setq active (gnus-activate-group group))
	    (if (and active (>= (cdr active) (car active)))
		(if (y-or-n-p "There are unsent drafts. Continue?")
		    (throw 'continue t)
		  (error "Stop!"))))))))

;;; Utility functions

(defcustom gnus-draft-decoding-function
  #'mime-edit-decode-message-in-buffer
  "*Function called to decode the message from network representation."
  :group 'gnus-agent
  :type 'function)

;;;!!!If this is byte-compiled, it fails miserably.
;;;!!!This is because `gnus-setup-message' uses uninterned symbols.
;;;!!!This has been fixed in recent versions of Emacs and XEmacs,
;;;!!!but for the time being, we'll just run this tiny function uncompiled.

(defun gnus-draft-setup-for-editing (narticle group)
  (let (ga)
    (gnus-setup-message 'forward
      (let ((article narticle))
	(message-mail)
	(erase-buffer)
	(if (not (gnus-request-restore-buffer article group))
	    (error "Couldn't restore the article")
	  (funcall gnus-draft-decoding-function)
	  ;; Insert the separator.
	  (goto-char (point-min))
	  (search-forward "\n\n")
	  (forward-char -1)
	  (insert mail-header-separator)
	  (forward-line 1)
	  (setq ga (message-fetch-field gnus-draft-meta-information-header))
	  (message-set-auto-save-file-name))))
    (when (and ga
	       (ignore-errors (setq ga (car (read-from-string ga)))))
      (setq message-post-method
	    `(lambda (arg)
	       (gnus-post-method arg ,(car ga))))
      (message-add-action
       `(gnus-add-mark ,(car ga) 'replied ,(cadr ga))
       'send))))

(defvar gnus-draft-send-draft-buffer " *send draft*")
(defun gnus-draft-setup-for-sending (narticle group)
  (let ((article narticle))
    (if (not (get-buffer gnus-draft-send-draft-buffer))
	(get-buffer-create gnus-draft-send-draft-buffer))
    (set-buffer gnus-draft-send-draft-buffer)
    (erase-buffer)
    (if (not (gnus-request-restore-buffer article group))
	(error "Couldn't restore the article")
      )))

(defun gnus-draft-article-sendable-p (article)
  "Say whether ARTICLE is sendable."
  (not (memq article gnus-newsgroup-unsendable)))

(provide 'gnus-draft)

;;; gnus-draft.el ends here

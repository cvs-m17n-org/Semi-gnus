;;; gnus-ofsetup.el --- Setup advisor for Offline reading for Mail/News.
;;;
;;; Copyright (C) 1998 Tatsuya Ichikawa
;;; Author: Tatsuya Ichikawa <t-ichi@po.shiojiri.ne.jp>
;;; Author: Keiichi Suzuki <keiichi@nanap.org>
;;;
;;; This file is part of Nana-gnus.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;
;;;; Commentary:
;;; How to use.
;;;
;;;      M-x load-library[RET]gnus-ofsetup[RET]
;;;      M-x gnus-setup-for-offline[RET]
;;;

;;; Code:

(eval-when-compile
  (require 'poe))

(defvar gnus-offline-setting-file "~/.nana-gnus-offline.el")

(defun gnus-ofsetup-read-from-minibuffer (format &rest args)
  (let ((server
	 (read-from-minibuffer
	  (apply 'format
		 (concat format
			 " (if you are finished, input null string.) : ")
		 args))))
    (unless (string-match "^[ \t]*$" server)
      server)))

(defun gnus-ofsetup-completing-read-symbol (msg &rest syms)
  (intern
   (completing-read (concat msg " (TAB to completion): ")
		    (mapcar
		     (lambda (sym)
		       (list (symbol-name sym)))
		     syms)
		    nil t nil)))

(defun gnus-ofsetup-read-pop-account (server)
  (let ((account (gnus-ofsetup-read-from-minibuffer
		  "Mail account at \"%s\"" server)))
    (when account
      (let ((auth (gnus-ofsetup-completing-read-symbol 
		   "Authentification Method"
		   'pass 'apop)))
	(list (concat "po:" account "@" server) :auth-scheme auth)))))

(defun gnus-setup-for-offline ()
  "*Set up Gnus for offline environment."
  (interactive)
  (unless (file-exists-p gnus-offline-setting-file)
    (let (movemail-option
	  news-fetch-method mail-fetch-method agent-directory drafts-queue-type
	  news-spool-directory mail-spool-directory send-news-function
	  sendmail-to-spool-directory news-spool-request-post-directory
	  MTA-type dialup-program dialup-program-arguments hangup-program
	  hangup-program-arguments movemail-program
	  movemail-program-apop-option spool-file save-passwd)
      (setq news-fetch-method
	    (gnus-ofsetup-completing-read-symbol
	     "Method for offline News reading"
	     'nnagent 'nnspool))
      (when (eq news-fetch-method 'nnagent)
	(setq agent-directory
	      (read-from-minibuffer "Agent directory: " "~/News/agent")))
      (setq drafts-queue-type
	    (cond
	     ((or (eq news-fetch-method 'nnspool)
		  (y-or-n-p "Use MIEE post/send message "))
	      ;; Setting for MIEE with nnspool.
	      (setq news-spool-directory
		    (read-from-minibuffer
		     "News spool directory for sending: "
		     "/usr/spool/news.out"))
	      (setq mail-spool-directory
		    (read-from-minibuffer
		     "Mail spool directory for sending: "
		     "/usr/spool/mail.out"))
	      ;; Set news post function for MIEE.
	      (setq send-news-function 'gnspool-request-post)
	      ;; Spool directory setting - MIEE.
	      (unless (file-exists-p mail-spool-directory)
		(make-directory mail-spool-directory t))
	      (setq sendmail-to-spool-directory mail-spool-directory)
	      (unless (file-exists-p news-spool-directory)
		(make-directory news-spool-directory t))
	      (setq news-spool-request-post-directory news-spool-directory)
	      'miee)
	     (t
	      'agent)))
      (setq mail-fetch-method 'nnmail)
      (setq MTA-type (gnus-ofsetup-completing-read-symbol
		      "Select MTA type for sending mail"
		      'smtp 'sendmail))
      (setq dialup-program
	    (read-file-name
	     "Dialup program (if you do not use it, input null string): "
	     nil nil t))
      (if (string-match "^[ \t]*$" dialup-program)
	  (setq dialup-program nil)
	(setq dialup-program-arguments
	      (split-string
	       (read-from-minibuffer "Dialup program options: ")
	       "[\t ]+")))
      (setq hangup-program
	    (read-file-name
	     "Hangup program (if you do not use it, input null string): "
	     (and dialup-program
		  (file-name-directory dialup-program))
	     dialup-program
	     t))
      (if (string-match "^[ \t]*$" hangup-program)
	  (setq hangup-program nil)
	(setq hangup-program-arguments
	      (split-string
	       (read-from-minibuffer "Hangup program options: ")
	       "[\t ]+")))

      ;; Set `movemail' type.
      (setq movemail-program
	    (if (y-or-n-p "Do you use pop3.el to fetch mail? ")
		'nnmail-pop3-movemail
	      (read-file-name "movemail program name: "
			      exec-directory "movemail")))
      (when (stringp movemail-program)
	(setq movemail-option (read-from-minibuffer "movemail options: " "-f"))
	(setq movemail-program-apop-option
	      (read-from-minibuffer "movemail options for APOP: ")))
    
      ;; Set E-Mail Addresses.
      (setq spool-file nil)
      (let (server spool)
	(while (setq server (gnus-ofsetup-read-from-minibuffer "POP server"))
	  (while (setq spool (gnus-ofsetup-read-pop-account server))
	    (setq spool-file (cons spool spool-file)))))

      (while (not save-passwd)
	(setq save-passwd
	      (gnus-ofsetup-completing-read-symbol
	       "How long do you save password"
	       'never 'exit-emacs 'permanence))
	(if (and (eq save-passwd 'permanence)
		 (not (y-or-n-p
		       "Your password will be saved to newsrc file. OK? ")))
	    (setq save-passwd nil)))
	
      ;; Write to setting file.
      (save-excursion
	(set-buffer (get-buffer-create "* Setting"))
	(erase-buffer)
	(insert ";;\n");
	(insert ";; This file is created by gnus-ofsetup.el\n")
	(insert ";; Creation date : " (current-time-string) "\n")
	(insert ";;\n")

	;; write Basic setting
	(insert "(setq gnus-offline-news-fetch-method '"
		(prin1-to-string news-fetch-method) ")\n")
	(insert "(setq gnus-offline-mail-fetch-method '"
		(prin1-to-string mail-fetch-method) ")\n")

	;; write dialup/hangup program and options.
	(insert "(setq gnus-offline-dialup-program "
		(prin1-to-string dialup-program) ")\n")
	(when (stringp dialup-program)
	  (insert "(setq gnus-offline-dialup-program-arguments '"
		  (prin1-to-string dialup-program-arguments) ")\n"))
	(insert "(setq gnus-offline-hangup-program "
		(prin1-to-string hangup-program) ")\n")
	(when (stringp hangup-program)
	  (insert "(setq gnus-offline-hangup-program-arguments '"
		  (prin1-to-string hangup-program-arguments)
		  ")\n"))

	;; write setting about MIEE.
	(when (eq drafts-queue-type 'miee)
	  (insert "(setq gnus-offline-mail-spool-directory "
		  (prin1-to-string mail-spool-directory) ")\n")
	  (insert "(setq gnus-offline-news-spool-directory "
		  (prin1-to-string news-spool-directory) ")\n")
	  (insert "(setq sendmail-to-spool-directory\n"
		  "gnus-offline-mail-spool-directory)\n")
	  (insert "(setq news-spool-request-post-directory\n"
		  "gnus-offline-news-spool-directory)\n")
	  (insert "(load \"miee\")\n")
	  (insert "(setq message-send-news-function '"
		  (prin1-to-string send-news-function) ")\n"))

	;; write setting about nnspool and gnus-agent.
	(if (equal news-fetch-method 'nnspool)
	    (insert "(message-offline-state)\n")
	  (insert "(setq gnus-agent-directory "
		  (prin1-to-string agent-directory) ")\n"))

	;; write setting about queue type -- MIEE or nnagent.
	(insert "(setq gnus-offline-drafts-queue-type '"
		(prin1-to-string drafts-queue-type) ")\n")
	(insert "(setq gnus-offline-MTA-type '"
		(prin1-to-string MTA-type) ")\n")

	;; Offline setting for gnus-nntp-*
	(insert "(setq gnus-nntp-service nil)\n")
	(insert "(setq gnus-nntp-server nil)\n")

	;; Write setting about hooks.
	(insert "(add-hook 'gnus-group-mode-hook 'gnus-offline-processed-by-timer t)\n")
	(insert "(add-hook 'gnus-group-mode-hook 'gnus-offline-error-check t)\n")
	(insert "(add-hook 'gnus-after-getting-new-news-hook 'gnus-offline-after-get-new-news)\n")
	(insert "(add-hook 'gnus-after-getting-news-hook 'gnus-offline-after-get-new-news)\n")
	(when (eq news-fetch-method 'nnspool)
	  (insert "(add-hook 'gnus-after-getting-news-hook 'gnus-offline-nnspool-hangup-line)\n")
	  (insert "(add-hook 'gnus-before-startup-hook (lambda () (setq nnmail-spool-file nil)))\n"))
	(insert "(add-hook 'message-send-hook 'gnus-offline-message-add-header)\n")
	(insert "(autoload 'gnus-offline-setup \"gnus-offline\")\n")
	(insert "(add-hook 'gnus-load-hook 'gnus-offline-setup)\n")

	;; Write stting about nnmail.el
	(insert "(setq nnmail-movemail-program '"
		(prin1-to-string movemail-program) ")\n")
	(when (stringp movemail-program)
	  (insert "(setenv \"MOVEMAIL\""
		  (prin1-to-string movemail-option) ")\n")
	  (insert "(setq nnmail-movemail-program-apop-option '"
		  (prin1-to-string movemail-program-apop-option) ")\n"))
	(insert "(setq gnus-offline-mail-source '"
		(prin1-to-string spool-file) ")\n")
	(insert
	 (cond
	  ((eq save-passwd 'never)
	   "(setq nnmail-pop-password-required nil)\n")
	  ((eq save-passwd 'exit-emacs)
	   "(setq nnmail-pop-password-required t)\n")
	  ((eq save-passwd 'permanence)
	   "(setq nnmail-pop-password-required t)
(add-hook 'gnus-setup-news-hook 
	  (lambda ()
	    (add-to-list 'gnus-variable-list 'nnmail-internal-password-cache)))\n")))
	(write-region (point-min) (point-max) gnus-offline-setting-file))
      (kill-buffer "* Setting")))
  (load gnus-offline-setting-file))

;; gnus-ofsetup.el Ends here.

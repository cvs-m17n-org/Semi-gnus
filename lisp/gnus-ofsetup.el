;;; gnus-ofsetup.el --- Setup advisor for Offline reading for Mail/News.
;;; $Id: gnus-ofsetup.el,v 1.1.2.1 1998-11-09 14:44:07 ichikawa Exp $

;;; Code:

(defvar gnus-offline-setting-file "~/.gnus-offline.el")
(defvar gnus-offline-use-miee nil)

;;; To silence byte compiler
(and
 (fboundp 'eval-when-compile)
 (eval-when-compile
   (save-excursion
     (beginning-of-defun)
     (eval-region (point-min) (point)))
   (let (case-fold-search)
     (mapcar
      (function
       (lambda (symbol)
	 (unless (boundp symbol)
	   (make-local-variable symbol)
	   (eval (list 'setq symbol nil)))))
      '(:group
	:prefix :type
	sendmail-to-spool-directory
	news-spool-request-post-directory
	nnspool-version
	nnagent-version
	msspool-news-server
	msspool-news-service
	gnspool-get-news
	mail-spool-send
	news-spool-post
	gnus-agent-handle-level
	))
     (make-local-variable 'byte-compile-warnings)
     (setq byte-compile-warnings nil))))

(defun gnus-setup-for-offline ()
  "*Set up Gnus for offline environment."
  (interactive)
  
  (if (not (file-exists-p gnus-offline-setting-file))
      (progn
	(let ((news-method
	       (completing-read
		"Select method for offline News reading: "
		'(("gnus-agent" 1) ("nnspool" 2))
		nil t nil))
	      (mail-method 'nnmail)
	      (use-miee
	       (y-or-n-p "Use MIEE post/send message "))
	      (program
	       (read-from-minibuffer "Dialup/Hangup program: "))
	      (mta-type
	       (completing-read
		"Select MTA type for sending mail: "
		'(("smtp" 1) ("sendmail" 2))
		nil t nil))
	      )
	  (if (equal news-method "gnus-agent")
	      (setq gnus-offline-news-fetch-method 'nnagent)
	    (setq gnus-offline-news-fetch-method 'nnspool))
	  (setq gnus-offline-mail-fetch-method mail-method)
	  (setq gnus-offline-use-miee use-miee)
	  (setq gnus-offline-hangup-program program)
	  (setq gnus-offline-dialup-program program)
	  (if use-miee
	      (progn
		;; Setting for MIEE.
		(let ((news-spool
		       (read-from-minibuffer "News spool directory for sending: "))
		      (mail-spool
		       (read-from-minibuffer "Mail spool directory for sending: ")))
		  (setq gnus-offline-mail-spool-directory mail-spool)
		  (setq gnus-offline-news-spool-directory news-spool)
		  (setq gnus-offline-drafts-queue-type 'miee)))
	    ;; Setting for gnus-agent.
	    (setq gnus-offline-drafts-queue-type 'agent)
	    (let ((agent-directory
		   (read-from-minibuffer "Agent directory: " "~/News/agent")))
	      (setq gnus-agent-directory agent-directory)))
	  (if (equal mta-type "smtp")
	      (setq gnus-offline-MTA-type 'smtp)
	    (setq gnus-offline-MTA-type 'sendmail)
	    )
	  
	  ;; Set offline news posting function and post directory using MIEE.
	  (if gnus-offline-use-miee
	      (progn
		;; Load MIEE.
		(load "miee")
		;; Set news post function for MIEE.
		(setq message-send-news-function 'gnspool-request-post)
		
		;; Spool directory setting - MIEE.
		(if (not (file-exists-p gnus-offline-mail-spool-directory))
		    (make-directory gnus-offline-mail-spool-directory t))
		(setq sendmail-to-spool-directory gnus-offline-mail-spool-directory)
		(if (not (file-exists-p gnus-offline-news-spool-directory))
		    (make-directory gnus-offline-news-spool-directory t))
	      (setq news-spool-request-post-directory gnus-offline-news-spool-directory)))
	  )
	
	;; Write to setting file.
	(setq tmp-buffer (get-buffer-create "* Setting"))
	(set-buffer "* Setting")
	(erase-buffer)
	(insert ";;\n");
	(insert ";; This file is created by gnus-ofsetup.el\n")
	(insert ";; Creation date : ")
	(insert (current-time-string))
	(insert "\n")
	(insert ";;\n")
	(insert "(setq gnus-offline-news-fetch-method '")
	(insert (prin1-to-string gnus-offline-news-fetch-method))
	(insert ")\n")
	(insert "(setq gnus-offline-mail-fetch-method '")
	(insert (prin1-to-string gnus-offline-mail-fetch-method))
	(insert ")\n")
	(insert "(setq gnus-offline-use-miee ")
	(insert (prin1-to-string gnus-offline-use-miee))
	(insert ")\n")
	(insert "(setq gnus-offline-dialup-program ")
	(insert (prin1-to-string gnus-offline-dialup-program))
	(insert ")\n")
	(insert "(setq gnus-offline-hangup-program ")
	(insert (prin1-to-string gnus-offline-hangup-program))
	(insert ")\n")
	(if gnus-offline-use-miee
	    (progn
	      (insert "(setq gnus-offline-mail-spool-directory ")
	      (insert (prin1-to-string gnus-offline-mail-spool-directory))
	      (insert ")\n")
	      (insert "(setq gnus-offline-news-spool-directory ")
	      (insert (prin1-to-string gnus-offline-news-spool-directory))
	      (insert ")\n")
	      (insert "(load \"miee\")\n")
	      (insert "(message-offline-state)\n"))
	  (insert "(setq gnus-agent-directory ")
	  (insert (prin1-to-string gnus-agent-directory))
	  (insert ")\n"))
	(insert "(setq gnus-offline-drafts-queue-type '")
	(insert (prin1-to-string gnus-offline-drafts-queue-type))
	(insert ")\n")
	(insert "(setq gnus-offline-MTA-type '")
	(insert (prin1-to-string gnus-offline-MTA-type))
	(insert ")\n")
	(insert "(setq gnus-nntp-service nil)\n")
	(insert "(setq gnus-nntp-server nil)\n")
	(insert "(setq nnmail-spool-file nil)\n")
	(insert "(add-hook 'gnus-group-mode-hook 'gnus-offline-processed-by-timer)\n")
	(insert "(add-hook 'gnus-after-getting-new-news-hook 'gnus-offline-after-get-new-news)\n")
	(insert "(add-hook 'gnus-after-getting-news-hook 'gnus-offline-after-get-new-news)\n")
	(if (eq gnus-offline-news-fetch-method 'nnspool)
	    (insert "(add-hook 'after-getting-news-hook 'gnus-offline-nnspool-hangup-line)\n"))
	(insert "(add-hook 'message-send-hook 'gnus-offline-message-add-header)\n")
	(insert "(require 'pop3-fma)\n")
	(insert "(add-hook 'message-send-hook 'pop3-fma-message-add-header)\n")
	(insert "(autoload 'gnus-offline-setup \"gnus-offline\")\n")
	(insert "(add-hook 'gnus-load-hook 'gnus-offline-setup)\n")
	(write-region (point-min) (point-max) gnus-offline-setting-file)
	(kill-buffer "* Setting"))
	)
  (load gnus-offline-setting-file)
  )

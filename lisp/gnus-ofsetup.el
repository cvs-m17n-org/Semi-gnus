;;; gnus-ofsetup.el --- Setup advisor for Offline reading for Mail/News.
;;; $Id: gnus-ofsetup.el,v 1.1.2.2 1998-11-10 14:53:51 ichikawa Exp $

;;; Code:

(defvar gnus-offline-setting-file "~/.gnus-offline.el")
(defvar gnus-offline-use-miee nil)
(defvar gnus-offline-news-fetch-method nil)
(defvar gnus-offline-mail-fetch-method nil)
(defvar gnus-offline-hangup-program nil)
(defvar gnus-offline-dialup-program nil)
(defvar pop3-fma-spool-file-alist nil)
(defvar pop3-fma-movemail-type nil)
(defvar pop3-fma-movemail-arguments nil)
(defvar address)
(defvar options)

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
		"Method for offline News reading (TAB to completion): "
		'(("gnus-agent" 1) ("nnspool" 2))
		nil t nil))
	      (mail-method 'nnmail)
	      (use-miee
	       (y-or-n-p "Use MIEE post/send message "))
	      (program
	       (read-file-name "Dialup/Hangup program(type nil you do not use): "))
	      (mta-type
	       (completing-read
		"Select MTA type for sending mail (TAB to completion): "
		'(("smtp" 1) ("sendmail" 2))
		nil t nil))
	      (num-of-address
	       (read-from-minibuffer "How many e-mail address do you have: "))
	      )
	  ;;
	  ;; Set variables.
	  (if (string-equal news-method "gnus-agent")
	      (setq gnus-offline-news-fetch-method 'nnagent)
	    (setq gnus-offline-news-fetch-method 'nnspool))
	  ;;
	  (setq gnus-offline-mail-fetch-method mail-method)
	  (setq gnus-offline-use-miee use-miee)
	  ;; Set programs.
	  (if (string-equal program "nil")
	      (progn
		(setq gnus-offline-hangup-program nil)
		(setq gnus-offline-dialup-program nil))
	    (let ((options
		   (read-from-minibuffer "Dialup program options: ")))
	      (setq gnus-offline-dialup-program-arguments (split-string options "[\t ]+")))
	    (let ((options
		   (read-from-minibuffer "Hangup program options: ")))
	      (setq gnus-offline-hangup-program-arguments (split-string options "[\t ]+")))
	    (setq gnus-offline-hangup-program program)
	    (setq gnus-offline-dialup-program program))

	  ;; Set spool directory for outgoing messages.
	  (if use-miee
	      (progn
		;; Setting for MIEE with nnspool.
		(let ((news-spool
		       (read-from-minibuffer "News spool directory for sending: " "~/spool/mail.out"))
		      (mail-spool
		       (read-from-minibuffer "Mail spool directory for sending: " "~/spool/news.out")))
		  (setq gnus-offline-mail-spool-directory mail-spool)
		  (setq gnus-offline-news-spool-directory news-spool)
		  (setq gnus-offline-drafts-queue-type 'miee)
		
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
	    ;; Setting for gnus-agent.
	    (setq gnus-offline-drafts-queue-type 'agent)
	    (let ((agent-directory
		   (read-from-minibuffer "Agent directory: " "~/News/agent")))
	      (setq gnus-agent-directory agent-directory)))
	  
	  ;; Determin MTA type.
	  (if (string-equal mta-type "smtp")
	      (setq gnus-offline-MTA-type 'smtp)
	    (setq gnus-offline-MTA-type 'sendmail)
	    )
	  ;;
	  ;; Set E-Mail Address and pop3 movemail type.
	  (setq i (string-to-int num-of-address))
	  (setq address nil)
	  (while (> i 0)
	    (setq address (append address (list (list
			       (concat "po:" (read-from-minibuffer "Email address (user@mailhost): "))
			       (completing-read
				"Authentification Method (TAB to completion): "
				'(("pass" 1) ("apop" 2)) nil t nil)))))
	    (setq i (- i 1)))

	  ;; Replace "hoge" -> 'hoge
	  (mapcar
	   (lambda (x)
	     (if (string-equal (nth 1 x) "pass")
		 (setcar (cdr x) 'pass)
	       (setcar (cdr x) 'apop)))
	   address)
	  (setq pop3-fma-spool-file-alist address)

	  ;; Set movemail type.
	  (let ((movemail-type
		 (completing-read
		  "Select movemail type for retreave mail (TAB to completion): "
		  '(("exe" 1) ("lisp" 2))
		  nil t nil))
		)
	    (if (string-equal movemail-type "exe")
		(let ((options
		       (read-from-minibuffer "movemail options: ")))
		  (setq pop3-fma-movemail-arguments (split-string options "[\t ]+"))))
	    (if (string-equal movemail-type "exe")
		(setq pop3-fma-movemail-type 'exe)
	      (setq pop3-fma-movemail-type 'lisp))))
	  
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
	(if (stringp gnus-offline-dialup-program)
	    (progn
	      (insert "(setq gnus-offline-dialup-program-arguments '")
	      (insert (prin1-to-string gnus-offline-dialup-program-arguments))
	      (insert ")\n")))
	(insert "(setq gnus-offline-hangup-program ")
	(insert (prin1-to-string gnus-offline-hangup-program))
	(insert ")\n")
	(if (stringp gnus-offline-hangup-program)
	    (progn
	      (insert "(setq gnus-offline-hangup-program-arguments '")
	      (insert (prin1-to-string gnus-offline-hangup-program-arguments))
	      (insert ")\n")))
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
	(insert "(autoload 'gnus-offline-setup \"gnus-offline\")\n")
	(insert "(add-hook 'gnus-load-hook 'gnus-offline-setup)\n")
	(insert "(require 'pop3-fma)\n")
	(insert "(add-hook 'message-send-hook 'pop3-fma-message-add-header)\n")
	(insert "(setq pop3-fma-spool-file-alist '")
	(insert (prin1-to-string pop3-fma-spool-file-alist))
	(insert ")\n")
	(insert "(setq pop3-fma-movemail-type '")
	(insert (prin1-to-string pop3-fma-movemail-type))
	(insert ")\n")
	(if (eq pop3-fma-movemail-type 'exe)
	    (progn
	      (insert "(setq pop3-fma-movemail-arguments '")
	      (insert (prin1-to-string pop3-fma-movemail-arguments))
	      (insert ")\n")))
	(write-region (point-min) (point-max) gnus-offline-setting-file)
	(kill-buffer "* Setting"))
	)
  (load gnus-offline-setting-file)
  )

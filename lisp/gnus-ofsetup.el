;;; gnus-ofsetup.el --- Setup advisor for Offline reading for Mail/News.
;;;
;;; $Id: gnus-ofsetup.el,v 1.1.2.18 1999-05-13 02:36:22 yamaoka Exp $
;;;
;;; Copyright (C) 1998 Tatsuya Ichikawa
;;; Author: Tatsuya Ichikawa <t-ichi@po.shiojiri.ne.jp>
;;;
;;; This file is part of Semi-gnus.
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
;;;      M-x load[RET]gnus-ofsetup
;;;      M-x gnus-setup-for-offline
;;;

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
(defvar use-miee nil)
(defvar address nil)
(defvar mail-source nil)
(defvar options nil)

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
	      (program
	       (read-file-name "Dialup/Hangup program(type nil or null string you do not use): "))
	      (mta-type
	       (completing-read
		"Select MTA type for sending mail (TAB to completion): "
		'(("smtp" 1) ("sendmail" 2))
		nil t nil))
	      (num-of-address
	       (read-from-minibuffer "How many e-mail address do you have: "))
	      )
	  (if (string-equal news-method "nnspool")
	      (setq use-miee t)
	    (setq use-miee (y-or-n-p "Use MIEE post/send message ")))
	  ;;
	  ;; Set variables.
	  (if (string-equal news-method "gnus-agent")
	      (setq gnus-offline-news-fetch-method 'nnagent)
	    (setq gnus-offline-news-fetch-method 'nnspool))
	  ;;
	  (setq gnus-offline-mail-fetch-method mail-method)
	  (setq gnus-offline-use-miee use-miee)
	  
	  ;; Set programs.
	  (if (or (string-equal program "nil")
		  (string-equal program ""))
	      (progn
		(setq gnus-offline-hangup-program nil)
		(setq gnus-offline-dialup-program nil))
	    (let ((options
		   (read-from-minibuffer "Dialup program options: ")))
	      (setq gnus-offline-dialup-program-arguments
		    (split-string options "[\t ]+")))
	    (let ((options
		   (read-from-minibuffer "Hangup program options: ")))
	      (setq gnus-offline-hangup-program-arguments
		    (split-string options "[\t ]+")))
	    (setq gnus-offline-hangup-program program)
	    (setq gnus-offline-dialup-program program))
	    
	    ;; Set spool directory for outgoing messages.
	  (if use-miee
	      (progn
		;; Setting for MIEE with nnspool.
		(let ((news-spool
		       (read-from-minibuffer
			"News spool directory for sending: "
			"/usr/spool/news.out"))
		      (mail-spool
		       (read-from-minibuffer
			"Mail spool directory for sending: "
			"/usr/spool/mail.out")))
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
		  (setq sendmail-to-spool-directory
			gnus-offline-mail-spool-directory)
		  (if (not (file-exists-p gnus-offline-news-spool-directory))
		      (make-directory gnus-offline-news-spool-directory t))
		  (setq news-spool-request-post-directory
			gnus-offline-news-spool-directory)))
	    
	    ;; Set drafts type gnus-agent.
	    (setq gnus-offline-drafts-queue-type 'agent))
	  
	  ;; Setting for gnus-agent.
	  (if (eq gnus-offline-news-fetch-method 'nnagent)
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
	  (if (not (locate-library "mail-source"))
	      (progn
		(while (> i 0)
		  (setq address
			(append address
				(list
				 (list
				  (concat "po:"
					  (read-from-minibuffer
					   "Email address (user@mailhost): "))
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
	    ;;
	    ;; Use mail-source.el
	    (setq mail-source nil)
	    (while (> i 0)
	      (let ((user (read-from-minibuffer "Mail Account name : "))
		    (server (read-from-minibuffer "Mail server : "))
		    (auth (completing-read
			  "Authentification Method (TAB to completion): "
			  '(("password") ("apop")) nil t nil))
		    (islisp (y-or-n-p "Do you use pop3.el to fetch mail? "))
		    source)
		(if (not islisp)
		    (let ((prog (read-file-name "movemail program name: "
						exec-directory "movemail"))
			  (args (read-from-minibuffer "movemail options: "
						      "-pf")))
		      (setq source (list 'pop
					 :user user
					 :server server
					 :program (format "%s %s %s %s %s"
							  prog
							  args
							  "po:%u"
							  "%t"
							  "%p"))))
		  (setq source (list 'pop
				     :user user
				     :server server)))
		(setq mail-source
		      (nconc mail-source
			     (list
			      (if (string-equal "apop" auth)
				  (nconc source '(:authentication apop))
				source)))))
	      (setq i (1- i)))
	    (setq gnus-offline-mail-source mail-source)))

	(setq save-passwd
	      (y-or-n-p "Do you save password information to newsrc file? "))
	
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

	;; write Basic setting
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

	;; write dialup/hangup program and options.
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

	;; write setting about MIEE.
	(if gnus-offline-use-miee
	    (progn
	      (insert "(setq gnus-offline-mail-spool-directory ")
	      (insert (prin1-to-string gnus-offline-mail-spool-directory))
	      (insert ")\n")
	      (insert "(setq gnus-offline-news-spool-directory ")
	      (insert (prin1-to-string gnus-offline-news-spool-directory))
	      (insert ")\n")
	      (insert "(setq sendmail-to-spool-directory gnus-offline-mail-spool-directory)\n")
	      (insert "(setq news-spool-request-post-directory gnus-offline-news-spool-directory)\n")
	      (insert "(load \"miee\")\n")
	      (insert "(setq message-send-news-function '")
	      (insert (prin1-to-string message-send-news-function))
	      (insert ")\n")))

	;; write setting about nnspool and gnus-agent.
	(if (equal gnus-offline-news-fetch-method 'nnspool)
	    (insert "(message-offline-state)\n")
	  (insert "(setq gnus-agent-directory ")
	  (insert (prin1-to-string gnus-agent-directory))
	  (insert ")\n"))

	;; write setting about queue type -- MIEE or nnagent.
	(insert "(setq gnus-offline-drafts-queue-type '")
	(insert (prin1-to-string gnus-offline-drafts-queue-type))
	(insert ")\n")
	(insert "(setq gnus-offline-MTA-type '")
	(insert (prin1-to-string gnus-offline-MTA-type))
	(insert ")\n")

	;; Offline setting for gnus-nntp-*
	(insert "(setq gnus-nntp-service nil)\n")
	(insert "(setq gnus-nntp-server nil)\n")

	;; Write setting about hooks.
	(insert "(add-hook 'gnus-group-mode-hook 'gnus-offline-processed-by-timer t)\n")
	(insert "(add-hook 'gnus-group-mode-hook 'gnus-offline-error-check t)\n")
	(insert "(add-hook 'gnus-after-getting-new-news-hook 'gnus-offline-after-get-new-news)\n")
	(insert "(add-hook 'gnus-after-getting-news-hook 'gnus-offline-after-get-new-news)\n")
	(if (eq gnus-offline-news-fetch-method 'nnspool)
	    (progn
	      (insert "(add-hook 'after-getting-news-hook 'gnus-offline-nnspool-hangup-line)\n")
	      (insert "(add-hook 'gnus-before-startup-hook (lambda () (setq nnmail-spool-file nil)))\n")))
	(insert "(add-hook 'message-send-hook 'gnus-offline-message-add-header)\n")
	(insert "(autoload 'gnus-offline-setup \"gnus-offline\")\n")
	(insert "(add-hook 'gnus-load-hook 'gnus-offline-setup)\n")

	(if (not (locate-library "mail-source"))
	    (progn
	      ;; Write setting about pop3-fma.
	      (insert "(require 'pop3-fma)\n")
	      (insert "(add-hook 'message-send-hook 'pop3-fma-message-add-header)\n")
	      (insert "(setq pop3-fma-spool-file-alist '")
	      (insert (prin1-to-string pop3-fma-spool-file-alist))
	      (insert ")\n")
	      (insert "(setq pop3-fma-movemail-type '")
	      (insert (prin1-to-string pop3-fma-movemail-type))
	      (insert ")\n")
	      (if save-passwd
		  (insert "(add-hook 'gnus-setup-news-hook \n    (lambda ()\n        (setq pop3-fma-save-password-information t)\n        (add-to-list 'gnus-variable-list 'pop3-fma-password)))\n"))
	      (if (eq pop3-fma-movemail-type 'exe)
		  (progn
		    (insert "(setq pop3-fma-movemail-arguments '")
		    (insert (prin1-to-string pop3-fma-movemail-arguments))
		    (insert ")\n"))))
	  ;; Write stting about mail-source.el
	  (insert "(setq gnus-offline-mail-source '")
	  (insert (prin1-to-string gnus-offline-mail-source))
	  (insert ")\n")
	  (insert "(setq nnmail-spool-file gnus-offline-mail-source)\n")
	  (insert "(require 'read-passwd)\n")
	  (insert "(setq mail-source-read-passwd 'read-pw-read-passwd)\n")
	  (insert "(add-hook 'gnus-setup-news-hook 'read-pw-set-mail-source-passwd-cache)\n")
	  (if save-passwd
	      (insert "(add-hook 'gnus-setup-news-hook \n    (lambda ()\n        (add-to-list 'gnus-variable-list 'mail-source-password-cache)))\n"))
	  )
	(write-region (point-min) (point-max) gnus-offline-setting-file)
	(kill-buffer "* Setting"))
    )
  (load gnus-offline-setting-file))
;; gnus-ofsetup.el Ends here.

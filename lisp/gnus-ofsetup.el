;;; gnus-ofsetup.el --- Setup advisor for Offline reading for Mail/News.
;;;
;;; $Id: gnus-ofsetup.el,v 1.1.2.19.4.2 1999-08-16 10:10:38 czkmt Exp $
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

(eval-when-compile
  (require 'poe)
  (make-local-variable 'byte-compile-warnings)
  (setq byte-compile-warnings nil))

(defvar gnus-offline-setting-file "~/.gnus-offline.el")

(defun gnus-ofsetup-completing-read-symbol (msg &rest syms)
  (intern
   (completing-read (concat msg " (TAB to completion): ")
		    (mapcar
		     (lambda (sym)
		       (list (symbol-name sym)))
		     syms)
		    nil t nil)))

(defun gnus-setup-for-offline ()
  "*Set up Gnus for offline environment."
  (interactive)
  (unless (file-exists-p gnus-offline-setting-file)
    (let (news-method
	  mail-method agent-directory drafts-queue-type news-spool mail-spool
	  send-news-function use-miee MTA-type dialup-program
	  dialup-program-arguments hangup-program hangup-program-arguments
	  sendmail-to-spool-directory news-spool-request-post-directory
	  address mail-source spool-file save-passwd)
      (setq news-method
	    (gnus-ofsetup-completing-read-symbol
	     "Method for offline News reading"
	     'nnagent 'nnspool))
      ;; Setting for gnus-agent.
      (if (eq news-method 'nnagent)
	  (setq agent-directory
		(read-from-minibuffer "Agent directory: " "~/News/agent")))
      (setq mail-method 'nnmail)
      (setq dialup-program
	    (read-file-name
	     "Dialup program (or null string if you do not use): "
	     nil nil t))
      (if (string-match "^[ \t]*$" dialup-program)
	  (setq dialup-program nil)
	(setq dialup-program-arguments
	      (delete "" (split-string
			  (read-from-minibuffer "Dialup program options: ")
			  "[\t ]+"))))
      (setq hangup-program
	    (read-file-name
	     "Hangup program (or null string if you do not use): "
	     nil nil t))
      (if (string-match "^[ \t]*$" hangup-program)
	  (setq hangup-program nil)
	(setq hangup-program-arguments
	      (delete "" (split-string
			  (read-from-minibuffer "Hangup program options: ")
			  "[\t ]+"))))
      (setq MTA-type (gnus-ofsetup-completing-read-symbol
		      "Select MTA type for sending mail"
		      'smtp 'sendmail))
      (if (eq news-method 'nnspool)
	  (setq use-miee t)
	(setq use-miee (y-or-n-p "Use MIEE post/send message ")))
      (if use-miee
	  (progn
	    ;; Setting for MIEE.
	    (setq news-spool
		  (read-from-minibuffer
		   "News spool directory for sending: "
		   "/usr/spool/news.out"))
	    (setq mail-spool
		  (read-from-minibuffer
		   "Mail spool directory for sending: "
		   "/usr/spool/mail.out"))
	    (setq drafts-queue-type 'miee)
	    (gnus-ofsetup-prepare-for-miee))
	;; Set drafts type gnus-agent.
	(setq drafts-queue-type 'agent))
      ;; Set E-Mail Address and pop3 movemail type.
      (setq num-of-address
	    (read-from-minibuffer "How many e-mail address do you have: "))
      (setq i (string-to-int num-of-address))
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
      (setq save-passwd
	    (y-or-n-p "Do you save password information to newsrc file? "))
      ;;
      (gnus-ofsetup-update-setting-file)))
  (load gnus-offline-setting-file))

(defun gnus-ofsetup-prepare-for-miee ()
  ;; Spool directory setting - MIEE.
  (setq mail-spool (or mail-spool "/usr/spool/mail.out"))
  (setq news-spool (or news-spool "/usr/spool/news.out"))
  (condition-case nil
      (progn
	(if (not (file-exists-p mail-spool))
	    (make-directory mail-spool t))
	(if (not (file-exists-p news-spool))
	    (make-directory news-spool t)))
    (error
     (error "Making directories failed. Set mail/news spool directories properly."))))

(defun gnus-ofsetup-update-setting-file ()
  ;; Write to setting file.
  (save-excursion
    (set-buffer (get-buffer-create "* Setting"))
    (erase-buffer)
    (insert ";;\n")
    (insert ";; This file is created by gnus-ofsetup.el\n")
    (insert ";; Creation date : " (current-time-string) "\n")
    (insert ";;\n")

    ;; write Basic setting
    (insert "(setq gnus-offline-news-fetch-method '"
	    (prin1-to-string news-method) ")\n")
    (insert "(setq gnus-offline-mail-fetch-method '"
	    (prin1-to-string mail-method) ")\n")

    ;; write dialup/hangup program and options.
    (insert "(setq gnus-offline-dialup-program "
	    (prin1-to-string dialup-program) ")\n")
    (if (stringp dialup-program)
	(insert "(setq gnus-offline-dialup-program-arguments '"
		(prin1-to-string dialup-program-arguments) ")\n"))
    (insert "(setq gnus-offline-hangup-program "
	    (prin1-to-string hangup-program) ")\n")
    (if (stringp hangup-program)
	(insert "(setq gnus-offline-hangup-program-arguments '"
		(prin1-to-string hangup-program-arguments) ")\n"))

    (if (boundp 'interval)
	  (insert "(setq gnus-offline-interval-time "
		  (prin1-to-string interval) ")\n"))

    ;; write setting about MIEE.
    (when use-miee
      (insert "(setq sendmail-to-spool-directory "
	      (prin1-to-string mail-spool) ")\n")
      (insert "(setq news-spool-request-post-directory "
	      (prin1-to-string news-spool) ")\n")
      (insert "(if (not (boundp 'miee-version))
    (load \"miee\"))\n")
      (insert "(setq message-send-news-function 'gnspool-request-post)\n"))

    ;; write setting about nnspool and gnus-agent.
    (if (eq news-method 'nnspool)
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
    (when (eq news-method 'nnspool)
      (insert "(add-hook 'after-getting-news-hook 'gnus-offline-nnspool-hangup-line)\n")
      (insert "(add-hook 'gnus-before-startup-hook (lambda () (setq nnmail-spool-file nil)))\n"))
    (insert "(add-hook 'message-send-hook 'gnus-offline-message-add-header)\n")
    (insert "(autoload 'gnus-offline-setup \"gnus-offline\")\n")
    (insert "(add-hook 'gnus-load-hook 'gnus-offline-setup)\n")

    ;; Write stting about mail-source.el
    (insert "(setq gnus-offline-mail-source '"
	    (prin1-to-string mail-source) ")\n")
    (insert "(setq nnmail-spool-file gnus-offline-mail-source)\n")
    (insert "(require 'read-passwd)\n")
    (insert "(setq mail-source-read-passwd 'read-pw-read-passwd)\n")
    (insert "(add-hook 'gnus-setup-news-hook 'read-pw-set-mail-source-passwd-cache)\n")
    (if save-passwd
	(insert "(add-hook 'gnus-setup-news-hook
	  (lambda ()
	    (add-to-list 'gnus-variable-list 'mail-source-password-cache)))\n"))

    ;;
    (write-region (point-min) (point-max) gnus-offline-setting-file))
  (kill-buffer "* Setting"))


;;; Suppport for customizing gnus-ofsetup parameters.

(autoload 'gnus-custom-mode "gnus-cus" nil t)

(defun gnus-ofsetup-find-parameters ()
  "Return the each current value of gnus-offline parameters."
  `((news-method
     (choice :tag "News Method" :value ,gnus-offline-news-fetch-method
	     (const :tag "Gnus Agent" nnagent)
	     (const :tag "nnspool"    nnspool)) "\
Method to fetch news articles.")

    (dialup-program
     (choice :tag "Dialup Program" :value ,gnus-offline-dialup-program
	     (string :tag "Use Program..")
	     (const :tag "Don't auto-dial." nil)) "\
Program which does dial.")

    (dialup-program-arguments
     (repeat :tag "Dialup Program Arguments"
	     :value ,gnus-offline-dialup-program-arguments
	     (string :tag "Argument"))"\
Program arguments of gnus-offline-dialup-program.")

    (hangup-program
     (choice :tag "Hangup Program" :value ,gnus-offline-hangup-program
	     (string :tag "Use Program..")
	     (const :tag "Don't auto-hangup." nil)) "\
Program which does hangup.")

    (hangup-program-arguments
     (repeat :tag "Hangup Program Arguments"
	     :value ,gnus-offline-hangup-program-arguments
	     (string :tag "Argument")) "\
Program arguments of gnus-offline-hangup-program.")

    (interval
     (integer :tag "Interval between Jobs"
	      :value ,gnus-offline-interval-time) "\
Interval time(minutes) to do online jobs.
If set to 0 , timer call is disabled.")

    (drafts-queue-type
     (choice :tag "Drafts Queue Type" :value ,gnus-offline-drafts-queue-type
	     (const :tag "Gnus Draft for queuing."    agent)
	     (const :tag "I prefer MIEE for queuing." miee)) "\
Type of queue used for draft messages.

If the select method for news is nnspool, you must choose MIEE.
MIEE is another library for offline messaging. It isn't part of
Semi-gnus. If you want to know about MIEE, see README-offline.{en,ja}.")

    (mail-spool
     (directory :tag "Mail Spool Directory for MIEE"
		:value ,(cond ((and (boundp 'sendmail-to-spool-directory)
				    sendmail-to-spool-directory)
			       sendmail-to-spool-directory)
			      (t "/usr/spool/mail.out"))))

    (news-spool
     (directory :tag "News Spool Directory for MIEE"
		:value ,(cond ((and (boundp 'news-spool-request-post-directory)
				    news-spool-request-post-directory)
			       news-spool-request-post-directory)
			      (t "/usr/spool/news.out"))))

    (MTA-type
     (choice :tag "MTA Type" :value ,gnus-offline-MTA-type
	     (const :tag "Use smtp.el"  smtp)
	     (const :tag "Use sendmail" sendmail)) "\
Type of MTA used for sending mail.")

    (save-passwd
     (choice :tag "Save Password in Startup File"
	     :value ,(if (memq 'mail-source-password-cache gnus-variable-list)
			 t
			 nil)
	     (const :tag "OK, I'm sure it's safe."     t)
	     (const :tag "No way, it's too dangerous!" nil)) "\
Whether you want your POP passwords written in .newsrc.eld or not.")

    (mail-source
     (sexp :tag "Mail Sources" :value ,gnus-offline-mail-source) "\
Information of mail sources. Actually, a list of `Mail Source Specifiers'.

The format of this variable is just the same as `mail-sources' (or
`nnmail-spool-file' which seems obsolete).

`Mail Source Specifiers' can take a lot of keywords. For example,
if you want to use movemail instead of pop3.el which comes with
Gnus, you can set a specifier using the kerword :program as shown
below:

          (pop :program \"movemail -pf po:%u %t %p\")

If you want to know more about mail source specifiers and keywords,
click the button below.")))

(defun gnus-ofsetup-customize ()
  "Edit the gnus-offline parameters."
  (interactive)
  (let* ((params (gnus-ofsetup-find-parameters))
	 (types (mapcar (lambda (entry)
			 `(cons :format "%v%h\n"
				:doc ,(nth 2 entry)
				(const :format "" ,(nth 0 entry))
				,(nth 1 entry)))
			params)))
  (kill-buffer (gnus-get-buffer-create "*Gnus Offline Customize*"))
  (switch-to-buffer (gnus-get-buffer-create "*Gnus Offline Customize*"))
  (gnus-custom-mode)
  (widget-insert "Customize the Gnus Offline Parameters, and press ")
  (widget-create 'push-button
		   :tag "done"
		   :help-echo "Push me when done customizing."
		   :action 'gnus-ofsetup-customize-done)
  (widget-insert "\n\n")
  (make-local-variable 'gnus-ofsetup-params)
  (setq gnus-ofsetup-params
	(widget-create 'group
		       `(set :inline t
			     :greedy t
			     :tag "Parameters"
			     :format "%t:\n%h%v"
			     :doc "\
These parameters will be saved in ~/.gnus-offline.el.

Note: Touching these parameters may require Gnus or even Emacs to be
restarted."
			     ,@types)))

  (widget-create 'info-link
		 :help-echo "Push me to learn more."
		 :tag "<Info> mail sources"
		 "(gnus)Mail Sources")

  (use-local-map widget-keymap)
  (local-set-key "q" 'bury-buffer)
  (widget-setup)
  (goto-char (point-min))))

(defun gnus-ofsetup-customize-done (&rest ignore)
  "Apply changes and bury the buffer."
  (interactive)
  (let ((params (widget-value gnus-ofsetup-params))
	(news-method gnus-offline-news-fetch-method)
	(mail-method gnus-offline-mail-fetch-method)
	(agent-directory gnus-agent-directory)
	(dialup-program gnus-offline-dialup-program)
	(dialup-program-arguments gnus-offline-dialup-program-arguments)
	(hangup-program gnus-offline-hangup-program)
	(hangup-program-arguments gnus-offline-hangup-program-arguments)
	(drafts-queue-type gnus-offline-drafts-queue-type)
	(interval gnus-offline-interval-time)
	(use-miee (and (boundp 'miee-version)
		       (or (eq gnus-offline-news-fetch-method 'nnspool)
			   (eq gnus-offline-drafts-queue-type 'miee))))
	(mail-spool (or (and (boundp 'sendmail-to-spool-directory)
			     sendmail-to-spool-directory)
			"/usr/spool/mail.out"))
	(news-spool (or (and (boundp 'news-spool-request-post-directory)
			     news-spool-request-post-directory)
			"/usr/spool/news.out"))
	(MTA-type gnus-offline-MTA-type)
	(mail-source gnus-offline-mail-source)
	(save-passwd (and (memq 'mail-source-password-cache gnus-variable-list)
			  t)))
    (if (null params)
	(gnus-message 4 "(No changes need to be saved)")
      (mapcar (lambda (el)
		(let ((sym (car el))
		      (val (cdr el)))
		  (set sym val)
		  (cond ((eq sym 'news-method)
			 (if (eq val 'nnspool)
			     (setq use-miee t)))
			((eq sym 'drafts-queue-type)
			 (setq use-miee
			       (if (eq val 'miee) t nil)))
			((eq sym 'save-passwd)
			 (if val
			     (add-to-list 'gnus-variable-list
					  'mail-source-password-cache)
			   (setq gnus-variable-list
				 (delq 'mail-source-password-cache
				       gnus-variable-list)))))))
	      params)
      (if (and (eq news-method 'nnspool)
	       (not (eq drafts-queue-type 'miee)))
	  (error "Invalid parameters. Check the news method and drafts queue type."))
      (if use-miee
	  (gnus-ofsetup-prepare-for-miee))
      (gnus-ofsetup-update-setting-file)
      (load gnus-offline-setting-file)))
  (bury-buffer)
  (switch-to-buffer gnus-group-buffer))

;; gnus-ofsetup.el Ends here.

;;; gnus-offline.el --- To process mail & news at offline environment.

;;; Copyright (C) 1998 Tatsuya Ichikawa
;;;                    Yukihiro Ito
;;; Author: Tatsuya Ichikawa <t-ichi@po.shiojiri.ne.jp>
;;;         Yukihiro Ito <ito@rs.civil.tohoku.ac.jp>
;;;         Hidekazu Nakamura <u90121@uis-inf.co.jp>

;;; Version: 1.53
;;; Keywords: news , mail , offline , gnus
;;;
;;; SPECIAL THANKS
;;;    Keiichi Suzuki <kei-suzu@mail.wbs.or.jp>
;;;    KORIYAMA Naohiro <kory@ba2.so-net.or.jp>
;;;    Katsumi Yamaoka <yamaoka@jpl.org>

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
;;; Note.
;;;   This file works only with after version of Emacs 19.30.
;;;   This file needs miee.el and SEMI.
;;;   If you set gnus-offline-drafts-queue-type to 'agent , you don't need 
;;;   miee.el
;;;   You must use Semi-gnus 6.X.X.
;;;
;;; How to use.
;;; put following code in you .emacs , after the setting of Gnus.
;;;
;;;  (setq gnus-offline-connect-program "/dir/program.exe")
;;;  (setq gnus-offline-connect-program-arguments '("-a" "-b"))
;;;  (setq gnus-offline-hangup-program "/dir/program")
;;;  (setq gnus-offline-hangup-program-arguments '("-c" "-d"))
;;;  (setq gnus-offline-mail-spool-directory "your-send-mail-spool-directory")
;;;  (setq gnus-offline-news-spool-directory "your-send-news-spool-directory")
;;;  (autoload 'gnus-offline-setup "gnus-offline")
;;;  (add-hook 'gnus-load-hook 'gnus-offline-setup)
;;;
;;; If you use gnus-agent as souper , put gnus-agent setup code in you .gnus.el
;;;
;;; If you use nnspool as souper , put following code in your .emacs before
;;; gnus-offline setting.
;;;
;;;  (load "miee")
;;;  (message-offline-state)
;;;
;;; Then , put hang.exe in exec-path directory.
;;;
;;; In Gnus group buffer , type g to get all news and mail.
;;; Then send mail and news in spool directory.
;;;
;;; Variables.
;;;  gnus-offline-connect-program     ... Dialup program name.
;;;  gnus-offline-connect-program-arguments
;;;                                   ... List of dialup program arguments.
;;;  gnus-offline-hangup-program      ... Program name that used hanup line.
;;;  gnus-offline-hangup-program-arguments
;;;                                   ... List of hangup program arguments.
;;;  gnus-offline-mail-spool-directory... spool directory sending mail.
;;;  gnus-offline-news-spool-directory... spool directory sending news.
;;;  gnus-offline-mail-treat-environ  ... toggle sending mail online/offline.
;;;  gnus-offline-articles-to-fetch   ... toggle fetch articles.
;;;                                        both->mail->news->both...
;;;  gnus-offline-load-hook           ... hook before gnus-offline load.
;;;  gnus-offline-before-online-hook  ... hook before all online jobs.
;;;  gnus-offline-after-online-hook   ... hook after all online jobs.
;;;  gnus-offline-interval-time       ... Interval time to do all online jobs.
;;;                                        (minutes)
;;;  gnus-offline-MTA-type            ... Type of MTA.
;;;                                        'smtp     ... Use smtp.el.
;;;                                        'sendmail ... Use sendmail.el.
;;;  gnus-offline-drafts-queue-type   ... Method type queuing message to spool.
;;;                                        'miee means queue message to spool
;;;                                         using miee.el.
;;;                                        'agent means queue message to spool
;;;                                         using gnus-agent.el.

;;; Code:

(eval '(run-hooks 'gnus-offline-load-hook))

(require 'cl)
(require 'custom)
(require 'pop3-fma)

(unless (and (condition-case ()
		 (require 'custom)
	       (file-error nil))
	     (fboundp 'defgroup)
	     (fboundp 'defcustom))
  (require 'backquote)
  (defmacro defgroup (&rest args))
  (defmacro defcustom (symbol value &optional doc &rest args)
    (` (defvar (, symbol) (, value) (, doc))))
  )
(defgroup gnus-offline nil
  "Offline backend utility for Gnus."
  :prefix "gnus-offline-"
  :group 'mail
  :group 'news)

(defconst gnus-offline-version-number "1.53")
(defconst gnus-offline-codename
;;  "You may be right"		; 1.40
;;  "Chilstie Lee"		; 1.45
;;  "Uptown Girl"		; 1.46
;;  "Easy money"		; 1.47
;;  "An Innocent man"		; 1.48
;;  "Tell her about it"		; 1.50
;;  "This night"		; 1.51
;;  "Movin'out"			; 1.52
  "Longest night"		; 1.53
;;  "Leave a tender moment alone"
;;  "Back in the U.S.S.R"
;;  "Running on ice"
;;  "This is the time"
;;  "A matter of trust"
;;  "Modern Woman"
;;  "Code of silence"
  )

(defconst gnus-offline-version (format "Gnus offline backend utiliy v%s"
				       gnus-offline-version-number))

(defcustom gnus-offline-connect-program nil
  "*Program name to dial-up dialup network.
If nil , use auto-dialup if required to connect the Internet."
  :group 'gnus-offline
  :type 'string)

(defcustom gnus-offline-connect-program-arguments nil
  "*Program arguments of gnus-offline-connect-program."
  :group 'gnus-offline
  :type '(repeat (string :tag "Argument")))

(defcustom gnus-offline-hangup-program nil
  "*Program name to hang-up dialup network."
  :group 'gnus-offline
  :type 'string)

(defcustom gnus-offline-hangup-program-arguments nil
  "*Program arguments of gnus-offline-hangup-program."
  :group 'gnus-offline
  :type '(repeat (string :tag "Argument")))

(defcustom gnus-offline-auto-hangup t
  "*Whether dialup-network automatically hang up when all online jobs has done."
  :group 'gnus-offline
  :type 'boolean)

(defcustom gnus-offline-mail-spool-directory "~/News/mail.out"
  "*Spool directory sending mail."
  :group 'gnus-offline
  :type 'directory)

(defcustom gnus-offline-news-spool-directory "~/News/news.out"
  "*Spool directory sending news."
  :group 'gnus-offline
  :type 'directory)

(defcustom gnus-offline-load-hook nil
  "*Hook to be run after the gnus-offline package has been loaded."
  :group 'gnus-offline
  :type 'hook)

(defcustom gnus-offline-before-online-hook nil
  "*Hook to be run before all online jobs."
  :group 'gnus-offline
  :type 'hook)

(defcustom gnus-offline-after-online-hook nil
  "*Hook to be run after all online jobs."
  :group 'gnus-offline
  :type 'hook)

(defcustom gnus-offline-mail-treat-environ 'offline
  "*If online , gnus-offline send all mail under online environ.
If offline , gnus-offline send all mail temporary to spool dir."
  :group 'gnus-offline
  :type '(choice (const offline)
		 (const online)))

(defcustom gnus-offline-articles-to-fetch 'both
  "*If both , gnus-offline fetch mail and news articles.
If mail , gnus-offline only fetch mail articles.
 If news , gnus-offline only fetch news articles."
  :group 'gnus-offline
  :type '(choice (const both)
		 (const mail)
		 (const news)))

(defcustom gnus-offline-interval-time 0
  "*Interval time(minutes) to do online jobs.
If set to 0 , timer call is disabled."
  :group 'gnus-offline
  :type 'integer)

(defcustom gnus-offline-mail-group-level 1
  "*Group level for mail group."
  :group 'gnus-offline
  :type 'integer)

(defcustom gnus-offline-MTA-type 'smtp
  "*Type of MTA program.
smtp means use smtp.el.
 sendmail means use sendmail.el."
  :group 'gnus-offline
  :type '(choice (const smtp)
		 (const sendmail)))

(defcustom gnus-offline-drafts-queue-type 'agent
  "*Type of to queue drafts method.
'miee means drafts are queued and sent by miee.el.
'agent means drafts are queued and sent by gnus-agent.el"
  :group 'gnus-offline
  :type '(choice (const miee)
		 (const agent)))

(defcustom gnus-offline-after-empting-spool-hook nil
  "*Hook to be run before empting spool."
  :group 'gnus-offline
  :type 'hook)

(defcustom gnus-offline-before-empting-spool-hook nil
  "*Hook to be run after empting spool."
  :group 'gnus-offline
  :type 'hook)

(defcustom gnus-offline-dialup-function 'gnus-offline-connect-server
  "*Function to dialup."
  :group 'gnus-offline
  :type 'function)

(defcustom gnus-offline-hangup-function 'gnus-offline-hangup-line
  "*Function to hangup."
  :group 'gnus-offline
  :type 'function)

;;; Internal variables.
(defvar gnus-offline-connected nil
  "*If value is t , dialup line is connected status.
If value is nil , dialup line is disconnected status.")

(defvar gnus-offline-news-fetch-method nil
  "*Method to fetch news articles.")

(defvar gnus-offline-mail-fetch-method nil
  "*Method to fetch mail articles.")

(defvar gnus-offline-header-string
  (format "%s - \"%s\""
	  gnus-offline-version
	  gnus-offline-codename)
  "*Header string for gnus-offline.")

(defvar gnus-offline-auto-hangup-indicator "Hup"
  "*Indicator whether auto hang up is enabled.")

(defvar gnus-offline-stored-group-level nil
  "*Mail Group level before changing.")

(defvar gnus-offline-movemail-arguments nil
  "*All command line arguments of exec-directory/movemail.")

;;; Temporary variable:
(defvar string)
(defvar hdr)
(defvar str)
(defvar passwd)
(defvar num)
(defvar gnus-offline-map (make-sparse-keymap))

(autoload 'message-offline-state "miee"
  "Set current status to offline state" t)
;;
;; mode-line control
(if (not (member 'gnus-offline-auto-hangup-indicator mode-line-format))
    (progn
      (delete "-%-" mode-line-format)
      (setq-default mode-line-format
		    (append mode-line-format
			    (list "--" 'gnus-offline-auto-hangup-indicator
				  "-%-")))))
(put 'gnus-offline-set-unplugged-state 'menu-enable 'gnus-offline-connected)
(add-hook 'gnus-startup-hook 'gnus-offline-setup)
;;; Functions
;;
;; Setting up...
;;
(defun gnus-offline-setup ()
  "*Initialize gnus-offline function"
  (if (eq system-type 'windows-nt)
      (define-process-argument-editing "/hang\\.exe\\'"
	(lambda (x) (general-process-argument-editing-function
		     x nil t t nil t t))))
  ;; Initialize Internal Variable
  (gnus-offline-initialize-variables)
  
  ;; Disable fetch mail when startup.
  (gnus-offline-disable-fetch-mail)
  
  ;; To transfer Mail/News function.
  (cond ((eq gnus-offline-mail-treat-environ 'offline)
	 ;; send mail under online environ.
	 (gnus-offline-set-offline-sendmail-function))
	((eq gnus-offline-mail-treat-environ 'online)
	 ;; send mail under offline environ.
	 (gnus-offline-set-online-sendmail-function)))

  ;; always treat news under offline environ.
  (gnus-offline-set-offline-post-news-function)
  
  ;; Spool directory setting - Miee
  (if (eq gnus-offline-drafts-queue-type 'miee)
      (progn
	(if (not (file-exists-p gnus-offline-mail-spool-directory))
	    (make-directory gnus-offline-mail-spool-directory t))
	(setq sendmail-to-spool-directory gnus-offline-mail-spool-directory)
	(if (not (file-exists-p gnus-offline-news-spool-directory))
	    (make-directory gnus-offline-news-spool-directory t))
	(setq news-spool-request-post-directory gnus-offline-news-spool-directory)))
  
  ;; When startup ... state is offline.
  (setq gnus-nntp-service nil
	gnus-nntp-server nil)
  
  ;; Setup needed Hooks
  (gnus-offline-setup-needed-hooks))
;;
;;
(defun gnus-offline-initialize-variables ()
  "*Initialize gnus-offline internal variable."
  (if (featurep 'nnmail)
      (setq gnus-offline-mail-fetch-method 'nnmail))
  (if (featurep 'gnus-agent)
      (setq gnus-offline-news-fetch-method 'nnagent))
  (if (featurep 'nnspool)
      (setq gnus-offline-news-fetch-method 'nnspool))
  (if (eq gnus-offline-drafts-queue-type 'miee)
      (load "miee"))
  (gnus-offline-define-menu-and-key))
;;
;;
(defun gnus-offline-set-offline-sendmail-function ()
  "*Initialize sendmail-function when unplugged status."
  (if (eq gnus-offline-drafts-queue-type 'miee)
      (setq message-send-mail-function 'sendmail-to-spool-in-gnspool-format)
    (setq message-send-mail-function 'gnus-agent-send-mail)))
;;
(defun gnus-offline-set-online-sendmail-function ()
  "*Initialize sendmail-function when plugged status."
  (if (eq gnus-offline-MTA-type 'smtp)
      (setq message-send-mail-function 'message-send-mail-with-smtp)
    (setq message-send-mail-function 'message-send-mail-with-sendmail)))
;;
(defun gnus-offline-set-offline-post-news-function ()
  "*Initialize sendnews-function when unplugged status."
  (if (eq gnus-offline-drafts-queue-type 'miee)
      (setq message-send-news-function 'gnspool-request-post)))
;;
(defun gnus-offline-set-online-post-news-function ()
  "*Initialize sendnews-function when plugged status."
  (setq message-send-news-function 'message-send-news-with-gnus))
;;
(defun gnus-offline-setup-needed-hooks ()
  "*Initialize needed hooks for gnus-offline."
  (add-hook 'gnus-group-mode-hook 'gnus-offline-processed-by-timer)
  (add-hook 'gnus-after-getting-new-news-hook 'gnus-offline-after-get-new-news)
  (add-hook 'gnus-after-getting-news-hook 'gnus-offline-after-get-new-news)
  (if (eq gnus-offline-news-fetch-method 'nnspool)
      (add-hook 'after-getting-news-hook 'gnus-offline-nnspool-hangup-line))
  (add-hook 'mime-edit-translate-hook 'gnus-offline-message-add-header)
  (if (featurep 'pop3-fma)
      (add-hook 'mime-edit-translate-hook 'pop3-fma-message-add-header)))
;;
;; Get new news jobs. (gnus-agent and nnspool)
;;
(defun gnus-offline-gnus-get-new-news (&optional arg)
  "*Override function \"gnus-grou-get-new-news\"."
  (interactive "P")
  (run-hooks 'gnus-offline-before-online-hook)
  (if (functionp gnus-offline-dialup-function)
      (funcall gnus-offline-dialup-function))
  (gnus-offline-get-new-news-function)
  (gnus-group-get-new-news arg))

;;
;; dialup...
;;
(defun gnus-offline-connect-server ()
  "*Dialup function."
  ;; Dialup if gnus-offline-connect-program is specified
  (if (stringp gnus-offline-connect-program)
      (progn
	(message "Dialing ...")
	(apply 'call-process gnus-offline-connect-program nil nil nil
	       gnus-offline-connect-program-arguments)
	(sleep-for 1)
	(message "Dialing ... done."))))

;;
;; Jobs before get new news , send mail and post news.
;;
(defun gnus-offline-get-new-news-function ()
  "*Prepare to get new news/mail."
  ;; Set mail group level
  (if (eq gnus-offline-articles-to-fetch 'mail)
      (gnus-offline-set-mail-group-level gnus-offline-mail-group-level))

  ;; Re initialize internal variable...if failed.
  (if (or (not gnus-offline-mail-fetch-method)
	  (not gnus-offline-news-fetch-method))
      (gnus-offline-initialize-variables))

  ;; Set to online environ.
  (setq gnus-offline-connected t)

  ;; Set send mail/news functions to online functions.
  (gnus-offline-set-online-sendmail-function)
  (gnus-offline-set-online-post-news-function)
  (message "Set to online status.")

  ;; fetch only news
  (if (eq gnus-offline-articles-to-fetch 'news)
      (gnus-offline-disable-fetch-mail))

  ;; fetch both mail and news. or Only mail.
  (gnus-offline-enable-fetch-news)
  (if (memq gnus-offline-articles-to-fetch '(both mail))
      (gnus-offline-enable-fetch-mail))

  ;; fetch only mail for gnus-agent
  (if (eq gnus-offline-news-fetch-method 'nnagent)
      (if (eq gnus-offline-articles-to-fetch 'mail)
	  (setq gnus-agent-handle-level gnus-offline-mail-group-level))))

;;
;; Change mail group level to handle only mail.
;;
(defun gnus-offline-set-mail-group-level (level)
  "*Set nnm* group level."
  (switch-to-buffer gnus-group-buffer)
  (goto-char (point-min))
  
  ;; Save current level
  (if (not gnus-offline-stored-group-level)
      (while (re-search-forward " nnm" nil t)
	(setq gnus-offline-stored-group-level
	      (append gnus-offline-stored-group-level
		      (list (gnus-group-group-level)))))
    (forward-line 1)
    (beginning-of-line))
  ;;
  (goto-char (point-min))
  (while (re-search-forward " nnm" nil t)
    (gnus-group-set-current-level 1 level)
    (forward-line 1)
    (beginning-of-line))
  t)
;;
;; Restore mail group level
;;
(defun gnus-offline-restore-mail-group-level ()
  "*Restore nnm* group level."
  (switch-to-buffer gnus-group-buffer)
  (goto-char (point-min))
  (setq num 0)
  (while (re-search-forward " nnm" nil t)
    (gnus-group-set-current-level 1 (nth num gnus-offline-stored-group-level))
    (forward-line 1)
    (setq num (+ num 1))
    (beginning-of-line)))
;;
;; Jobs after getting new news.
;;
(defun gnus-offline-after-get-new-news ()
  "*After getting news and mail jobs."
  (if (memq gnus-offline-articles-to-fetch '(both mail))
      (progn
	;; Mail/both
	;; send mail/news in spool
	(gnus-offline-empting-spool)
	(if (eq gnus-offline-articles-to-fetch 'mail)
	    (progn
	      ;; Send only mail and hang up...
	      (if (and gnus-offline-connected
		       gnus-offline-auto-hangup)
		  (gnus-offline-set-unplugged-state))
	      ;; Disable fetch mail.
	      (gnus-offline-disable-fetch-mail)
	      (gnus-offline-after-jobs-done)))))
  
  ;; News/Both
  (if (memq gnus-offline-articles-to-fetch '(both news))
      (progn
	(if gnus-offline-connected
	    (progn
	      (if (eq gnus-offline-news-fetch-method 'nnagent)
		  (progn
		    ;; Get New News (gnus-agent)
		    (gnus-agent-toggle-plugged t)

		    ;; fetch articles
		    (gnus-agent-fetch-session)

		    ;; Hang Up line. then set to offline status.
		    (if (and gnus-offline-connected
			     gnus-offline-auto-hangup)
			(gnus-offline-set-unplugged-state))

		    ;; All online jobs has done.
		    (gnus-offline-after-jobs-done)))
	      (if (eq gnus-offline-news-fetch-method 'nnspool)
		  ;; Get New News (nnspool)
		  (gnspool-get-news)))))))
;;
;; Disable fetch mail
;;
(defun gnus-offline-disable-fetch-mail ()
  "*Set do not fetch mail."
  (if (eq gnus-offline-mail-fetch-method 'nnmail)
      (setq nnmail-spool-file nil)))
;;
;; Enable fetch mail
;;
(defun gnus-offline-enable-fetch-mail ()
  "*Set to fetch mail."
  (if (eq gnus-offline-mail-fetch-method 'nnmail)
      (progn
	(setq gnus-offline-mail-fetch-method 'nnmail)
	(setq nnmail-movemail-program 'pop3-fma-movemail)
	(setq nnmail-spool-file (append
				 pop3-fma-local-spool-file-alist
				 (mapcar
				  (lambda (spool)
				    (car spool))
				  pop3-fma-spool-file-alist))))))
;;
;; Enable fetch news
;;
(defun gnus-offline-enable-fetch-news ()
  "*Set to fetch news."
  (if (eq gnus-offline-news-fetch-method 'nnagent)
      (progn
	(setq gnus-agent-handle-level gnus-level-subscribed)
	(gnus-agent-toggle-plugged t))))

;;
;; Add your custom header.
;;
(defun gnus-offline-add-custom-header (header string)
  "*Add X-Gnus-Offline-Backend header to Mail/News message."
  (let ((delimline
	 (progn (goto-char (point-min))
		(re-search-forward
		 (concat "^" (regexp-quote mail-header-separator) "\n"))
		(point-marker))))
    (goto-char (point-min))
    (or (re-search-forward (concat "^" header) delimline t)
	(progn
	  (goto-char delimline)
	  (forward-line -1)
	  (beginning-of-line)
	  (setq hdr (concat header " "))
	  (setq str (concat hdr string))
	  (setq hdr (concat str "\n"))
	  (insert-string hdr)))))
;;
;; Add X-Offline-Backend header.
;;
(defun gnus-offline-message-add-header ()
  "*Add X-Gnus-Offline-Backend header to Mail/News message."
  (if (eq gnus-offline-mail-treat-environ 'offline)
      (progn
	(if (eq gnus-offline-news-fetch-method 'nnagent)
	    (setq str (format "\n                        with %s" nnagent-version)
		  string (concat gnus-offline-header-string str))
	  (setq str (format "\n                        with %s" nnspool-version)
		string (concat gnus-offline-header-string str)))
	(gnus-offline-add-custom-header "X-Gnus-Offline-Backend:" string))))
  

;;
;; Toggle plugged/unplugged
;;
(defun gnus-offline-toggle-plugged (plugged)
  "*Override function \"Jj\" - gnus-agent-toggle-plugged."
  (interactive (list (not gnus-offline-connected)))
  (if plugged
      (progn
	(setq gnus-offline-connected plugged)
	(gnus-agent-toggle-plugged plugged)
	;; Set send mail/news function to offline functions.
	(gnus-offline-set-online-sendmail-function)
	(gnus-offline-set-online-post-news-function))
    ;; Set to offline status
    (gnus-offline-set-unplugged-state)))
;;
;; Function of hang up line.
;;
(defun gnus-offline-set-unplugged-state ()
  "*Set to unplugged state."
  (interactive)
  ;; Hang Up Line.
  (if (functionp gnus-offline-hangup-function)
      (funcall gnus-offline-hangup-function))
  (setq gnus-offline-connected nil)
  (gnus-agent-toggle-plugged nil)

  ;; Set send mail/news function to offline functions.
  (gnus-offline-set-offline-sendmail-function)
  (gnus-offline-set-offline-post-news-function)
  ;;
  (setenv "MAILHOST" nil))
;;
;; Hangup line function 
;;
(defun gnus-offline-hangup-line ()
  "*Hangup line function."
  (message "Hang up line ... ")
  (if (stringp gnus-offline-hangup-program)
      (apply 'start-process "hup" nil gnus-offline-hangup-program
	     gnus-offline-hangup-program-arguments))
  (message "Hang up line ... done."))
;;
;; Hang Up line routine whe using nnspool
;;
(defun gnus-offline-nnspool-hangup-line ()
  (if (and gnus-offline-connected
	   gnus-offline-auto-hangup)
      (gnus-offline-set-unplugged-state))
  (gnus-offline-after-jobs-done))
;;
;; Function of all jobs has done.
;;
(defun gnus-offline-after-jobs-done ()
  "*Jobs after all online jobs."
  (run-hooks 'gnus-offline-after-online-hook)
  (if (eq gnus-offline-articles-to-fetch 'mail)
      (gnus-offline-restore-mail-group-level))
  (if (eq gnus-offline-news-fetch-method 'nnagent)
      (gnus-offline-agent-expire))
  (ding)
  (message "All online jobs have done."))


;;
;; Toggle auto hang up
;;
(defun gnus-offline-toggle-auto-hangup ()
  "*Toggle auto hangup flag."
  (interactive)
  (setq string "Auto hang up logic")
  (if gnus-offline-auto-hangup
      (progn
	(setq gnus-offline-auto-hangup nil
	      gnus-offline-auto-hangup-indicator "Con"
	      str "disabled."))
    (setq gnus-offline-auto-hangup t
	  gnus-offline-auto-hangup-indicator "Hup"
	  str "enabled."))
  (message (format "%s %s" string str)))
;;
;; Toggle offline/online to send mail.
;;
(defun gnus-offline-toggle-on/off-send-mail ()
  "*Toggel online/offline sendmail."
  (interactive)
  (if (eq gnus-offline-mail-treat-environ 'offline)
      (progn
	;; Sending mail under online environ.
	(gnus-offline-set-online-sendmail-function)
	(setq gnus-offline-mail-treat-environ 'online)
	(message "Sending mail immidiately."))
    ;; Sending mail under offline environ.
    (gnus-offline-set-offline-sendmail-function)
    (setq gnus-offline-mail-treat-environ 'offline)
    (message "Sending mail temporary to spool directory.")))
;;
;; Toggle articles to fetch ... both -> mail -> news -> both
;;
(defun gnus-offline-toggle-articles-to-fetch ()
  "*Set articles to fetch... both(Mail/News) -> mail only -> News only -> both"
  (interactive)
  (setq string "Articles fetch from server.")
  (cond ((eq gnus-offline-articles-to-fetch 'both)
	 (setq gnus-offline-articles-to-fetch 'mail
	       str "Only Mail"))
	((eq gnus-offline-articles-to-fetch 'mail)
	   (setq gnus-offline-articles-to-fetch 'news
		 str "Only News"))
	(t
	 (setq gnus-offline-articles-to-fetch 'both
	       str "Mail/News both")))
  (message (format "%s %s" string str)))
;;
;; Toggle movemail program pop3.el -> movemail -> pop3.el
;;
(defun gnus-offline-toggle-movemail-program ()
  "*Toggle movemail program movemail -> pop3.el -> movemail ->..."
  (interactive)
  (setq string "Set nnmail-movemail-program")
  (cond ((eq pop3-fma-movemail-type 'lisp)
	 (setq pop3-fma-movemail-type 'exe
	       str "to movemail"))
	(t
	 (setq pop3-fma-movemail-type 'lisp
	       str "to pop3.el")))
  (message (format "%s %s" string str)))
;;
;; Send mail and Post news using Miee or gnus-agent.
;;
(defun gnus-offline-empting-spool ()
  "*Send all drafts on queue."
  (run-hooks 'gnus-offline-before-empting-spool-hook)
  (if (eq gnus-offline-drafts-queue-type 'miee)
      ;; Send queued message by miee.el.
      (progn
	(if (eq gnus-offline-mail-treat-environ 'offline)
	    (progn
	      (message "Sending mails in spool ...")
	      ;; Using miee to send mail.
	      (mail-spool-send)
	      (message "Sending mails in spool ... done.")))
	(message "Posting news in spool ...")
	;; Using miee to post news.
	(if (and (not (stringp msspool-news-server))
		 (not msspool-news-service))
	    (progn
	      (setq msspool-news-server (nth 1 gnus-select-method))
	      (setq msspool-news-service 119)))
	(news-spool-post)
	(message "Posting news in spool ... done."))
    ;; Send queued message by gnus-agent
    (message "Sending messages in spool ...")
    (gnus-group-send-drafts)
    (message "Sending messages in spool ... done."))
  ;;
  (run-hooks 'gnus-offline-after-empting-spool-hook))
;;
;; Set interval time
;;
(defun gnus-offline-set-interval-time ()
  "*Set interval time for gnus-daemon."
  (interactive)
  (setq gnus-offline-interval-time
	(string-to-int (read-from-minibuffer
			(format "Interval time (now %s minutes) : "
				gnus-offline-interval-time)
			nil)))
  (if (< gnus-offline-interval-time 2)
      (progn
	(message "Retrieving message logic by timer is disabled.")
	(setq gnus-offline-interval-time 0))
    (message (format "Interval time set to %d minutes" gnus-offline-interval-time)))
  (gnus-offline-processed-by-timer))
;;
;; Expire articles using gnus-agent.
;;
(defun gnus-offline-agent-expire ()
  "*Expire expirable article on News group."
  (interactive)
  (gnus-agent-expire))
;;
;; Menu.
;;
(defun gnus-offline-define-menu-and-key ()
  "*Set key and menu."
  (if (eq gnus-offline-drafts-queue-type 'miee)
      (add-hook 'gnus-group-mode-hook 'gnus-offline-define-menu-on-miee)
    (add-hook 'gnus-group-mode-hook 'gnus-offline-define-menu-on-agent))
  (add-hook 'gnus-group-mode-hook
	    '(lambda ()
	       (local-set-key "\C-coh" 'gnus-offline-set-unplugged-state)
	       (local-set-key "\C-com" 'gnus-offline-toggle-movemail-program)
	       (local-set-key "\C-cof" 'gnus-offline-toggle-articles-to-fetch)
	       (local-set-key "\C-coo" 'gnus-offline-toggle-on/off-send-mail)
	       (local-set-key "\C-cox" 'gnus-offline-toggle-auto-hangup)
	       (local-set-key "\C-cos" 'gnus-offline-set-interval-time)
	       (substitute-key-definition
		'gnus-group-get-new-news 'gnus-offline-gnus-get-new-news
		gnus-group-mode-map)
	       (if (eq gnus-offline-news-fetch-method 'nnagent)
		   (progn
		     (substitute-key-definition
		      'gnus-agent-toggle-plugged 'gnus-offline-toggle-plugged
		      gnus-agent-group-mode-map)
		     (local-set-key "\C-coe" 'gnus-offline-agent-expire)))))
  (if (eq gnus-offline-news-fetch-method 'nnagent)
      (add-hook 'gnus-summary-mode-hook
		'(lambda ()
		   (substitute-key-definition
		    'gnus-agent-toggle-plugged 'gnus-offline-toggle-plugged
		    gnus-agent-summary-mode-map)))))

(defun gnus-offline-define-menu-on-miee ()
  "*Set menu bar on MIEE menu."
  (global-set-key
   [menu-bar
    miee
    gnus-offline-hup-separator]
   '("--"))

  (global-set-key
   [menu-bar
    miee
    gnus-offline]
   (cons "Gnus Offline Utility"
	 (make-sparse-keymap "Gnus Offline Utiliry")))
  
  (if (featurep 'pop3-fma)
      (global-set-key
       [menu-bar
	miee
	gnus-offline
	gnus-offline-toggle-movemail-program]
       '("Toggle movemail program" .
	 gnus-offline-toggle-movemail-program)))
  
  (global-set-key
   [menu-bar
    miee
    gnus-offline
    gnus-offline-toggle-articles-to-fetch]
   '("Toggle articles to fetch" .
     gnus-offline-toggle-articles-to-fetch))
  
  (global-set-key
   [menu-bar
    miee
    gnus-offline
    gnus-offline-toggle-on/off-send-mail]
   '("Toggle online/offline send mail" .
     gnus-offline-toggle-on/off-send-mail))
  
  (global-set-key
   [menu-bar
    miee
    gnus-offline
    gnus-offline-toggle-auto-hangup]
   '("Toggle auto hang up" . gnus-offline-toggle-auto-hangup))
  
  (global-set-key
   [menu-bar
    miee
    gnus-offline
    gnus-offline-expire-separator]
   '("--"))
  
  (if (eq gnus-offline-news-fetch-method 'nnagent)
      (global-set-key
       [menu-bar
	miee
	gnus-offline
	gnus-offline-agent-expire]
       '("Expire articles" . gnus-offline-agent-expire)))
  
  (global-set-key
   [menu-bar
    miee
    gnus-offline
    gnus-offline-set-interval-time]
   '("Set interval time." . gnus-offline-set-interval-time))
  
  (global-set-key
   [menu-bar
    miee
    gnus-offline
    gnus-offline-hup-separator]
   '("--"))
  
  (global-set-key
   [menu-bar
    miee
    gnus-offline
    gnus-offline-set-unplugged-state]
   '("Hang Up Line." . gnus-offline-set-unplugged-state)))
;;
;; define menu without miee.
;;
(defun gnus-offline-define-menu-on-agent ()
  "*Set menu bar on OFFLINE menu."
  (define-key-after
    (lookup-key global-map [menu-bar])
    [offline]
    (cons "Offline" (make-sparse-keymap "Offline"))
    'help)               ;; Actually this adds before "Help".

  (if (featurep 'pop3-fma)
      (global-set-key
       [menu-bar
	offline
	gnus-offline-toggle-movemail-program]
       '("Toggle movemail program" . gnus-offline-toggle-movemail-program)))
  
  (global-set-key
   [menu-bar
    offline
    gnus-offline-toggle-articles-to-fetch]
   '("Toggle articles to fetch" . gnus-offline-toggle-articles-to-fetch))
  
  (global-set-key
   [menu-bar
    offline
    gnus-offline-toggle-on/off-send-mail]
   '("Toggle online/offline send mail" . gnus-offline-toggle-on/off-send-mail))
  
  (global-set-key
   [menu-bar
    offline
    gnus-offline-toggle-auto-hangup]
   '("Toggle auto hang up" . gnus-offline-toggle-auto-hangup))
  
  (global-set-key
   [menu-bar
    offline
    gnus-offline-separator]
   '("--"))
  
  (if (eq gnus-offline-news-fetch-method 'nnagent)
      (progn
	(global-set-key
	 [menu-bar
	  offline
	  gnus-offline-agent-expire]
	 '("Expire articles" . gnus-offline-agent-expire))))
  
  (global-set-key
   [menu-bar
    offline
    gnus-offline-set-interval-time]
   '("Set interval time." . gnus-offline-set-interval-time))
  
  (global-set-key
   [menu-bar
    offline
    gnus-offline-hup-separator]
   '("--"))
  
  (global-set-key
   [menu-bar
    offline
    gnus-offline-set-unplugged-state]
   '("Hang Up Line." . gnus-offline-set-unplugged-state)))

;;
;; Timer Function
(defun gnus-offline-processed-by-timer ()
  "*Set timer interval."
  (if (and (> gnus-offline-interval-time 0)
	   (not gnus-offline-connected))
      ;; Timer call
      (gnus-demon-add-handler 'gnus-offline-gnus-get-new-news
			      gnus-offline-interval-time
			      gnus-offline-interval-time))
  (if (= gnus-offline-interval-time 0)
      (gnus-demon-remove-handler 'gnus-offline-gnus-get-new-news t)))
;;
;;
(provide 'gnus-offline)
;;; gnus-offline.el ends here

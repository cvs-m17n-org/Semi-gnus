;;; gnus-offline.el --- To process mail & news at offline environment.
;;; $Id: gnus-offline.el,v 1.1.2.5.2.36.4.2 1999-08-20 23:20:32 czkmt Exp $

;;; Copyright (C) 1998 Tatsuya Ichikawa
;;;                    Yukihiro Ito
;;; Author: Tatsuya Ichikawa <t-ichi@po.shiojiri.ne.jp>
;;;         Yukihiro Ito <ito@rs.civil.tohoku.ac.jp>
;;;         Hidekazu Nakamura <u90121@uis-inf.co.jp>
;;;         Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>

;;; Version: 2.20
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
;;;   You must use T-gnus 6.12.0 or later.
;;;
;;; How to use.
;;;
;;; Add following code at the end in your .emacs
;;;
;;;    (load "gnus-ofsetup")
;;;    (gnus-setup-for-offline)
;;;
;;; If you use gnus-agent as souper , put gnus-agent setup code in you .gnus.el
;;;
;;; If you use nnspool as souper , put following code in your .emacs before
;;; gnus-offline setting.
;;;
;;; Then , put hang.exe in exec-path directory.
;;;
;;; In Gnus group buffer , type g to get all news and mail.
;;; Then send mail and news in spool directory.
;;;
;;; Variables.
;;;  gnus-offline-dialup-program-arguments
;;;                                   ... List of dialup program arguments.
;;;  gnus-offline-hangup-program-arguments
;;;                                   ... List of hangup program arguments.
;;;  gnus-offline-mail-treat-environ  ... toggle sending mail online/offline.
;;;  gnus-offline-articles-to-fetch   ... toggle fetch articles.
;;;                                        both->mail->news->both...
;;;  gnus-offline-load-hook           ... hook before gnus-offline load.
;;;  gnus-offline-before-online-hook  ... hook before all online jobs.
;;;  gnus-offline-after-online-hook   ... hook after all online jobs.
;;;  gnus-offline-interval-time       ... Interval time to do all online jobs.
;;;                                        (minutes)
;;;  gnus-offline-dialup-function     ... Function to diualup.
;;;  gnus-offline-hangup-function     ... Function to hangup.

;;; Code:

(eval '(run-hooks 'gnus-offline-load-hook))

(require 'cl)
(require 'custom)
(require 'easymenu)
(provide 'gnus-offline)

(defgroup gnus-offline nil
  "Offline backend utility for Gnus."
  :prefix "gnus-offline-"
  :group 'mail
  :group 'news)

(defconst gnus-offline-version-number "2.20")
(defconst gnus-offline-codename
;;  "Beta5"			; Beta
;;  "This is the time"		; 2.00
;;  "A matter of trust"
;;  "Modern Woman"
;;  "Ahhhhhhh!!"		; 2.10b1
  "Cup of life"                 ; 2.20
;;  "Code of silence"
  )

(defconst gnus-offline-version (format "Gnus offline backend utiliy v%s"
				       gnus-offline-version-number))

(eval-when-compile
  (mapc
   #'(lambda (symbol)
       (unless (boundp symbol)
	 (make-local-variable symbol)
	 (eval (list 'setq symbol nil))))
   '(nnagent-version
     nnspool-version
     msspool-news-server
     msspool-news-service
     miee-popup-menu
     gnus-group-toolbar))
  (mapc
   #'(lambda (symbol)
       (unless (fboundp symbol)
	 (defalias symbol 'ignore)))
   '(general-process-argument-editing-function
     define-process-argument-editing
     gnspool-get-news
     mail-spool-send
     news-spool-post)))

(put 'gnus-offline-set-unplugged-state 'menu-enable 'gnus-offline-connected)
(if (eq system-type 'windows-nt)
    (define-process-argument-editing "/hang\\.exe\\'"
      (lambda (x)
	(general-process-argument-editing-function
	 x nil t t nil t t))))

(defcustom gnus-offline-auto-hangup t
  "*Whether dialup-network automatically hang up when all online jobs has done."
  :group 'gnus-offline
  :type 'boolean)

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

(defcustom gnus-offline-mail-group-level 1
  "*Group level for mail group."
  :group 'gnus-offline
  :type 'integer)

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

;; These variables should be customized using `gnus-offline-customize',
;; not by `customize'.

(defvar gnus-offline-dialup-program nil
  "*Program name for dialup.")

(defvar gnus-offline-hangup-program nil
  "*Program name for hangup.")

(defvar gnus-offline-dialup-program-arguments nil
  "*Program arguments of gnus-offline-dialup-program.")

(defvar gnus-offline-hangup-program-arguments nil
  "*Program arguments of gnus-offline-hangup-program.")

(defvar gnus-offline-interval-time 0
  "*Interval time(minutes) to do online jobs.
If set to 0 , timer call is disabled.")

(defvar gnus-offline-drafts-queue-type 'agent
  "*Queuing function used for draft messages.")

(defvar gnus-offline-MTA-type 'smtp
  "*Type of MTA, sendmail or smtp.el.")

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

(defvar gnus-offline-stored-group-level nil
  "*Mail Group level before changing.")

(defvar gnus-offline-mail-source nil
  "*mail-sources save variable.")

;;; Functions
;;
;; Setting up...
;;
(defun gnus-offline-setup ()
  "*Initialize gnus-offline function"

  ;; Menu and keymap
  (gnus-offline-define-menu-and-key)
  
  ;; To transfer Mail/News function.
  (cond ((eq gnus-offline-mail-treat-environ 'offline)
	 ;; send mail under offline environ.
	 (gnus-offline-set-offline-sendmail-function))
	((eq gnus-offline-mail-treat-environ 'online)
	 ;; send mail under offline environ.
	 (gnus-offline-set-online-sendmail-function))))

;;
;; Setting Error check.
(defun gnus-offline-error-check ()
  ;; Check gnus-agent and nnspool setting.
  (let ((buffer " *Offline Error*"))
    (cond ((eq gnus-offline-news-fetch-method 'nnagent)
	   ;; nnagent and gnus-agent loaded ??
	   (unless (and (featurep 'gnus-agent)
			(featurep 'nnagent))
	     (set-buffer (gnus-get-buffer-create buffer))
	     (erase-buffer)
	     (insert "WARNING!!: gnus-agent.el or nnagent.el is not loaded.
Please check your .emacs or .gnus.el to work gnus-agent fine.")
	     (pop-to-buffer buffer)))
	
	  ((eq gnus-offline-news-fetch-method 'nnspool)
	   (unless (featurep 'nnspool)
	     (set-buffer (gnus-get-buffer-create buffer))
	     (erase-buffer)
	     (insert "WARNING!!: nnspool.el is not loaded.
Please check your .emacs or .gnus.el to work nnspool fine.")
	     (pop-to-buffer buffer)))
	  (t
	   nil))))
;;
;;
(defun gnus-offline-set-offline-sendmail-function ()
  "*Initialize sendmail-function when unplugged status."
  (cond ((eq gnus-offline-drafts-queue-type 'miee)
	 (if (eq gnus-offline-news-fetch-method 'nnagent)
	     (setq gnus-agent-send-mail-function
		   'sendmail-to-spool-in-gnspool-format))
	 (setq message-send-mail-function 'sendmail-to-spool-in-gnspool-format))
	(t
	 (setq gnus-agent-send-mail-function
	       (gnus-offline-set-online-sendmail-function)
	       message-send-mail-function 'gnus-agent-send-mail))))
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
;; Get new news jobs. (gnus-agent and nnspool)
;;
(defun gnus-offline-gnus-get-new-news (&optional arg)
  "*Override function \"gnus-group-get-new-news\"."
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
  ;; Dialup if gnus-offline-dialup-program is specified
  (if (stringp gnus-offline-dialup-program)
      (progn
	(message "Dialing ...")
	(apply 'call-process gnus-offline-dialup-program nil nil nil
	       gnus-offline-dialup-program-arguments)
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
  (if (and (eq gnus-offline-news-fetch-method 'nnagent)
	   (eq gnus-offline-articles-to-fetch 'mail))
	  (setq gnus-agent-handle-level gnus-offline-mail-group-level)))

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
  (let ((num 0))
    (while (re-search-forward " nnm" nil t)
      (gnus-group-set-current-level 1 (nth num gnus-offline-stored-group-level))
      (forward-line 1)
      (setq num (+ num 1))
      (beginning-of-line))))
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
	    (cond ((eq gnus-offline-news-fetch-method 'nnagent)
		   ;; Get New News (gnus-agent)
		   (gnus-agent-toggle-plugged t)
		  
		   ;; fetch articles
		   (gnus-agent-fetch-session)
		  
		   ;; Hang Up line. then set to offline status.
		   (if (and gnus-offline-connected
			    gnus-offline-auto-hangup)
		       (gnus-offline-set-unplugged-state))
		   
		   ;; All online jobs has done.
		   (gnus-offline-after-jobs-done))
		  (t
		   (if (eq gnus-offline-news-fetch-method 'nnspool)
		       ;; Get New News (nnspool)
		       (gnspool-get-news))))))))
;;
;; Disable fetch mail
;;
(defun gnus-offline-disable-fetch-mail ()
  "*Set do not fetch mail."
  (setq mail-sources nil
	nnmail-spool-file nil))
;;
;; Enable fetch mail
;;
(defun gnus-offline-enable-fetch-mail ()
  "*Set to fetch mail."
  (setq gnus-offline-mail-fetch-method 'nnmail)
  (setq mail-sources gnus-offline-mail-source))
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
		(point-marker)))
	hdr str)
    (goto-char (point-min))
    (unless (re-search-forward (concat "^" header) delimline t)
      (goto-char delimline)
      (forward-line -1)
      (beginning-of-line)
      (setq hdr (concat header " "))
      (setq str (concat hdr string))
      (setq hdr (concat str "\n"))
      (insert-string hdr))))
;;
;; Add X-Offline-Backend header.
;;
(defun gnus-offline-message-add-header ()
  "*Add X-Gnus-Offline-Backend header to Mail/News message."
  (when (eq gnus-offline-mail-treat-environ 'offline)
    (let* ((ver (if (eq gnus-offline-news-fetch-method 'nnagent)
		    nnagent-version
		  nnspool-version))
	   (str (format "\n                        with %s" ver)))
    (gnus-offline-add-custom-header
     "X-Gnus-Offline-Backend:" (concat gnus-offline-header-string str)))))


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
  (if (eq gnus-offline-news-fetch-method 'nnagent)
      (gnus-agent-toggle-plugged nil))

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
      (or gnus-agent-expire-all
	  (gnus-offline-agent-expire)))
  (if (and (featurep 'xemacs)
	   (fboundp 'play-sound-file))
      (ding nil 'drum)
    (ding))
  (gnus-group-save-newsrc)
  (message "All online jobs has done."))


;;
;; Toggle auto hang up
;;
(defun gnus-offline-toggle-auto-hangup ()
  "*Toggle auto hangup flag."
  (interactive)
  (let ((string "Auto hang up logic") str)
    (if gnus-offline-auto-hangup
	(progn
	  (setq gnus-offline-auto-hangup nil
		str "disabled."))
      (setq gnus-offline-auto-hangup t
	    str "enabled."))
    (message (format "%s %s" string str))))
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
  (let ((string "Articles fetch from server.") str)
    (cond ((eq gnus-offline-articles-to-fetch 'both)
	   (setq gnus-offline-articles-to-fetch 'mail
		 str "Only Mail"))
	  ((eq gnus-offline-articles-to-fetch 'mail)
	   (setq gnus-offline-articles-to-fetch 'news
		 str "Only News"))
	  (t
	   (setq gnus-offline-articles-to-fetch 'both
		 str "Mail/News both")))
    (message (format "%s %s" string str))))
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
    (message
     (format "Interval time set to %d minutes" gnus-offline-interval-time)))
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
      (if (featurep 'xemacs)
	  (add-hook 'gnus-group-mode-hook 'gnus-offline-define-menu-on-miee)
	(gnus-offline-define-menu-on-miee))
    (add-hook 'gnus-group-mode-hook 'gnus-offline-define-menu-on-agent))
  (add-hook 'gnus-group-mode-hook
	    '(lambda ()
	       (local-set-key "\C-coh" 'gnus-offline-set-unplugged-state)
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
		     (local-set-key "\C-coe" 'gnus-offline-agent-expire)))
	       (or (featurep 'xemacs)
		   (define-key gnus-group-mode-map 
		     (if (eq system-type 'windows-nt) [S-mouse-2] [mouse-3])
		     'gnus-offline-popup-menu))))
  (if (eq gnus-offline-news-fetch-method 'nnagent)
      (add-hook 'gnus-summary-mode-hook
		'(lambda ()
		   (substitute-key-definition
		    'gnus-agent-toggle-plugged 'gnus-offline-toggle-plugged
		    gnus-agent-summary-mode-map))))
  (if (featurep 'xemacs)
      ;; Overwrite the toolbar spec for gnus-group-mode.
      (add-hook 'gnus-startup-hook
		(lambda ()
		  (catch 'tag
		    (mapc (lambda (but)
			    (when (eq 'gnus-group-get-new-news (aref but 1))
			      (aset but 1 'gnus-offline-gnus-get-new-news)
			      (throw 'tag nil)))
			  gnus-group-toolbar))))))
;;
;;
(defun gnus-offline-define-menu-on-miee ()
  "*Set and change menu bar on MIEE menu."
  (let (menu)
    (if (featurep 'meadow)
	(easy-menu-change
	 nil
	 "Miee"
	 '(
	   ["Spool $B$K$"$k5-;v$NAw?.(B" news-spool-post t]
	   ["Spool $B$K$"$k(B Mail $B$NAw?.(B" mail-spool-send t]
	   "----"
	   ["Offline $B>uBV$X(B" message-offline-state (not message-offline-state)]
	   ["Online $B>uBV$X(B" message-online-state message-offline-state]
	   "----"
	   ("Gnus Offline"
	    ["$B<hF@5-;v<oN`$NJQ99(B" gnus-offline-toggle-articles-to-fetch t]
	    ["Mail $BAw?.J}K!(B(On/Off)$B$N@ZBX$((B"
	     gnus-offline-toggle-on/off-send-mail t]
	  ["$B<+F0@ZCG$N@ZBX$((B" gnus-offline-toggle-auto-hangup t]
	  "----"
	  ["$B<hF@:Q5-;v$r>C$9(B" gnus-offline-agent-expire
	   (eq gnus-offline-news-fetch-method 'nnagent)]
	  ["$B5-;v<hF@4V3V;~4V$N@_Dj(B" gnus-offline-set-interval-time t]
	  "----"
	  ["$B2s@~$N@ZCG(B" gnus-offline-set-unplugged-state gnus-offline-connected]
	  "----"
	  ["$B%W%m%Q%F%#(B..." gnus-ofsetup-customize t])
	 ))
      (setq menu
	    (easy-menu-change
	     nil
	     "Miee"
	     '(
	       ["Post news in spool" news-spool-post t]
	       ["Send mails in spool" mail-spool-send t]
	       "----"
	       ["Message Offline" message-offline-state
		(not message-offline-state)]
	       ["Message Online" message-online-state message-offline-state]
	       "----"
	       ("Gnus Offline"
		["Toggle articles to fetch"
		 gnus-offline-toggle-articles-to-fetch t]
		["Toggle online/offline send mail"
		 gnus-offline-toggle-on/off-send-mail t]
		["Toggle auto hangup" gnus-offline-toggle-auto-hangup t]
		"----"
		["Expire articles" gnus-offline-agent-expire
		 (eq gnus-offline-news-fetch-method 'nnagent)]
		["Set interval time" gnus-offline-set-interval-time t]
		"----"
		["Hang up Line." gnus-offline-set-unplugged-state
		 gnus-offline-connected]
		"----"
		["Customize options..." gnus-ofsetup-customize t]
		)))))
    (and (featurep 'xemacs)
	 (easy-menu-add menu))))
;;
;; define menu without miee.
;;
(defun gnus-offline-define-menu-on-agent ()
  "*Set menu bar on OFFLINE menu."
  (easy-menu-define 
   gnus-offline-menu-on-agent
   gnus-group-mode-map
   "Gnus offline Menu"
   (if (featurep 'meadow)
       '("Offline"
	 ["$B<hF@5-;v<oN`$NJQ99(B" gnus-offline-toggle-articles-to-fetch t]
	 ["Mail $BAw?.J}K!(B(On/Off)$B$N@ZBX$((B" gnus-offline-toggle-on/off-send-mail
	  t]
	 ["$B<+F0@ZCG$N@ZBX$((B" gnus-offline-toggle-auto-hangup t]
	 "----"
	 ["$B<hF@:Q5-;v$r>C$9(B" gnus-offline-agent-expire
	  (eq gnus-offline-news-fetch-method 'nnagent)]
	 ["$B5-;v<hF@4V3V;~4V$N@_Dj(B" gnus-offline-set-interval-time t]
	 "----"
	 ["$B2s@~$N@ZCG(B" gnus-offline-set-unplugged-state gnus-offline-connected]
	 "----"
	 ["$B%W%m%Q%F%#(B..." gnus-ofsetup-customize t])
     '("Offline"
       ["Toggle articles to fetch" gnus-offline-toggle-articles-to-fetch t]
       ["Toggle online/offline send mail" gnus-offline-toggle-on/off-send-mail
	t]
       ["Toggle auto hangup" gnus-offline-toggle-auto-hangup t]
       "----"
       ["Expire articles" gnus-offline-agent-expire
	(eq gnus-offline-news-fetch-method 'nnagent)]
       ["Set interval time" gnus-offline-set-interval-time t]
       "----"
       ["Hang up Line." gnus-offline-set-unplugged-state gnus-offline-connected]
       "----"
       ["Customize options..." gnus-ofsetup-customize t])))
  (and (featurep 'xemacs)
       (easy-menu-add gnus-offline-menu-on-agent)))
;;
;; Popup menu within the group buffer (under Emacs).
;;
(defvar gnus-offline-popup-menu nil)
(defun gnus-offline-popup-menu (event)
  "Popup menu for Gnus offline."
  (interactive "e")
  (unless gnus-offline-popup-menu
    (setq gnus-offline-popup-menu
	  (let ((menu
		 (if (boundp 'miee-popup-menu)
		     (or (assq 'keymap
				(assq 'Miee (assq 'menu-bar global-map)))
			 miee-popup-menu)
		   (symbol-value 'gnus-offline-menu-on-agent))))
	    (if (string< emacs-version "20")
		(append (list 'keymap
			      (if (boundp 'miee-popup-menu)
				  '(nil "Miee")
				'(nil "Offline"))
			      '(nil "")
			      '(nil ""))
			(cdr menu))
	      menu))))
  (let* ((pop (x-popup-menu t gnus-offline-popup-menu))
	 (func (and pop (lookup-key gnus-offline-popup-menu
				    (apply 'vector pop)))))
    (and pop func (funcall func))))

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
;;; gnus-offline.el ends here

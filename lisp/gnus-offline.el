;;; gnus-offline.el --- To process mail & news at offline environment.
;;; $Id: gnus-offline.el,v 1.1.4.7 1999-02-01 06:45:20 ichikawa Exp $

;;; Copyright (C) 1998 Tatsuya Ichikawa
;;;                    Yukihiro Ito
;;; Author: Tatsuya Ichikawa <t-ichi@po.shiojiri.ne.jp>
;;;         Yukihiro Ito <ito@rs.civil.tohoku.ac.jp>
;;;         Hidekazu Nakamura <u90121@uis-inf.co.jp>
;;;         Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>

;;; Version: 2.10
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
;;;
;;; Add following code at the end in your .emacs
;;;
;;;    (load "gnus-ofsetup")
;;;    (gnus-setup-for-offline)
;;;    (load gnus-offline-setting-file)
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
;;; Security Notice. (This is available before version 2.02)
;;;
;;; You can set the variable gnus-offline-pop-password-file to save your POP
;;; passwords. But TAKE CARE. Use it at your own risk.
;;; If you decide to use it, then write in .emacs or .gnus-offline.el 
;;; something like:
;;;
;;;  (setq gnus-offline-pop-password-file "~/.pop.passwd")
;;;
;;; and write in this file something like:
;;;
;;;  (setq pop3-fma-password
;;;	 '(("SERVER1" "ACCOUNT1" "PASSWORD1")
;;;	   ("SERVER2" "ACCOUNT2" "PASSWORD2")
;;;        ............................
;;;        ))
;;;
;;; If you want to encode the file with base64, try:
;;;
;;;    M-: (base64-encode-region (point-min) (point-max))
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
;;;  gnus-offline-pop-password-file   ... File to keep the POP password info.
;;;  gnus-offline-pop-password-decoding-function
;;;                                   ... Function to decode the password info.

;;; Code:

(eval '(run-hooks 'gnus-offline-load-hook))

(require 'cl)
(require 'custom)
(require 'easymenu)

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

(defconst gnus-offline-version-number "2.10b1")
(defconst gnus-offline-codename
;;  "Beta5"			; Beta
;;  "This is the time"		; 2.00
;;  "A matter of trust"
;;  "Modern Woman"
  "Ahhhhhhh!!"			; 2.10b1
;;  "Code of silence"
  )

(defconst gnus-offline-version (format "Gnus offline backend utiliy v%s"
				       gnus-offline-version-number))

(defcustom gnus-offline-dialup-program-arguments nil
  "*Program arguments of gnus-offline-dialup-program."
  :group 'gnus-offline
  :type '(repeat (string :tag "Argument")))

(defcustom gnus-offline-hangup-program-arguments nil
  "*Program arguments of gnus-offline-hangup-program."
  :group 'gnus-offline
  :type '(repeat (string :tag "Argument")))

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

(defcustom gnus-offline-interval-time 0
  "*Interval time(minutes) to do online jobs.
If set to 0 , timer call is disabled."
  :group 'gnus-offline
  :type 'integer)

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

(defcustom gnus-offline-pop-password-file nil
  "*File name for saving one's POP password information.
This variable should be nil if there's some possibility that
your passwords be stolen."
  :group 'gnus-offline
  :type '(choice (file :tag "File")
		 (const nil)))

(defcustom gnus-offline-pop-password-decoding-function 
  (function (lambda () (base64-decode-region (point-min) (point-max))))
  "*Function for decoding one's password information.
The value has no effect when `gnus-offline-pop-password-file'
is nil.
This variable might be nil if you don't need to encode your passwords."
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

(defvar gnus-offline-stored-group-level nil
  "*Mail Group level before changing.")

(defvar gnus-offline-movemail-arguments nil
  "*All command line arguments of exec-directory/movemail.")

(defvar gnus-offline-mail-source nil
  "*nnmail-spool-file save variable.")

;;; Temporary variable:
(defvar string)
(defvar hdr)
(defvar str)
(defvar ver)
(defvar passwd)
(defvar num)
(defvar gnus-offline-error-buffer " *Error*")
(defvar gnus-offline-map (make-sparse-keymap))

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
       
(put 'gnus-offline-set-unplugged-state 'menu-enable 'gnus-offline-connected)
(if (eq system-type 'windows-nt)
    (define-process-argument-editing "/hang\\.exe\\'"
      (lambda (x) (general-process-argument-editing-function
		   x nil t t nil t t))))
;;; Functions
;;
;; Setting up...
;;
(defun gnus-offline-setup ()
  "*Initialize gnus-offline function"

  ;; Load setting file - required.
  (load gnus-offline-setting-file)

  ;; Menu and keymap
  (gnus-offline-define-menu-and-key)
  
  ;; To transfer Mail/News function.
  (cond ((eq gnus-offline-mail-treat-environ 'offline)
	 ;; send mail under offline environ.
	 (gnus-offline-set-offline-sendmail-function))
	((eq gnus-offline-mail-treat-environ 'online)
	 ;; send mail under offline environ.
	 (gnus-offline-set-online-sendmail-function))))
;;  (add-hook 'gnus-group-mode-hook 'gnus-offline-setup))

;;
;; Setting Error check.
(defun gnus-offline-error-check ()
  ;; Check gnus-agent and nnspool setting.
  (cond ((eq gnus-offline-news-fetch-method 'nnagent)
	 ;; nnagent and gnus-agent loaded ??
	 (if (not (and (featurep 'gnus-agent)
		       (featurep 'nnagent)))
	     (progn
	       (get-buffer-create gnus-offline-error-buffer)
	       (set-buffer gnus-offline-error-buffer)
	       (erase-buffer)
	       (insert "WARNING!!: gnus-agent.el or nnagent.el is not loaded.\n")
	       (insert "Please check your .emacs or .gnus.el to work gnus-agent fine.")
	       (pop-to-buffer gnus-offline-error-buffer))))
	
	((eq gnus-offline-news-fetch-method 'nnspool)
	 (if (not (featurep 'nnspool))
	     (progn
	       (get-buffer-create gnus-offline-error-buffer)
	       (set-buffer gnus-offline-error-buffer)
	       (erase-buffer)
	       (insert "WARNING!!: nnspool.el is not loaded.\n")
	       (insert "Please check your .emacs or .gnus.el to work nnspool fine.")
	       (pop-to-buffer gnus-offline-error-buffer))))))
;;
;;
(defun gnus-offline-set-offline-sendmail-function ()
  "*Initialize sendmail-function when unplugged status."
  (if (eq gnus-offline-drafts-queue-type 'miee)
      (progn
	(if (eq gnus-offline-news-fetch-method 'nnagent)
	    (setq gnus-agent-send-mail-function 'sendmail-to-spool-in-gnspool-format))
	(setq message-send-mail-function 'sendmail-to-spool-in-gnspool-format))
    (setq gnus-agent-send-mail-function (gnus-offline-set-online-sendmail-function)
	  message-send-mail-function 'gnus-agent-send-mail)))
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
  (if (not (locate-library "mail-source"))
      (progn
	(let (buffer)
	  (unwind-protect
	      (progn
		(save-excursion
		  (or pop3-fma-password
		      (when gnus-offline-pop-password-file
			(setq pop3-fma-save-password-information t)
			(setq buffer (get-buffer-create "*offline-temp*"))
			(set-buffer buffer)
			(erase-buffer)
			(insert-file-contents-as-binary gnus-offline-pop-password-file)
			(and gnus-offline-pop-password-decoding-function
			     (funcall gnus-offline-pop-password-decoding-function))
			(eval-buffer))))
		(gnus-group-get-new-news arg))
	    (when gnus-offline-pop-password-file
	      (setq pop3-fma-password nil)
	      (setq pop3-fma-save-password-information nil)
	      (kill-buffer buffer)))))
    ;;
    ;; Use mail-source.el
    (gnus-group-get-new-news arg)))

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
  (setq nnmail-spool-file nil))
;;
;; Enable fetch mail
;;
(defun gnus-offline-enable-fetch-mail ()
  "*Set to fetch mail."
  (setq gnus-offline-mail-fetch-method 'nnmail)
  (if (not (locate-library "mail-source"))
      (progn
	(setq nnmail-movemail-program 'pop3-fma-movemail)
	(setq nnmail-spool-file (append
				 pop3-fma-local-spool-file-alist
				 (mapcar
				  (lambda (spool)
				    (car spool))
				  pop3-fma-spool-file-alist))))
    (setq nnmail-spool-file gnus-offline-mail-source)))
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
	    (setq ver nnagent-version)
	  (setq ver nnspool-version))
	(setq str (format "\n                        with %s" ver)
	      string (concat gnus-offline-header-string str))
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
  (setq string "Auto hang up logic")
  (if gnus-offline-auto-hangup
      (progn
	(setq gnus-offline-auto-hangup nil
	      str "disabled."))
    (setq gnus-offline-auto-hangup t
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
      (if (featurep 'xemacs)
	  (add-hook 'gnus-group-mode-hook 'gnus-offline-define-menu-on-miee)
	(gnus-offline-define-menu-on-miee))
    (add-hook 'gnus-group-mode-hook 'gnus-offline-define-menu-on-agent))
  (add-hook 'gnus-group-mode-hook
	    '(lambda ()
	       (local-set-key "\C-coh" 'gnus-offline-set-unplugged-state)
	       (if (not (locate-library "mail-source"))
		   (local-set-key "\C-com" 'gnus-offline-toggle-movemail-program))
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
		  (let ((i 0) (stat t) but)
		    (while (and stat (setq but (nth i gnus-group-toolbar)))
		      (and (equal 'gnus-group-get-new-news (aref but 1))
			   (aset but 1 'gnus-offline-gnus-get-new-news)
			   (setq stat nil))
		      (setq i (1+ i))))))))
;;
;;
(defun gnus-offline-define-menu-on-miee ()
  "*Set and change menu bar on MIEE menu."
  (let ((menu
  (if (featurep 'meadow)
      (easy-menu-change
       nil
       "Miee"
       '(
	 ["Spool にある記事の送信" news-spool-post t]
	 ["Spool にある Mail の送信" mail-spool-send t]
	 "----"
	 ["Offline 状態へ" message-offline-state (not message-offline-state)]
	 ["Online 状態へ" message-online-state message-offline-state]
	 "----"
	 ("Gnus Offline"
	  ["movemail の切替え" gnus-offline-toggle-movemail-program
	   (not (locate-library "mail-source"))]
	  ["取得記事種類の変更" gnus-offline-toggle-articles-to-fetch t]
	  ["Mail 送信方法(On/Off)の切替え" gnus-offline-toggle-on/off-send-mail t]
	  ["自動切断の切替え" gnus-offline-toggle-auto-hangup t]
	  "----"
	  ["取得済記事を消す" gnus-offline-agent-expire (eq gnus-offline-news-fetch-method 'nnagent)]
	  ["記事取得間隔時間の設定" gnus-offline-set-interval-time t]
	  "----"
	  ["回線の切断" gnus-offline-set-unplugged-state gnus-offline-connected])
	 ))
    (easy-menu-change
     nil
     "Miee"
     '(
       ["Post news in spool" news-spool-post t]
       ["Send mails in spool" mail-spool-send t]
       "----"
       ["Message Offline" message-offline-state (not message-offline-state)]
       ["Message Online" message-online-state message-offline-state]
       "----"
       ("Gnus Offline"
	["Toggle movemail program" gnus-offline-toggle-movemail-program
	 (not (locate-library "mail-source"))]
	["Toggle articles to fetch" gnus-offline-toggle-articles-to-fetch t]
	["Toggle online/offline send mail" gnus-offline-toggle-on/off-send-mail t]
	["Toggle auto hangup" gnus-offline-toggle-auto-hangup t]
	"----"
	["Expire articles" gnus-offline-agent-expire (eq gnus-offline-news-fetch-method 'nnagent)]
	["Set interval time" gnus-offline-set-interval-time t]
	"----"
	["Hang up Line." gnus-offline-set-unplugged-state gnus-offline-connected]
	))))))
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
	 ["movemail の切替え" gnus-offline-toggle-movemail-program
	  (not (locate-library "mail-source"))]
	 ["取得記事種類の変更" gnus-offline-toggle-articles-to-fetch t]
	 ["Mail 送信方法(On/Off)の切替え" gnus-offline-toggle-on/off-send-mail t]
	 ["自動切断の切替え" gnus-offline-toggle-auto-hangup t]
	 "----"
	 ["取得済記事を消す" gnus-offline-agent-expire (eq gnus-offline-news-fetch-method 'nnagent)]
	 ["記事取得間隔時間の設定" gnus-offline-set-interval-time t]
	 "----"
	 ["回線の切断" gnus-offline-set-unplugged-state gnus-offline-connected])
     '("Offline"
       ["Toggle movemail program" gnus-offline-toggle-movemail-program
	(not (locate-library "mail-source"))]
       ["Toggle articles to fetch" gnus-offline-toggle-articles-to-fetch t]
       ["Toggle online/offline send mail" gnus-offline-toggle-on/off-send-mail t]
       ["Toggle auto hangup" gnus-offline-toggle-auto-hangup t]
       "----"
       ["Expire articles" gnus-offline-agent-expire (eq gnus-offline-news-fetch-method 'nnagent)]
       ["Set interval time" gnus-offline-set-interval-time t]
       "----"
       ["Hang up Line." gnus-offline-set-unplugged-state gnus-offline-connected])))
  (and (featurep 'xemacs)
       (easy-menu-add gnus-offline-menu-on-agent)))
;;
;; Popup menu within the group buffer (under Emacs).
;;
(defun gnus-offline-popup-menu (event)
  "Popup menu for Gnus offline."
  (interactive "e")
  (let* ((menu (if (boundp 'miee-popup-menu)
		   (or (assoc 'keymap
			      (assoc 'Miee (assoc 'menu-bar global-map)))
		       miee-popup-menu)
		 gnus-offline-menu-on-agent))
	 (pop (x-popup-menu t menu))
	 (func (and pop (lookup-key menu (apply 'vector pop)))))
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
(provide 'gnus-offline)
;;; gnus-offline.el ends here

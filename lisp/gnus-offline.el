;;; gnus-offline.el --- To process mail & news at offline environment.

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

(eval-when-compile (require 'cl) (require 'static))
(require 'custom)
(require 'easymenu)
(provide 'gnus-offline)

(eval-after-load "eword-decode"
  '(mime-set-field-decoder 'X-Gnus-Offline-Backend nil nil))

(defgroup gnus-offline nil
  "Offline backend utility for Gnus."
  :prefix "gnus-offline-"
  :group 'gnus
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
   (lambda (symbol)
     (unless (boundp symbol)
       (make-local-variable symbol)
       (eval (list 'setq symbol nil))))
   '(nnagent-version
     nnspool-version
     msspool-news-server
     msspool-news-service
     miee-popup-menu
     gnus-group-toolbar)))

(static-if (eq system-type 'windows-nt)
    (define-process-argument-editing "/hang\\.exe\\'"
      (lambda (x)
	(general-process-argument-editing-function
	 x nil t t nil t t))))

(defcustom gnus-offline-auto-ppp '(connect disconnect)
  "*This variable decides whether to connect and/or disconnect automatically."
  :group 'gnus-offline
  :type '(choice
	  (const :tag "Connection and Disconnection" (connect disconnect))
	  (const :tag "Connection Only" (connect))
	  (const :tag "Do Everything Manually" nil)))

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

(defcustom gnus-offline-agent-automatic-expire t
  "*Non-nil means expire articles on every session."
  :group 'gnus-offline
  :type 'boolean)

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

(defvar gnus-offline-lang)

(defvar gnus-offline-resource-en
  '((error-check-1
     . "WARNING!!: gnus-agent.el or nnagent.el is not loaded.
Please check your .emacs or .gnus.el to work gnus-agent fine.")
    (error-check-2 ."WARNING!!: nnspool.el is not loaded.
Please check your .emacs or .gnus.el to work nnspool fine.")
    (connect-server-1 . "Dialing ...")
    (connect-server-2 . "Dialing ... done.")
    (get-new-news-function-1 . "Set to online status.")
    (hangup-line-1 . "Hang up line ... ")
    (hangup-line-2 . "Hang up line ... done.")
    (after-jobs-done-1 . "All online jobs has done.")
    (set-auto-ppp-1 . "Connect and disconnect automatically.")
    (set-auto-ppp-2 . "Connect automatically.")
    (set-auto-ppp-3 . "Connect and disconnect manually.")
    (set-auto-ppp-menu-1 . "Automatically Connect/Disconnect")
    (set-auto-ppp-menu-2 . "Automatically Connect")
    (set-auto-ppp-menu-3 . "Manually Connect/Disconnect")
    (toggle-on/off-send-mail-1 . "Sending mail immidiately.")
    (toggle-on/off-send-mail-2 . "Sending mail temporary to spool directory.")
    (toggle-articles-to-fetch-1 . "Articles fetch from server.")
    (toggle-articles-to-fetch-2 . "Only Mail")
    (toggle-articles-to-fetch-3 . "Only News")
    (toggle-articles-to-fetch-4 . "Mail/News both")
    (empting-spool-1 . "Sending mails in spool ...")
    (empting-spool-2 . "Sending mails in spool ... done.")
    (empting-spool-3 . "Posting news in spool ...")
    (empting-spool-4 . "Posting news in spool ... done.")
    (empting-spool-5 . "Sending messages in spool ...")
    (empting-spool-6 . "Sending messages in spool ... done.")
    (interval-time-1 . "Interval time (now %d minutes) : ")
    (interval-time-2 . "Retrieving message logic by timer is disabled.")
    (interval-time-3 . "Interval time set to %d minutes")
    (menu-miee-1 . "Post news in spool")
    (menu-miee-2 . "Send mails in spool")
    (menu-miee-3 . "Message Offline")
    (menu-miee-4 . "Message Online")
    (menu-1 . "Toggle articles to fetch")
    (menu-2 . "Toggle online/offline send mail")
    (menu-3 . "Set auto PPP")
    (menu-4 . "Expire articles")
    (menu-5 . "Set interval time")
    (menu-6 . "Hang up Line.")
    (menu-7 . "Customize options...")))

(defvar gnus-offline-resource-ja
  '((error-check-1
     . "警告!!: gnus-agent.el または nnagent.el がロードされていません。
.emacs または .gnus.el の gnus-agent の設定を正しくしてください。")
    (error-check-2 ."警告!!: nnspool.el がロードされていません。
.emacs または .gnus.el の nnspool の設定を正しくしてください。")
    (connect-server-1 . "接続しています...")
    (connect-server-2 . "接続しています...完了。")
    (get-new-news-function-1 . "オンライン状態です。")
    (set-auto-ppp-1 . "自動的に PPP 接続・切断します。")
    (set-auto-ppp-2 . "自動的に PPP 接続します。")
    (set-auto-ppp-3 . "手動で PPP 接続・切断します。")
    (hangup-line-1 . "切断しています...")
    (hangup-line-2 . "切断しています...完了。")
    (after-jobs-done-1 . "全てのオンライン処理を完了しました。")
    (toggle-on/off-send-mail-1 . "メールを直接送信します。")
    (toggle-on/off-send-mail-2 . "メールはキューに送られます。")
    (toggle-articles-to-fetch-1 . "受信するメッセージは... ")
    (toggle-articles-to-fetch-2 . "メールのみです。")
    (toggle-articles-to-fetch-3 . "ニュースのみです。")
    (toggle-articles-to-fetch-4 . "メールとニュースの両方です。")
    (empting-spool-1 . "キューのメールを送信中...")
    (empting-spool-2 . "キューのメールを送信中... 完了。")
    (empting-spool-3 . "キューのニュース記事を送信中...")
    (empting-spool-4 . "キューのニュース記事を送信中... 完了。")
    (empting-spool-5 . "キューのメッセージを送信中...")
    (empting-spool-6 . "キューのメッセージを送信中... 完了。")
    (interval-time-1 . "送受信ジョブの間隔 (現在の設定は %d 分です) : ")
    (interval-time-2 . "自動送受信機能を オフ にしました。")
    (interval-time-3 . "自動送受信の間隔を %d 分に設定しました。")))

(defvar gnus-offline-resource-ja_complete
  (append
   gnus-offline-resource-ja
   '((menu-miee-1 . "Spool にある記事の送信")
     (menu-miee-2 . "Spool にある Mail の送信")
     (menu-miee-3 . "Offline 状態へ")
     (menu-miee-4 . "Online 状態へ")
     (menu-1 . "取得記事種類の変更")
     (menu-2 . "Mail 送信方法(On/Off)の切替え")
     (menu-3 . "自動 PPP 制御の設定")
     (menu-4 . "取得済記事を消す")
     (menu-5 . "記事取得間隔時間の設定")
     (menu-6 . "回線の切断")
     (menu-7 . "プロパティ...")
     (set-auto-ppp-menu-1 . "自動的に PPP 接続・切断")
     (set-auto-ppp-menu-2 . "自動的に PPP 接続")
     (set-auto-ppp-menu-3 . "手動で PPP 接続・切断"))))

;;; Functions

(defun gnus-offline-get-message (symbol &optional lang)
  (setq lang (or lang gnus-offline-lang))
  (or
   (cdr (assq symbol (symbol-value
		      (intern (format "gnus-offline-resource-%s" lang)))))
   (cdr (assq symbol gnus-offline-resource-en))))

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
	     (insert (gnus-offline-get-message 'error-check-1))
	     (pop-to-buffer buffer)))

	  ((eq gnus-offline-news-fetch-method 'nnspool)
	   (unless (featurep 'nnspool)
	     (set-buffer (gnus-get-buffer-create buffer))
	     (erase-buffer)
	     (insert (gnus-offline-get-message 'error-check-2))
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
  (if (and (memq 'connect gnus-offline-auto-ppp)
	   (functionp gnus-offline-dialup-function))
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
	(message (gnus-offline-get-message 'connect-server-1))
	(apply 'call-process gnus-offline-dialup-program nil nil nil
	       gnus-offline-dialup-program-arguments)
	(sleep-for 1)
	(message (gnus-offline-get-message 'connect-server-2)))))

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
  (message (gnus-offline-get-message 'get-new-news-function-1))

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
	      (if gnus-offline-connected
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
		   (gnus-offline-set-unplugged-state)

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
  (if (and (memq 'disconnect gnus-offline-auto-ppp)
	   (functionp gnus-offline-hangup-function))
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
  (message (gnus-offline-get-message 'hangup-line-1))
  (if (stringp gnus-offline-hangup-program)
      (apply 'start-process "hup" nil gnus-offline-hangup-program
	     gnus-offline-hangup-program-arguments))
  (message (gnus-offline-get-message 'hangup-line-2)))
;;
;; Hang Up line routine whe using nnspool
;;
(defun gnus-offline-nnspool-hangup-line ()
  (if gnus-offline-connected
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
  (if (and (featurep 'xemacs)
	   (fboundp 'play-sound-file))
      (ding nil 'drum)
    (ding))
  (gnus-group-save-newsrc)
  (message (gnus-offline-get-message 'after-jobs-done-1)))


;;
;; Set auto PPP
;;
(defun gnus-offline-set-auto-ppp ()
  "*Decide whether to connect and/or disconnect automatically."
  (interactive)
  (let ((keys (key-description (this-command-keys)))
	menu title str)
    (cond ((or (string= "misc-user" keys)
	       (string-match "^menu-bar" keys)
	       (string-match "^mouse" keys))
	   (setq title (gnus-offline-get-message 'menu-3))
	   (setq menu
		 (cons
		  title
		  (gnus-offline-get-menu-items
		   '((set-auto-ppp-menu-1
		      (progn
			(setq gnus-offline-auto-ppp '(connect disconnect))
			(message (gnus-offline-get-message 'set-auto-ppp-1)))
		      t)
		     (set-auto-ppp-menu-2
		      (progn
			(setq gnus-offline-auto-ppp '(connect))
			(message (gnus-offline-get-message 'set-auto-ppp-2)))
		      t)
		     (set-auto-ppp-menu-3
		      (progn
			(setq gnus-offline-auto-ppp nil)
			(message (gnus-offline-get-message 'set-auto-ppp-3)))
		      t)))))
	   (gnus-offline-popup menu title))
	  (t
	   (cond ((eq gnus-offline-auto-ppp nil)
		  (setq gnus-offline-auto-ppp '(connect disconnect))
		  (setq str (gnus-offline-get-message 'set-auto-ppp-1)))
		 ((memq 'connect gnus-offline-auto-ppp)
		  (cond ((memq 'disconnect gnus-offline-auto-ppp)
			 (setq gnus-offline-auto-ppp '(connect))
			 (setq str
			       (gnus-offline-get-message 'set-auto-ppp-2)))
			(t
			 (setq gnus-offline-auto-ppp nil)
			 (setq str
			       (gnus-offline-get-message 'set-auto-ppp-3))))))
	   (message str)))))
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
	(message (gnus-offline-get-message 'toggle-on/off-send-mail-1)))
    ;; Sending mail under offline environ.
    (gnus-offline-set-offline-sendmail-function)
    (setq gnus-offline-mail-treat-environ 'offline)
    (message (gnus-offline-get-message 'toggle-on/off-send-mail-2))))
;;
;; Toggle articles to fetch ... both -> mail -> news -> both
;;
(defun gnus-offline-toggle-articles-to-fetch ()
  "*Set articles to fetch... both(Mail/News) -> mail only -> News only -> both"
  (interactive)
  (let ((string (gnus-offline-get-message 'toggle-articles-to-fetch-1))
	str)
    (cond ((eq gnus-offline-articles-to-fetch 'both)
	   (setq gnus-offline-articles-to-fetch 'mail
		 str (gnus-offline-get-message 'toggle-articles-to-fetch-2)))
	  ((eq gnus-offline-articles-to-fetch 'mail)
	   (setq gnus-offline-articles-to-fetch 'news
		 str (gnus-offline-get-message 'toggle-articles-to-fetch-3)))
	  (t
	   (setq gnus-offline-articles-to-fetch 'both
		 str (gnus-offline-get-message 'toggle-articles-to-fetch-4))))
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
	      (message (gnus-offline-get-message 'empting-spool-1))
	      ;; Using miee to send mail.
	      (mail-spool-send)
	      (message (gnus-offline-get-message 'empting-spool-2))))
	(message (gnus-offline-get-message 'empting-spool-3))
	;; Using miee to post news.
	(if (and (not (stringp msspool-news-server))
		 (not msspool-news-service))
	    (progn
	      (setq msspool-news-server (nth 1 gnus-select-method))
	      (setq msspool-news-service 119)))
	(news-spool-post)
	(message (gnus-offline-get-message 'empting-spool-4)))
    ;; Send queued message by gnus-agent
    (message (gnus-offline-get-message 'empting-spool-5))
    (gnus-group-send-drafts)
    (message (gnus-offline-get-message 'empting-spool-6)))
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
			(format (gnus-offline-get-message 'interval-time-1)
				gnus-offline-interval-time)
			nil)))
  (if (< gnus-offline-interval-time 2)
      (progn
	(message (gnus-offline-get-message 'interval-time-2))
	(setq gnus-offline-interval-time 0))
    (message
     (format (gnus-offline-get-message 'interval-time-3)
	     gnus-offline-interval-time)))
  (gnus-offline-processed-by-timer))
;;
;; Expire articles using gnus-agent.
;;
(defun gnus-offline-agent-expire ()
  "*Expire expirable article on News group."
  (interactive)
  (and gnus-offline-agent-automatic-expire
       (if (eq 0 gnus-agent-expire-days)
	   (let (gnus-agent-expire-all)
	     (gnus-agent-expire))
	 (gnus-agent-expire))))
;;
;; Menu.
;;
(defun gnus-offline-define-menu-and-key ()
  "*Set key and menu."
  (if (eq gnus-offline-drafts-queue-type 'miee)
      (static-if (featurep 'xemacs)
	  (add-hook 'gnus-group-mode-hook 'gnus-offline-define-menu-on-miee)
	(gnus-offline-define-menu-on-miee))
    (add-hook 'gnus-group-mode-hook 'gnus-offline-define-menu-on-agent))
  (add-hook 'gnus-group-mode-hook
	    '(lambda ()
	       (local-set-key "\C-coh" 'gnus-offline-set-unplugged-state)
	       (local-set-key "\C-cof" 'gnus-offline-toggle-articles-to-fetch)
	       (local-set-key "\C-coo" 'gnus-offline-toggle-on/off-send-mail)
	       (local-set-key "\C-cox" 'gnus-offline-set-auto-ppp)
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
		    gnus-agent-summary-mode-map))))
  (static-cond
   ((featurep 'xemacs)
    ;; Overwrite the toolbar spec for gnus-group-mode.
    (add-hook 'gnus-startup-hook
	      #'(lambda ()
		  (catch 'tag
		    (mapc (lambda (but)
			    (when (eq 'gnus-group-get-new-news (aref but 1))
			      (aset but 1 'gnus-offline-gnus-get-new-news)
			      (throw 'tag nil)))
			  gnus-group-toolbar)))))
   (t
    (add-hook
     'gnus-group-mode-hook
     `(lambda ()
	(define-key gnus-group-mode-map
	  ,(static-if (eq system-type 'windows-nt) [S-mouse-2] [mouse-3])
	  'gnus-offline-popup-menu))))))
;;
;;
(defun gnus-offline-popup (menu &optional title)
  (static-cond
   ((featurep 'xemacs)
    (popup-menu menu))
   (t
    (let ((menu-func (or (and (fboundp 'easy-menu-create-menu)
			      'easy-menu-create-menu)
			 'easy-menu-create-keymaps))
	  keymap pop func)
      (static-cond ((< emacs-major-version 20)
		    ;; For Emacsen from 19.34 down to 19.28.
		    ;; Seems the first item in MENU will be ignored.
		    (or (keymapp menu)
			(setq menu
			      (append (list ""  ;; This will be ignored.
					    (or title "Popup Menu")
					    "-----"
					    "-----")
				      (cdr menu))))
		    (setq keymap
			  (if (keymapp menu)
			      (append (list 'keymap
					    (if title
						`(nil ,title)
					      '(nil "Popup Menu"))
					    '(nil "")
					    '(nil ""))
				      (cdr menu))
			    (funcall menu-func (car menu) (cdr menu)))))
		   (t
		    (setq keymap
			  (if (keymapp menu)
			      menu
			    (funcall menu-func (car menu) (cdr menu))))))
      ;; Display the popup menu.
      (if (and (setq pop (x-popup-menu t keymap))
	       (setq func (lookup-key keymap
				      (apply 'vector pop))))
	  (funcall func))))))

(defun gnus-offline-get-menu-items (list)
  (mapcar
   #'(lambda (el)
       (if (listp el)
	   (apply 'vector
		  (cons (gnus-offline-get-message (car el)) (cdr el)))
	 el))
   list))

(defvar gnus-offline-menu
  (gnus-offline-get-menu-items
   '((menu-1 gnus-offline-toggle-articles-to-fetch t)
     (menu-2 gnus-offline-toggle-on/off-send-mail t)
     (menu-3 gnus-offline-set-auto-ppp t)
     "----"
     (menu-4 gnus-offline-agent-expire
	     (eq gnus-offline-news-fetch-method 'nnagent))
     (menu-5 gnus-offline-set-interval-time t)
     "----"
     (menu-6 gnus-offline-set-unplugged-state gnus-offline-connected)
     "----"
     (menu-7 gnus-ofsetup-customize t))))

(defun gnus-offline-define-menu-on-miee ()
  "*Set and change menu bar on MIEE menu."
  (let ((miee-menu
	 (gnus-offline-get-menu-items
	  '((menu-miee-1 news-spool-post t)
	    (menu-miee-2 mail-spool-send t)
	    "----"
	    (menu-miee-3 message-offline-state (not message-offline-state))
	    (menu-miee-4 message-online-state message-offline-state)
	    "----")))
	menu)
    (setq menu
	  (easy-menu-change
	   nil "Miee"
	   (append miee-menu
		   (list (cons "Gnus Offline" gnus-offline-menu)))))
    (static-if (featurep 'xemacs)
	(easy-menu-add menu))))
;;
;; define menu without miee.
;;
(defun gnus-offline-define-menu-on-agent ()
  "*Set menu bar on OFFLINE menu."
  (easy-menu-define
   gnus-offline-menu-on-agent gnus-group-mode-map "Gnus offline Menu"
   (cons "Offline" gnus-offline-menu))
  (static-if (featurep 'xemacs)
      (easy-menu-add gnus-offline-menu-on-agent)))
;;
;; Popup menu within the group buffer (under Emacs).
;;
(static-unless (featurep 'xemacs)
  (defun gnus-offline-popup-menu (event)
    "Popup menu for Gnus Offline."
    (interactive "e")
    (apply 'gnus-offline-popup
	   (if (boundp 'miee-popup-menu)
	       (list (or (assq 'keymap
			       (assq 'Miee (assq 'menu-bar global-map)))
			 miee-popup-menu)
		     "Miee")
	     (list (symbol-value 'gnus-offline-menu-on-agent)
		   "Offline")))))

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
;; Code for making Gnus and Gnus Offline cooperate each other.
;;

(defadvice gnus (after gnus-offline-ad activate)
  "Synchronize `gnus-offline-connected' with `gnus-plugged'."
  (and (featurep 'gnus-agent)
       (setq gnus-offline-connected gnus-plugged)))
;;
;;
;;; gnus-offline.el ends here

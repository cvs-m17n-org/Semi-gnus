;;; gnus-ofsetup.el --- Setup advisor for Offline reading for Mail/News.

;; Copyright (C) 1998, 2001 Tatsuya Ichikawa

;; Author: Tatsuya Ichikawa <t-ichi@po.shiojiri.ne.jp>
;;	Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;;
;; This file is part of Semi-gnus.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:

;; How to use.
;;
;;      M-x load[RET]gnus-ofsetup
;;      M-x gnus-setup-for-offline
;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'read-passwd)

(eval-and-compile
  (defvar gnus-offline-lang
    (cond ((and (featurep 'meadow)
		(string= current-language-environment "Japanese"))
	   "ja_complete")
	  ((and (boundp 'current-language-environment)
		(string= current-language-environment "Japanese"))
	   "ja")
	  (t
	   "en"))
    "This variable decides which language will be used for display."))

(eval-when-compile
  (require 'gnus)
  (require 'gnus-offline))

(defvar gnus-offline-setting-file
  (let ((user (user-login-name))
	(real-user (user-real-login-name)))
    (if (string= user real-user)
	"~/.gnus-offline.el"
      ;; Seems it is after "su".
      (let ((file (concat "~" user "/.gnus-offline.el"))
	    (real-file (concat "~" real-user "/.gnus-offline.el")))
	(cond ((file-exists-p real-file)
	       real-file)
	      ((file-exists-p file)
	       file)
	      (t
	       real-file))))))

(eval-when-compile
  (defvar gnus-ofsetup-prepare-for-miee
    '(;; Spool directory setting - MIEE.
      (setq mail-spool (or mail-spool "/usr/spool/mail.out"))
      (setq news-spool (or news-spool "/usr/spool/news.out"))
      (condition-case nil
	  (progn
	    (if (not (file-exists-p mail-spool))
		(make-directory mail-spool t))
	    (if (not (file-exists-p news-spool))
		(make-directory news-spool t)))
	(error
	 (error (gnus-ofsetup-gettext 'prepare-miee-1))))))

  (defvar gnus-ofsetup-update-setting-file
    '((save-excursion
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

	(if (integerp interval)
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

	;; Write setting about hooks.
	(when (eq news-method 'nnspool)
	  (insert (format "%s %s %s\n"
			  "(add-hook"
			  "'after-getting-news-hook"
			  "'gnus-offline-nnspool-hangup-line)"))
	  (insert (format "%s %s %s\n"
			  "(add-hook"
			  "'gnus-before-startup-hook"
			  "(lambda () (setq nnmail-spool-file nil)
	   (setq mail-sources nil)))")))

	;; Write stting about mail-source.el
	(insert "(setq gnus-offline-mail-source '"
		(prin1-to-string mail-source) ")\n")
	(insert "(setq mail-sources gnus-offline-mail-source)\n")
	(if save-passwd
	    (insert "(add-hook 'gnus-setup-news-hook
	  (lambda ()
	    (add-to-list 'gnus-variable-list 'mail-source-password-cache)))\n"))

	;;
	(write-region (point-min) (point-max) gnus-offline-setting-file))
      (kill-buffer "* Setting")))

  (defmacro gnus-ofsetup-prepare (list)
    (let ((forms (symbol-value list)))
      `(progn ,@forms))))

(defvar gnus-ofsetup-resource-en
  '((prepare-miee-1
     . "Making directories failed. Set mail/news spool directories properly.")
    (completing-read-symbol-1 . " (TAB to completion): ")
    (setup-1 . "Method for offline News reading")
    (setup-2 . "Agent directory: ")
    (setup-3 . "Dialup program (give a null string if you do not use): ")
    (setup-4 . "Dialup program options: ")
    (setup-5 . "Hangup program (give a null string if you do not use): ")
    (setup-6 . "Hangup program options: ")
    (setup-7 . "Select MTA type for sending mail")
    (setup-8 . "Use MIEE post/send message ")
    (setup-9 . "News spool directory for sending: ")
    (setup-10 . "Mail spool directory for sending: ")
    (setup-11 . "How many mail sources will get mails from? : ")
    (setup-12 . "What type of the mail source? ")
    (setup-13 . "Mail Account name : ")
    (setup-14 . "Mail server : ")
    (setup-15 . "Authentification Method ")
    (setup-16 . "Do you use pop3.el to fetch mail? ")
    (setup-17 . "movemail program name: ")
    (setup-18 . "movemail options: ")
    (setup-19 . "What network stream? ")
    (setup-20 . "File: ")
    (setup-21 . "Directory: ")
    (setup-22 . "Do you save password information to newsrc file? ")

    (param-news-method-1 . "News Method")
    (param-news-method-2 . "Gnus Agent")
    (param-news-method-3 . "nnspool")
    (param-news-method-4 . "\
Method to fetch news articles.")
    (param-dialup-program-1 . "Dialup Program")
    (param-dialup-program-2 . "Use program..")
    (param-dialup-program-3 . "Don't use a program.")
    (param-dialup-program-4 . "\
Program which does dial.")
    (param-dialup-program-arg-1 . "Dialup Program Arguments")
    (param-dialup-program-arg-2 . "Argument")
    (param-dialup-program-arg-3 . "\
Program arguments of gnus-offline-dialup-program.")
    (param-hangup-program-1 . "Hangup Program")
    (param-hangup-program-2 . "Use program..")
    (param-hangup-program-3 . "Don't use a program.")
    (param-hangup-program-4 . "\
Program which does hangup.")
    (param-hangup-program-arg-1 . "Hangup Program Arguments")
    (param-hangup-program-arg-2 . "Argument")
    (param-hangup-program-arg-3 . "\
Program arguments of gnus-offline-hangup-program.")
    (param-interval-1 . "Interval between Jobs")
    (param-interval-2 . "\
Interval time(minutes) to do online jobs.
If set to 0 , timer call is disabled.")
    (param-drafts-queue-type-1 . "Drafts Queue Type")
    (param-drafts-queue-type-2 . "Gnus Draft for queuing.")
    (param-drafts-queue-type-3 . "I prefer MIEE for queuing.")
    (param-drafts-queue-type-4 . "\
Type of queue used for draft messages.

If the select method for news is nnspool, you must choose MIEE.
MIEE is another library for offline messaging. It isn't part of
Semi-gnus. If you want to know about MIEE, see README-offline.{en,ja}.")
    (param-mail-spool-1 . "Mail Spool Directory for MIEE")
    (param-news-spool-1 . "News Spool Directory for MIEE")
    (param-MTA-type-1 . "MTA Type")
    (param-MTA-type-2 . "Use smtp.el")
    (param-MTA-type-3 . "Use sendmail")
    (param-MTA-type-4 . "\
Type of MTA used for sending mail.")
    (param-save-passwd-1 . "Save Password in Startup File")
    (param-save-passwd-2 . "OK, I'm sure it's safe.")
    (param-save-passwd-3 . "No way, it's too dangerous!")
    (param-save-passwd-4 . "\
Whether you want your POP passwords written in .newsrc.eld or not.")
    (param-mail-source-1 . "Mail Sources")
    (param-mail-source-2 . "\
Information of mail sources. Actually, a list of `Mail Source Specifiers'.

The format of this variable is just the same as `mail-sources' (or
`nnmail-spool-file' which seems obsolete).

`Mail Source Specifiers' can take a lot of keywords. For example,
if you want to use movemail instead of pop3.el which comes with
Gnus, you can set a specifier using the kerword :program as shown
below:

	  (pop :program \"movemail -pf po:%u %t %p\")

If you want to know more about mail source specifiers and keywords,
click the button below.")
    (customize-1 . "Customize the Gnus Offline Parameters, and press ")
    (customize-2 . "done")
    (customize-3 . "Push me when done customizing.")
    (customize-4 . "Parameters")
    (customize-5 . "\
These parameters will be saved in ~/.gnus-offline.el.

Note: Touching these parameters may require Gnus or even Emacs to be
restarted.")
    (customize-6 . "Push me to learn more.")
    (customize-done-1 . "(No changes need to be saved)")
    (customize-done-2
     . "Invalid parameters. Check the news method and drafts queue type.")))

(defvar gnus-ofsetup-resource-ja
  '((prepare-miee-1
     . "ディレクトリが作れません。スプールの設定を正しくしてください。")
    (completing-read-symbol-1 . "(TAB で補完します): ")
    (setup-1 . "オフラインでニュースを読む方法を選んでください ")
    (setup-2 . "Gnus Agent のディレクトリ: ")
    (setup-3
     . "ダイヤル接続プログラム名 (使わない場合は空文字列を返して下さい): ")
    (setup-4 . "ダイヤル接続プログラムの引数: ")
    (setup-5
     . "ダイヤル切断プログラム名 (使わない場合は空文字列を返して下さい): ")
    (setup-6 . "ダイヤル切断プログラムの引数: ")
    (setup-7 . "メール送信に使う MTA を選んで下さい ")
    (setup-8 . "メッセージの送信に MIEE を使いますか? ")
    (setup-9 . "ニュースのスプールディレクトリ: ")
    (setup-10 . "メールのスプールディレクトリ: ")
    (setup-11 . "設定するメール取得先の数は? (後で追加できます): ")
    (setup-12 . "メール取得先のタイプは? ")
    (setup-13 . "メールのアカウント名: ")
    (setup-14 . "そのアカウントのあるメールサーバ名: ")
    (setup-15 . "認証方式は? ")
    (setup-16 . "メールの受信には pop3.el を使いますか? ")
    (setup-17 . "movemail プログラムの名前: ")
    (setup-18 . "movemail プログラムに渡す引数: ")
    (setup-19 . "接続方式は? ")
    (setup-20 . "ファイル: ")
    (setup-21 . "ディレクトリ: ")
    (setup-22 . "newsrc ファイルに POP パスワードを保存しますか? ")

    (param-news-method-4 . "\
ニュース記事を取得する方法です。")
    (param-dialup-program-4 . "\
自動ダイヤル接続に使うプログラム。")
    (param-dialup-program-arg-3 . "\
ダイヤル接続プログラムに渡す引数。")
    (param-hangup-program-4 . "\
自動ダイヤル切断に使うプログラム。")
    (param-hangup-program-arg-3 . "\
ダイヤル切断プログラムに渡す引数。")
    (param-interval-2 . "\
送受信ジョブを行う間隔。分単位で指定します。

これを 0 に設定すると自動送受信機能はオフになります。")
    (param-drafts-queue-type-4 . "\
ドラフト記事を保存するキューの種類を指定します。

ニュースの講読手段が nnspool の場合、MIEE を指定する必要があります。
MIEE はオフラインでのメール/ニュース送信機能を提供するライブラリですが、
Semi-gnus には付属していません。MIEE について知りたい場合は
README-offline.{en,ja} を参照してください。")
    (param-MTA-type-4 . "\
MTA はメールの送信を受け持つプログラムです。その種類を指定します。")
    (param-save-passwd-4 . "\
POP パスワードを .newsrc.eld に保存するか否かを指定します。")
    (param-mail-source-2 . "\
メール取得先や取得方法を指定します。

このオプションの書式は `mail-sources' という変数と同じであり、
`mail source specifier' と呼ばれるもののリストとして記述します。

各々の `mail source specifier' には色々なキーワードを指定することが
できます。例えばメールの取得には pop3.el の代わりに外部プログラム
(movemail など) を使いたい、という場合には、:program をいうキーワー
ドを指定して以下の様に記述します。

	(pop :program \"movemail -pf po:%u %t %p\")

mail source specifier とか上記のようなキーワードについてもっとよく
知りたい場合は、以下のボタンをクリックしてください。(Info の該当箇所
へのリンクになっています。)" )

     (customize-1
      . "Gnus Offline のパラメータ。終わったら右のボタンを押してください。")
     (customize-2 . "done")
     (customize-3 . "設定し終わったらボクを押してね。")
     (customize-4 . "パラメータ")
     (customize-5 . "\
これらのパラメータは ~/.gnus-offline.el に保存されます。

注意: これらのパラメータの変更は場合によっては Gnus (さらに場合に
よっては Emacs) の再起動を必要とすることがあります。")
     (customize-6 . "もっと知りたい場合はボクを押してね。")
     (customize-done-1 . "(保存すべき変更はありません)")
     (customize-done-2
      . "ニュース講読手段とドラフト記事取り扱い手段の設定を見直してください。")
  ))

(defvar gnus-ofsetup-resource-ja_complete
  (append
   gnus-ofsetup-resource-ja
   '((param-news-method-1 . "ニュース講読方法")
     (param-news-method-2 . "Gnus Agent")
     (param-news-method-3 . "nnspool")
     (param-dialup-program-1 . "ダイヤル接続プログラム")
     (param-dialup-program-2 . "プログラムを使う..")
     (param-dialup-program-3 . "プログラムは指定しない")
     (param-dialup-program-arg-1 . "ダイヤル接続プログラムの引数")
     (param-dialup-program-arg-2 . "引数")
     (param-hangup-program-1 . "ダイヤル切断プログラム")
     (param-hangup-program-2 . "プログラムを使う..")
     (param-hangup-program-3 . "プログラムは指定しない")
     (param-hangup-program-arg-1 . "ダイヤル切断プログラムの引数")
     (param-hangup-program-arg-2 . "引数")
     (param-interval-1 . "自動送受信ジョブの間隔")
     (param-drafts-queue-type-1 . "ドラフト記事を保存するキューの種類")
     (param-drafts-queue-type-2 . "Gnus Draft の機能を利用する")
     (param-drafts-queue-type-3 . "MIEE の機能を利用する")
     (param-mail-spool-1 . "MIEE が使うメールのスプールディレクトリ")
     (param-news-spool-1 . "MIEE が使うニュースのスプールディレクトリ")
     (param-MTA-type-1 . "MTA の種類")
     (param-MTA-type-2 . "smtp.el を使う")
     (param-MTA-type-3 . "sendmail を使う")
     (param-save-passwd-1
      . "ファイルに POP パスワードを保存する")
     (param-save-passwd-2 . "安全なので保存する")
     (param-save-passwd-3 . "危険だからやめとく")
     (param-mail-source-1 . "メール取得先の情報"))))

(defsubst gnus-ofsetup-gettext (symbol &optional lang)
  (setq lang (or lang gnus-offline-lang))
  (or
   (cdr (assq symbol (symbol-value
		      (intern (format "gnus-ofsetup-resource-%s" lang)))))
   (cdr (assq symbol gnus-ofsetup-resource-en))))

(defun gnus-ofsetup-completing-read-symbol (msg &rest syms)
  (intern
   (completing-read (concat
		     msg
		     (gnus-ofsetup-gettext 'completing-read-symbol-1))
		    (mapcar
		     #'(lambda (sym)
			 (list (symbol-name sym)))
		     syms)
		    nil t nil)))

(defun gnus-setup-for-offline (&optional force)
  "*Set up Gnus for offline environment."
  (interactive "P")
  (unless (and (file-exists-p gnus-offline-setting-file) (not force))
    (let (news-method
	  mail-method agent-directory drafts-queue-type news-spool mail-spool
	  use-miee MTA-type dialup-program dialup-program-arguments
	  hangup-program hangup-program-arguments interval
	  num-of-address i n mail-source save-passwd)
      (setq news-method
	    (gnus-ofsetup-completing-read-symbol
	     (gnus-ofsetup-gettext 'setup-1)
	     'nnagent 'nnspool))
      ;; Setting for gnus-agent.
      (if (eq news-method 'nnagent)
	  (setq agent-directory
		(read-from-minibuffer
		 (gnus-ofsetup-gettext 'setup-2) "~/News/agent")))
      (setq mail-method 'nnmail)
      (setq dialup-program
	    (read-file-name
	     (gnus-ofsetup-gettext 'setup-3)
	     nil nil t))
      (if (string-match "^[ \t]*$" dialup-program)
	  (setq dialup-program nil)
	(setq dialup-program-arguments
	      (delete "" (split-string
			  (read-from-minibuffer
			   (gnus-ofsetup-gettext 'setup-4))
			  "[\t ]+"))))
      (setq hangup-program
	    (read-file-name
	     (gnus-ofsetup-gettext 'setup-5)
	     nil nil t))
      (if (string-match "^[ \t]*$" hangup-program)
	  (setq hangup-program nil)
	(setq hangup-program-arguments
	      (delete "" (split-string
			  (read-from-minibuffer
			   (gnus-ofsetup-gettext 'setup-6))
			  "[\t ]+"))))
      (setq MTA-type (gnus-ofsetup-completing-read-symbol
		      (gnus-ofsetup-gettext 'setup-7)
		      'smtp 'sendmail))
      (if (eq news-method 'nnspool)
	  (setq use-miee t)
	(setq use-miee (y-or-n-p (gnus-ofsetup-gettext 'setup-8))))
      (if use-miee
	  (progn
	    ;; Setting for MIEE.
	    (setq news-spool
		  (read-from-minibuffer
		   (gnus-ofsetup-gettext 'setup-9)
		   "/usr/spool/news.out"))
	    (setq mail-spool
		  (read-from-minibuffer
		   (gnus-ofsetup-gettext 'setup-10)
		   "/usr/spool/mail.out"))
	    (setq drafts-queue-type 'miee)
	    (gnus-ofsetup-prepare gnus-ofsetup-prepare-for-miee))
	;; Set drafts type gnus-agent.
	(setq drafts-queue-type 'agent))
      ;; Create a list of mail source specifiers.
      (setq num-of-address
	    (read-from-minibuffer (gnus-ofsetup-gettext 'setup-11)))
      (setq i (setq n (string-to-int num-of-address)))
      ;;
      (while (> i 0)
	(let* ((j (- n (1- i)))
	       (type (gnus-ofsetup-completing-read-symbol
		      (format "<%d of %d> %s" j n
			      (gnus-ofsetup-gettext 'setup-12))
		      'pop 'imap 'file 'directory 'maildir))
	       user server authentication stream islisp source
	       prog args program path)
	  ;; Prepare.
	  (when (or (string= type "pop") (string= type "imap"))
	    (setq user (read-from-minibuffer
			(format "<%d of %d> %s" j n
				(gnus-ofsetup-gettext 'setup-13))))
	    (setq server (read-from-minibuffer
			  (format "<%d of %d> %s" j n
				  (gnus-ofsetup-gettext 'setup-14)))))
	  (when (string= type "pop")
	    (setq authentication (gnus-ofsetup-completing-read-symbol
				  (format "<%d of %d> %s" j n
					  (gnus-ofsetup-gettext 'setup-15))
				  'password 'apop))
	    (setq islisp (y-or-n-p
			  (format "<%d of %d> %s" j n
				  (gnus-ofsetup-gettext 'setup-16))))
	    (unless islisp
	      (setq prog (read-file-name
			  (format "<%d of %d> %s" j n
				  (gnus-ofsetup-gettext 'setup-17))
			  exec-directory "movemail"))
	      (setq args (read-from-minibuffer
			  (format "<%d of %d> %s" j n
				  (gnus-ofsetup-gettext 'setup-18) "-pf")))
	      (setq program (format "%s %s %s %s %s"
				    prog args "po:%u" "%t" "%p"))))
	  (when (string= type "imap")
	    (setq stream (gnus-ofsetup-completing-read-symbol
			  (format "<%d of %d> %s" j n
				  (gnus-ofsetup-gettext 'setup-19))
			  'kerberos4 'starttls 'ssl 'network))
	    (setq authentication (gnus-ofsetup-completing-read-symbol
				  (format "<%d of %d> %s" j n
					  (gnus-ofsetup-gettext 'setup-14))
				  'kerberos4 'digest-md5 'cram-md5 'login
				  'anonymous)))
	  (when (string= type "file")
	    (setq path (read-file-name
			(format "<%d of %d> %s" j n
				(gnus-ofsetup-gettext 'setup-20)))))
	  (when (or (string= type "directory") (string= type "maildir"))
	    (setq path (read-file-name
			(format "<%d of %d> %s" j n
				(gnus-ofsetup-gettext 'setup-21)))))
	  ;; Now set a mail source specifier.
	  (setq source (list type))
	  (let (value)
	    (dolist (symbol '(path user server authentication stream program))
	      (when (setq value (symbol-value symbol))
		(setq source (nconc source
				    (list (make-symbol (format ":%s" symbol))
					  value))))))
	  (setq mail-source (nconc mail-source (list source))))
	(setq i (1- i)))
      (setq save-passwd
	    (y-or-n-p (gnus-ofsetup-gettext 'setup-22)))
      ;;
      (gnus-ofsetup-prepare gnus-ofsetup-update-setting-file)))
  (load gnus-offline-setting-file))


;; Suppport for customizing gnus-ofsetup parameters.

(defvar sendmail-to-spool-directory)
(defvar news-spool-request-post-directory)

(defun gnus-ofsetup-find-parameters ()
  "Return the each current value of gnus-offline parameters."
  `((news-method
     (choice :tag ,(gnus-ofsetup-gettext 'param-news-method-1)
	     :value ,gnus-offline-news-fetch-method
	     (const :tag ,(gnus-ofsetup-gettext 'param-news-method-2)
		    nnagent)
	     (const :tag ,(gnus-ofsetup-gettext 'param-news-method-3)
		    nnspool))
     ,(gnus-ofsetup-gettext 'param-news-method-4))

    (dialup-program
     (choice :tag ,(gnus-ofsetup-gettext 'param-dialup-program-1)
	     :value ,gnus-offline-dialup-program
	     (string :tag ,(gnus-ofsetup-gettext 'param-dialup-program-2))
	     (const :tag ,(gnus-ofsetup-gettext
			   'param-dialup-program-3) nil))
     ,(gnus-ofsetup-gettext 'param-dialup-program-4))

    (dialup-program-arguments
     (repeat :tag ,(gnus-ofsetup-gettext 'param-dialup-program-arg-1)
	     :value ,gnus-offline-dialup-program-arguments
	     (string :tag ,(gnus-ofsetup-gettext
			    'param-dialup-program-arg-2)))
     ,(gnus-ofsetup-gettext 'param-dialup-program-arg-3))

    (hangup-program
     (choice :tag ,(gnus-ofsetup-gettext 'param-hangup-program-1)
	     :value ,gnus-offline-hangup-program
	     (string :tag ,(gnus-ofsetup-gettext 'param-hangup-program-2))
	     (const :tag ,(gnus-ofsetup-gettext 'param-hangup-program-3)
		    nil))
     ,(gnus-ofsetup-gettext 'param-hangup-program-4))

    (hangup-program-arguments
     (repeat :tag ,(gnus-ofsetup-gettext 'param-hangup-program-arg-1)
	     :value ,gnus-offline-hangup-program-arguments
	     (string :tag ,(gnus-ofsetup-gettext
			    'param-hangup-program-arg-2)))
     ,(gnus-ofsetup-gettext 'param-hangup-program-arg-3))

    (interval
     (integer :tag ,(gnus-ofsetup-gettext 'param-interval-1)
	      :value ,gnus-offline-interval-time)
     ,(gnus-ofsetup-gettext 'param-interval-2))

    (drafts-queue-type
     (choice :tag ,(gnus-ofsetup-gettext 'param-drafts-queue-type-1)
	     :value ,gnus-offline-drafts-queue-type
	     (const :tag ,(gnus-ofsetup-gettext 'param-drafts-queue-type-2)
		    agent)
	     (const :tag ,(gnus-ofsetup-gettext 'param-drafts-queue-type-3)
		    miee))
     ,(gnus-ofsetup-gettext 'param-drafts-queue-type-4))

    (mail-spool
     (directory :tag ,(gnus-ofsetup-gettext 'param-mail-spool-1)
		:value ,(cond ((and (boundp 'sendmail-to-spool-directory)
				    sendmail-to-spool-directory)
			       sendmail-to-spool-directory)
			      (t
			       "/usr/spool/mail.out"))))

    (news-spool
     (directory :tag ,(gnus-ofsetup-gettext 'param-news-spool-1)
		:value ,(cond ((and (boundp 'news-spool-request-post-directory)
				    news-spool-request-post-directory)
			       news-spool-request-post-directory)
			      (t
			       "/usr/spool/news.out"))))

    (MTA-type
     (choice :tag ,(gnus-ofsetup-gettext 'param-MTA-type-1)
	     :value ,gnus-offline-MTA-type
	     (const :tag ,(gnus-ofsetup-gettext 'param-MTA-type-2) smtp)
	     (const :tag ,(gnus-ofsetup-gettext 'param-MTA-type-3)
		    sendmail))
     ,(gnus-ofsetup-gettext 'param-MTA-type-4))

    (save-passwd
     (choice :tag ,(gnus-ofsetup-gettext 'param-save-passwd-1)
	     :value ,(if (memq 'mail-source-password-cache gnus-variable-list)
			 t
		       nil)
	     (const :tag ,(gnus-ofsetup-gettext 'param-save-passwd-2) t)
	     (const :tag ,(gnus-ofsetup-gettext 'param-save-passwd-3) nil))
     ,(gnus-ofsetup-gettext 'param-save-passwd-4))

    (mail-source
     (sexp :tag ,(gnus-ofsetup-gettext 'param-mail-source-1)
	   :value ,gnus-offline-mail-source)
     ,(gnus-ofsetup-gettext 'param-mail-source-2))))

(defvar gnus-ofsetup-params)

(defun gnus-ofsetup-customize ()
  "Edit the gnus-offline parameters."
  (interactive)
  (let* ((params (gnus-ofsetup-find-parameters))
	 (types (mapcar #'(lambda (entry)
			    `(cons :format "%v%h\n"
				   :doc ,(nth 2 entry)
				   (const :format "" ,(nth 0 entry))
				   ,(nth 1 entry)))
			params)))
    (kill-buffer (gnus-get-buffer-create "*Gnus Offline Customize*"))
    (switch-to-buffer (gnus-get-buffer-create "*Gnus Offline Customize*"))
    (gnus-custom-mode)
    (widget-insert (gnus-ofsetup-gettext 'customize-1))
    (widget-create 'push-button
		   :tag (gnus-ofsetup-gettext 'customize-2)
		   :help-echo (gnus-ofsetup-gettext 'customize-3)
		   :action 'gnus-ofsetup-customize-done)
    (widget-insert "\n\n")
    (make-local-variable 'gnus-ofsetup-params)
    (setq gnus-ofsetup-params
	  (widget-create 'group
			 `(set :inline t
			       :greedy t
			       :tag ,(gnus-ofsetup-gettext 'customize-4)
			       :format "%t:\n%h%v"
			       :doc ,(gnus-ofsetup-gettext 'customize-5)
			       ,@types)))

    (widget-create 'info-link
		   :help-echo (gnus-ofsetup-gettext 'customize-6)
		   :tag "<Info> mail sources"
		   (if (string-match "^ja" gnus-offline-lang)
		       "(gnus-ja)Mail Sources"
		     "(gnus)Mail Sources"))

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
	(gnus-message 4 (gnus-ofsetup-gettext 'customize-done-1))
      (let (symbol value)
	(dolist (elem params)
	  (setq symbol (car elem)
		value (cdr elem))
	  (set symbol value)
	  (cond ((eq symbol 'news-method)
		 (if (eq value 'nnspool)
		     (setq use-miee t)))
		((eq symbol 'drafts-queue-type)
		 (setq use-miee (eq value 'miee)))
		((eq symbol 'save-passwd)
		 (if value
		     (add-to-list 'gnus-variable-list
				  'mail-source-password-cache)
		   (setq gnus-variable-list
			 (delq 'mail-source-password-cache
			       gnus-variable-list)))))))
      (if (and (eq news-method 'nnspool)
	       (not (eq drafts-queue-type 'miee)))
	  (error (gnus-ofsetup-gettext 'customize-done-2)))
      (if use-miee
	  (gnus-ofsetup-prepare gnus-ofsetup-prepare-for-miee))
      (gnus-ofsetup-prepare gnus-ofsetup-update-setting-file)
      (load gnus-offline-setting-file)))
  (bury-buffer)
  (switch-to-buffer gnus-group-buffer))


;;; Code for making Gnus and Gnus Offline cooperate with each other.

;; Advice.
(defadvice gnus (around gnus-ofsetup-advice activate preactivate)
  "Setup offline environment when Gnus is invoked."
  (require 'gnus-offline) ad-do-it (gnus-offline-setup))

;; Miscellaneous settings.

(setq gnus-nntp-service nil)
(setq gnus-nntp-server nil)
(eval-after-load "gnus-start"
  '(add-hook 'gnus-after-getting-new-news-hook 'gnus-offline-after-get-new-news))
(eval-after-load "message"
  '(add-hook 'message-send-hook 'gnus-offline-message-add-header))
(setq mail-source-read-passwd 'read-pw-read-passwd)
(add-hook 'gnus-setup-news-hook 'read-pw-set-mail-source-passwd-cache)

(provide 'gnus-ofsetup)

;;; gnus-ofsetup.el ends here

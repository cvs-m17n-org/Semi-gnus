;;; webmail.el --- interfacing with web mail
;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: hotmail netaddress my-deja netscape

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Note: You need to have `url' and `w3' installed for this backend to
;; work. `w3' must be 4.0pre46+one-line-cookie patch or standalone
;; `url'.

;; Todo: To support more web mail servers.

;; Known bugs: 
;; 1. Net@ddress may corrupt `X-Face'.

;; Warning:
;; Webmail is an experimental function, which means NO WARRANTY.

;;; Code:

(eval-when-compile (require 'cl))

(require 'nnoo)
(require 'message)
(require 'gnus-util)
(require 'gnus)
(require 'nnmail)
(require 'mm-util)
(require 'mml)
(eval-when-compile
  (ignore-errors
    (require 'w3)
    (require 'url)
    (require 'url-cookie)
    (require 'w3-forms)
    (require 'nnweb)))
;; Report failure to find w3 at load time if appropriate.
(eval '(progn
	 (require 'w3)
	 (require 'url)
	 (require 'url-cookie)
	 (require 'w3-forms)
	 (require 'nnweb)))

;;;

(defvar webmail-type-definition
  '((hotmail
     ;; Hotmail hate other HTTP user agents and use one line cookie
     (paranoid agent cookie post)
     (address . "www.hotmail.com")
     (open-url "http://www.hotmail.com/")
     (open-snarf . webmail-hotmail-open)
     ;; W3 hate redirect POST
     (login-url
      "http://%s/cgi-bin/dologin?login=%s&passwd=%s&enter=Sign+in&sec=no&curmbox=ACTIVE&_lang=&js=yes&id=2&tw=-10000&beta="
      webmail-aux user password)
     (list-snarf . webmail-hotmail-list)
     (article-snarf . webmail-hotmail-article)
     (trash-url 
      "%s&login=%s&f=33792&curmbox=ACTIVE&_lang=&foo=inbox&js=&page=&%s=on&_HMaction=MoveTo&tobox=trAsH&nullbox=" 
      webmail-aux user id))
    (yahoo
     (paranoid cookie post)
     (address . "mail.yahoo.com")
     (open-url "http://mail.yahoo.com/")
     (open-snarf . webmail-yahoo-open)
     (login-url;; yahoo will not accept GET
      content 
      ("%s" webmail-aux)
      ".tries=1&.src=ym&.last=&promo=&lg=us&.intl=us&.bypass=&.chkP=Y&.done=http%%253a%%2F%%2Fedit.yahoo.com%%2Fconfig%%2Fmail%%253f.intl%%3D&login=%s&passwd=%s" 
      user password)
     (login-snarf . webmail-yahoo-login)
     (list-url "%s&rb=Inbox&YN=1" webmail-aux)
     (list-snarf . webmail-yahoo-list)
     (article-snarf . webmail-yahoo-article)
     (trash-url 
      "%s/ym/us/ShowFolder?YY=52107&inc=50&order=down&sort=date&pos=0&box=Inbox&DEL=Delete&destBox=&Mid=%s&destBox2="
      webmail-aux id))
    (netaddress
     (paranoid cookie post)
     (address . "www.netaddress.com")
     (open-url "http://www.netaddress.com/")
     (open-snarf . webmail-netaddress-open)
     (login-url
      content 
      ("%s" webmail-aux)
      "LoginState=2&SuccessfulLogin=%%2Ftpl&NewServerName=www.netaddress.com&JavaScript=JavaScript1.2&DomainID=4&NA31site=classic.netaddress.com&NA31port=80&UserID=%s&passwd=%s" 
      user password)
     (login-snarf . webmail-netaddress-login)
     (list-url 
      "http://www.netaddress.com/tpl/Mail/%s/List?FolderID=-4&SortUseCase=True"
      webmail-session)
     (list-snarf . webmail-netaddress-list)
     (article-url "http://www.netaddress.com/")
     (article-snarf . webmail-netaddress-article)
     (trash-url 
      "http://www.netaddress.com/tpl/Message/%s/Move?FolderID=-4&Q=%s&N=&Sort=Date&F=-1"
      webmail-session id))
    (netscape
     (paranoid cookie post agent)
     (address . "webmail.netscape.com")
     (open-url "http://ureg.netscape.com/iiop/UReg2/login/login?U2_LA=en&U2_BACK_FROM_CJ=true&U2_CS=iso-8859-1&U2_ENDURL=http://webmail.netscape.com/tpl/Subscribe/Step1&U2_NEW_ENDURL=http://webmail.netscape.com/tpl/Subscribe/Step1&U2_EXITURL=http://home.netscape.com/&U2_SOURCE=Webmail")
     (open-snarf . webmail-netscape-open)
     (login-url
      content 
      ("http://ureg.netscape.com/iiop/UReg2/login/loginform")
      "%s&U2_USERNAME=%s&U2_PASSWORD=%s"
      webmail-aux user password)
     (login-snarf . webmail-netaddress-login)
     (list-url 
      "http://webmail.netscape.com/tpl/Mail/%s/List?FolderID=-4&SortUseCase=True"
      webmail-session)
     (list-snarf . webmail-netaddress-list)
     (article-url "http://webmail.netscape.com/")
     (article-snarf . webmail-netscape-article)
     (trash-url 
      "http://webmail.netscape.com/tpl/Message/%s/Move?FolderID=-4&Q=%s&N=&Sort=Date&F=-1"
      webmail-session id))
    (my-deja
     (paranoid cookie post)
     (address . "www.my-deja.com")
     (open-url "http://www.my-deja.com/")
     (open-snarf . webmail-my-deja-open)
     (login-url
      content 
      ("%s" webmail-aux)
      "member_name=%s&pw=%s&go=&priv_opt_MyDeja99="
      user password)
     (list-url "http://www.deja.com/rg_gotomail.xp")
     (list-snarf . webmail-my-deja-list)
     (article-snarf . webmail-my-deja-article)
     (trash-url 
      "%s/gmm_multiplex.femail?%%2Fgmm_domovemesg_top.femail=Move+to%%3A&folder_top=%s%%3Azzz%%3A%%7E6trash%%3AF%%3A0&docid=%s"
      webmail-aux user id))))

(defvar webmail-variables
  '(address article-snarf article-url list-snarf list-url 
	    login-url login-snarf open-url open-snarf site articles
	    post-process paranoid trash-url))

(defconst webmail-version "webmail 1.0")

(defvar webmail-newmail-only nil
  "Only fetch new mails.")

(defvar webmail-move-to-trash-can t
  "Move mail to trash can after fetch it.")

;;; Internal variables

(defvar webmail-address nil)
(defvar webmail-paranoid nil)
(defvar webmail-aux nil)
(defvar webmail-session nil)
(defvar webmail-article-snarf nil)
(defvar webmail-article-url nil)
(defvar webmail-list-snarf nil)
(defvar webmail-list-url nil)
(defvar webmail-login-url nil)
(defvar webmail-login-snarf nil)
(defvar webmail-open-snarf nil)
(defvar webmail-open-url nil)
(defvar webmail-trash-url nil)
(defvar webmail-articles nil)
(defvar webmail-post-process nil)

(defvar webmail-buffer nil)
(defvar webmail-buffer-list nil)

(defvar webmail-type nil)

;;; Interface functions

(defun webmail-error (str)
  (message "%s HTML has changed; please get a new version of webmail (%s)"
	   webmail-type str)
  (error "%s HTML has changed; please get a new version of webmail (%s)"
	 webmail-type str))

(defun webmail-setdefault (type)
  (let ((type-def (cdr (assq type webmail-type-definition)))
	(vars webmail-variables)
	pair)
    (setq webmail-type type)
    (dolist (var vars)
      (if (setq pair (assq var type-def))
	  (set (intern (concat "webmail-" (symbol-name var))) (cdr pair))
	(set (intern (concat "webmail-" (symbol-name var))) nil)))))

(defun webmail-encode-www-form-urlencoded (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   pairs "&"))

(defun webmail-fetch-simple (url content)
  (let ((url-request-data content)
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded"))))
    (nnweb-insert url))
  t)

(defun webmail-fetch-form (url pairs)
  (let ((url-request-data (webmail-encode-www-form-urlencoded pairs))
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded"))))
    (nnweb-insert url))
  t)

(defun webmail-eval (expr)
  (cond
   ((consp expr)
    (cons (webmail-eval (car expr)) (webmail-eval (cdr expr))))
   ((symbolp expr)
    (eval expr))
   (t
    expr)))

(defun webmail-url (xurl)
  (mm-with-unibyte-current-buffer
    (cond 
     ((eq (car xurl) 'content)
      (pop xurl)
      (webmail-fetch-simple (if (stringp (car xurl))
				(car xurl)
			      (apply 'format (webmail-eval (car xurl))))
			    (apply 'format (webmail-eval (cdr xurl)))))
     ((eq (car xurl) 'post)
      (pop xurl)
      (webmail-fetch-form (car xurl) (webmail-eval (cdr xurl))))
     (t
      (nnweb-insert (apply 'format (webmail-eval xurl)))))))
  
(defun webmail-init ()
  "Initialize buffers and such."
  (if (gnus-buffer-live-p webmail-buffer)
      (set-buffer webmail-buffer)
    (setq webmail-buffer
	  (mm-with-unibyte
	    (nnheader-set-temp-buffer " *webmail*")))))

(defvar url-package-name)
(defvar url-package-version)
(defvar url-cookie-multiple-line)
(defvar url-confirmation-func)

;; Hack W3 POST redirect.  See `url-parse-mime-headers'.
;;
;; Netscape uses "GET" as redirect method when orignal method is POST
;; and status is 302, .i.e no security risks by default without
;; confirmation.
;;
;; Some web servers (at least Apache used by yahoo) return status 302
;; instead of 303, though they mean 303.

(defun webmail-url-confirmation-func (prompt)
  (cond 
   ((equal prompt (concat "Honor redirection with non-GET method "
			  "(possible security risks)? "))
    nil)
   ((equal prompt "Continue (with method of GET)? ")
    t)
   (t (error prompt))))

(defun webmail-refresh-redirect ()
  "Redirect refresh url in META."
  (goto-char (point-min))
  (while (re-search-forward 
	  "HTTP-EQUIV=\"Refresh\"[^>]*URL=\\([^\"]+\\)\""
	  nil t)
    (let ((url (match-string 1)))
      (erase-buffer)
      (mm-with-unibyte-current-buffer
	(nnweb-insert url)))
    (goto-char (point-min))))

(defun webmail-fetch (file subtype user password)
  (save-excursion
    (webmail-setdefault subtype)
    (let ((url-package-name (if (memq 'agent webmail-paranoid)
				"Mozilla"
			      url-package-name))
	  (url-package-version (if (memq 'agent webmail-paranoid)
				   "4.0"
				 url-package-version))
	  (url-cookie-multiple-line (if (memq 'cookie webmail-paranoid)
					nil
				      url-cookie-multiple-line))
	  (url-confirmation-func (if (memq 'post webmail-paranoid)
				     'webmail-url-confirmation-func
				   url-confirmation-func))
	  url-cookie-storage url-cookie-secure-storage
	  url-cookie-confirmation
	  item id (n 0))
      (webmail-init)
      (setq webmail-articles nil)
      (when webmail-open-url 
	(erase-buffer)
	(webmail-url webmail-open-url))
      (if webmail-open-snarf (funcall webmail-open-snarf))
      (when webmail-login-url 
	(erase-buffer)
	(webmail-url webmail-login-url))
      (if webmail-login-snarf 
	  (funcall webmail-login-snarf))
      (when webmail-list-url 
	(erase-buffer)
	(webmail-url webmail-list-url))
      (if webmail-list-snarf 
	  (funcall webmail-list-snarf))
      (while (setq item (pop webmail-articles))
	(message "Fetching mail #%d..." (setq n (1+ n)))
	(erase-buffer)
	(mm-with-unibyte-current-buffer
	  (nnweb-insert (cdr item)))
	(setq id (car item))
	(if webmail-article-snarf 
	    (funcall webmail-article-snarf file id))
	(when (and webmail-trash-url webmail-move-to-trash-can)
	  (message "Move mail #%d to trash can..." n)
	  (condition-case err
	      (progn
		(webmail-url webmail-trash-url)
		(let (buf)
		  (while (setq buf (pop webmail-buffer-list))
		    (kill-buffer buf))))
	    (error 
	     (let (buf)
	       (while (setq buf (pop webmail-buffer-list))
		 (kill-buffer buf)))
	     (error err))))))
    (if webmail-post-process
	(funcall webmail-post-process))))

(defun webmail-encode-8bit ()
  (goto-char (point-min))
  (skip-chars-forward "^\200-\377")
  (while (not (eobp))
    (insert (format "&%d;" (mm-char-int (char-after))))
    (delete-char 1)
    (skip-chars-forward "^\200-\377")))

;;; hotmail

(defun webmail-hotmail-open ()
  (goto-char (point-min))
  (if (re-search-forward 
       "action=\"https?://\\([^/]+\\)/cgi-bin/dologin" nil t)
      (setq webmail-aux (match-string 1))
    (webmail-error "open@1")))

(defun webmail-hotmail-list ()
  (let (site url newp)
    (goto-char (point-min))
    (if (re-search-forward "[0-9]+ new" nil t) 
	(message "Found %s" (match-string 0))
      (webmail-error "maybe your w3 version is too old"))
    (goto-char (point-min))
    (if (re-search-forward 
	 "action=\"https?://\\([^/]+\\)/cgi-bin/HoTMaiL" nil t)
	(setq site (match-string 1))
      (webmail-error "list@1"))
    (goto-char (point-min))
    (if (re-search-forward "disk=\\([^&]+\\)&" nil t)
	(setq webmail-aux 
	      (concat "http://" site "/cgi-bin/HoTMaiL?disk=" 
		      (match-string 1)))
      (webmail-error "list@2"))
    (goto-char (point-max))
    (while (re-search-backward 
	    "newmail\\.gif\\|href=\"\\(/cgi-bin/getmsg\\?[^\"]+\\)\"" 
	    nil t)
      (if (setq url (match-string 1))
	  (progn
	    (if (or newp (not webmail-newmail-only))
		(let (id)
		  (if (string-match "msg=\\([^&]+\\)" url)
		      (setq id (match-string 1 url)))
		  (push (cons id (concat "http://" site url "&raw=0")) 
			webmail-articles)))
	    (setq newp nil))
	(setq newp t)))))

;; Thank victor@idaccr.org (Victor S. Miller) for raw=0

(defun webmail-hotmail-article (file id)
  (goto-char (point-min))
  (if (not (search-forward "<pre>" nil t))
      (webmail-error "article@3"))
  (skip-chars-forward "\n\r\t ")
  (delete-region (point-min) (point))
  (if (not (search-forward "</pre>" nil t))
      (webmail-error "article@3.1"))
  (delete-region (match-beginning 0) (point-max))
  (nnweb-remove-markup)
  (nnweb-decode-entities)
  (goto-char (point-min))
  (while (re-search-forward "\r\n?" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (insert "\n\n")
  (if (not (looking-at "\n*From "))
      (insert "From nobody " (current-time-string) "\n"))
  (mm-append-to-file (point-min) (point-max) file))

(defun webmail-hotmail-article-old (file id)
  (let (p attachment count mime hotmail-direct)
    (save-restriction
      (webmail-encode-8bit)
      (goto-char (point-min))
      (if (not (search-forward "<DIV>" nil t))
	  (if (not (search-forward "Reply&nbsp;All" nil t))
	      (webmail-error "article@1")
	    (setq hotmail-direct t))
	(goto-char (match-beginning 0)))
      (narrow-to-region (point-min) (point))
      (if (not (search-backward "<table" nil t 2))
	  (webmail-error "article@1.1"))
      (delete-region (point-min) (match-beginning 0)) 
      (while (search-forward "<a href=" nil t)
	(setq p (match-beginning 0))
	(search-forward "</a>" nil t)
	(delete-region p (match-end 0)))
      (nnweb-remove-markup)
      (nnweb-decode-entities)
      (goto-char (point-min))
      (delete-blank-lines)
      (goto-char (point-min))
      (when (search-forward "\n\n" nil t)
	(backward-char)
	(delete-region (point) (point-max)))
      (goto-char (point-max))
      (widen)
      (insert "\n")
      (setq p (point))
      (while (re-search-forward 
	      "<tt>\\|<div>\\|\\(http://[^/]+/cgi-bin/getmsg/\\([^\?]+\\)\?[^\"]*\\)\"" 
	      nil t)
	(if (setq attachment (match-string 1))
	    (let ((filename (match-string 2))
		  bufname);; Attachment
	      (delete-region p (match-end 0))
	      (save-excursion
		(set-buffer (generate-new-buffer " *webmail-att*"))
		(nnweb-insert attachment)
		(push (current-buffer) webmail-buffer-list)
		(setq bufname (buffer-name)))
	      (setq mime t)
	      (insert "<#part type=" 
		      (or (and filename
			       (string-match "\\.[^\\.]+$" filename)
			       (mailcap-extension-to-mime
				(match-string 0 filename)))
			  "application/octet-stream"))
	      (insert " buffer=\"" bufname "\"")
	      (insert " filename=\"" filename "\"")
	      (insert " disposition=\"inline\"")
	      (insert "><#/part>\n")
	      (setq p (point)))
	  (delete-region p (match-end 0))
	  (if hotmail-direct
	      (if (not (search-forward "</tt>" nil t))
		  (webmail-error "article@1.2")
		(delete-region (match-beginning 0) (match-end 0)))
	    (setq count 1)
	    (while (and (> count 0) 
			(re-search-forward "</div>\\|\\(<div>\\)" nil t))
	      (if (match-string 1)
		  (setq count (1+ count))
		(if (= (setq count (1- count)) 0)
		    (delete-region (match-beginning 0)
				   (match-end 0))))))
	  (narrow-to-region p (point))
	  (goto-char (point-min))
	  (cond 
	   ((looking-at "<pre>")
	    (goto-char (match-end 0))
	    (if (looking-at "$") (forward-char))
	    (delete-region (point-min) (point))
	    (nnweb-remove-markup)
	    (nnweb-decode-entities)
	    nil)
	   (t
	    (setq mime t)
	    (insert "<#part type=\"text/html\" disposition=inline>")
	    (goto-char (point-max))
	    (insert "<#/part>")))
	  (goto-char (point-max))
	  (setq p (point))
	  (widen)))
      (delete-region p (point-max))
      (goto-char (point-min))
      ;; Some blank line to seperate mails.
      (insert "\n\nFrom nobody " (current-time-string) "\n")
      (if id
	  (insert (format "Message-ID: <%s@hotmail.com>\n" id)))
      (unless (looking-at "$") 
	(if (search-forward "\n\n" nil t)
	    (forward-line -1)
	  (webmail-error "article@2")))
      (narrow-to-region (point) (point-max))
      (if mime
	  (insert "MIME-Version: 1.0\n"
		  (prog1
		      (mml-generate-mime)
		    (delete-region (point-min) (point-max)))))
      (goto-char (point-min))
      (widen)
      (let (case-fold-search)
	(while (re-search-forward "^From " nil t)
	  (beginning-of-line)
	  (insert ">"))))
    (mm-append-to-file (point-min) (point-max) file)))

;;; yahoo

(defun webmail-yahoo-open ()
  (goto-char (point-min))
  (if (re-search-forward "action=\"\\([^\"]+\\)\"" nil t)
      (setq webmail-aux (match-string 1))
    (webmail-error "open@1")))

(defun webmail-yahoo-login ()
  (goto-char (point-min))
  (if (re-search-forward "http://[a-zA-Z][0-9]\\.mail\\.yahoo\\.com/" nil t)
      (setq webmail-aux (match-string 0))
    (webmail-error "login@1"))
  (if (re-search-forward "YY=[0-9]+" nil t)
      (setq webmail-aux (concat webmail-aux "ym/us/ShowFolder?"
				(match-string 0)))
    (webmail-error "login@2")))

(defun webmail-yahoo-list ()
  (let (url (newp t) (tofetch 0))
    (goto-char (point-min))
    (when (re-search-forward 
	   "showing [0-9]+-\\([0-9]+\\) of \\([0-9]+\\)" nil t) 
      ;;(setq listed (match-string 1))
      (message "Found %s mail(s)" (match-string 2)))
    (if (string-match "http://[^/]+" webmail-aux)
	(setq webmail-aux (match-string 0 webmail-aux))
      (webmail-error "list@1"))
    (goto-char (point-min))
    (while (re-search-forward 
	    "bgcolor=\"#eeeeee\"\\|href=\"\\(/ym/us/ShowLetter\\?MsgId=\\([^&]+\\)&[^\"]*\\)\""
	    nil t)
      (if (setq url (match-string 1))
	  (progn
	    (when (or newp (not webmail-newmail-only))
	      (push (cons (match-string 2) (concat webmail-aux url "&toc=1")) 
		    webmail-articles)
	      (setq tofetch (1+ tofetch)))
	    (setq newp t))
	(setq newp nil)))
    (setq webmail-articles (nreverse webmail-articles))
    (message "Fetching %d mail(s)" tofetch)))

(defun webmail-yahoo-article (file id)
  (let (p attachment)
    (save-restriction
      (goto-char (point-min))
      (if (not (search-forward "value=\"Done\"" nil t))
	  (webmail-error "article@1"))
      (if (not (search-forward "<table" nil t))
	  (webmail-error "article@2"))
      (delete-region (point-min) (match-beginning 0)) 
      (if (not (search-forward "</table>" nil t))
	  (webmail-error "article@3"))
      (narrow-to-region (point-min) (match-end 0))
      (while (search-forward "<a href=" nil t)
	(setq p (match-beginning 0))
	(search-forward "</a>" nil t)
	(delete-region p (match-end 0)))
      (nnweb-remove-markup)
      (nnweb-decode-entities)
      (goto-char (point-min))
      (delete-blank-lines)
      (goto-char (point-max))
      (widen)
      (insert "\n")
      (setq p (point))
      (while (re-search-forward "[^\"]*/ShowLetter/[^\?]+\?[^\"]*" nil t)
	(setq attachment (match-string 0))
	(let (bufname ct ctl cd description)
	  (if (not (search-forward "<table" nil t))
	      (webmail-error "article@4"))
	  (delete-region p (match-beginning 0))
	  (if (not (search-forward "</table>" nil t))
	      (webmail-error "article@5"))
	  (narrow-to-region p (match-end 0))
	  (nnweb-remove-markup)
	  (nnweb-decode-entities)
	  (goto-char (point-min))
	  (delete-blank-lines)
	  (setq ct (mail-fetch-field "content-type")
		ctl (ignore-errors (mail-header-parse-content-type ct))
		;;cte (mail-fetch-field "content-transfer-encoding")
		cd (mail-fetch-field "content-disposition")
		description (mail-fetch-field "content-description")
		id (mail-fetch-field "content-id"))
	  (delete-region (point-min) (point-max))
	  (widen)
	  (save-excursion
	    (set-buffer (generate-new-buffer " *webmail-att*"))
	    (nnweb-insert (concat webmail-aux attachment))
	    (push (current-buffer) webmail-buffer-list)
	    (setq bufname (buffer-name)))
	  (insert "<#part")
	  (if (and ctl (not (equal (car ctl) "text/")))
	      (insert " type=\"" (car ctl) "\""))
	  (insert " buffer=\"" bufname "\"")
	  (if cd
	      (insert " disposition=\"" cd "\""))
	  (if description
	      (insert " description=\"" description "\""))
	  (insert "><#/part>\n")
	  (setq p (point))))
      (delete-region p (point-max))
      (goto-char (point-min))
      ;; Some blank line to seperate mails.
      (insert "\n\nFrom nobody " (current-time-string) "\n")
      (if id
	  (insert (format "Message-ID: <%s@yahoo.com>\n" id)))
      (unless (looking-at "$") 
	(if (search-forward "\n\n" nil t)
	    (forward-line -1)
	  (webmail-error "article@2")))
      (narrow-to-region (point) (point-max))
      (insert "MIME-Version: 1.0\n"
	      (prog1
		  (mml-generate-mime)
		(delete-region (point-min) (point-max))))
      (goto-char (point-min))
      (widen)
      (let (case-fold-search)
	(while (re-search-forward "^From " nil t)
	  (beginning-of-line)
	  (insert ">"))))
    (mm-append-to-file (point-min) (point-max) file)))

;;; netaddress

(defun webmail-netscape-open ()
  (goto-char (point-min))
  (if (re-search-forward "login/hint\\?\\([^\"]+\\)\"" nil t)
      (setq webmail-aux (match-string 1))
    (webmail-error "open@1")))

(defun webmail-netaddress-open ()
  (goto-char (point-min))
  (if (re-search-forward "action=\"\\([^\"]+\\)\"" nil t)
      (setq webmail-aux (concat (car webmail-open-url) (match-string 1)))
    (webmail-error "open@1")))

(defun webmail-netaddress-login ()
  (webmail-refresh-redirect)
  (goto-char (point-min))
  (if (re-search-forward  "tpl/[^/]+/\\([^/]+\\)" nil t)
      (setq webmail-session (match-string 1))
    (webmail-error "login@1")))

(defun webmail-netaddress-list ()
  (let (item id)
    (goto-char (point-min))
    (when (re-search-forward 
	   "(\\([0-9]+\\) unread, \\([0-9]+\\) total)" nil t) 
      (message "Found %s mail(s), %s unread" 
	       (match-string 2) (match-string 1)))
    (goto-char (point-min))
    (while (re-search-forward 
	    "MR\\[i\\]\\.R='\\([^']*\\)'\\|MR\\[i\\]\\.Q='\\([^']+\\)'" nil t)
      (if (setq id (match-string 2))
	  (setq item 
		(cons id 
		      (format "%s/tpl/Message/%s/Read?Q=%s&FolderID=-4&SortUseCase=True&Sort=Date&Headers=True"
			      (car webmail-article-url)
			      webmail-session id)))
	(if (or (not webmail-newmail-only)
		(equal (match-string 1) "True"))
	    (push item webmail-articles))))
    (setq webmail-articles (nreverse webmail-articles))))

(defun webmail-netaddress-single-part ()
  (goto-char (point-min))
  (cond 
   ((looking-at "[\t\040\r\n]*<font face=[^>]+>[\t\040\r\n]*")
    ;; text/plain
    (replace-match "")
    (while (re-search-forward "[\t\040\r\n]+" nil t)
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward "<br>" nil t)
      (replace-match "\n"))
    (nnweb-remove-markup)
    (nnweb-decode-entities) 
    nil)
   (t
    (insert "<#part type=\"text/html\" disposition=inline>")
    (goto-char (point-max))
    (insert "<#/part>")
    t)))

(defun webmail-netaddress-article (file id)
  (let (p p1 attachment count mime type)
    (save-restriction
      (webmail-encode-8bit)
      (goto-char (point-min))
      (if (not (search-forward "Trash" nil t))
	  (webmail-error "article@1"))
      (if (not (search-forward "<form>" nil t))
	  (webmail-error "article@2"))
      (delete-region (point-min) (match-beginning 0)) 
      (if (not (search-forward "</form>" nil t))
	  (webmail-error "article@3"))
      (narrow-to-region (point-min) (match-end 0))
      (goto-char (point-min))
      (while (re-search-forward "[\040\t\r\n]+" nil t)
	(replace-match " "))
      (goto-char (point-min))
      (while (search-forward "<b>" nil t)
	(replace-match "\n"))
      (nnweb-remove-markup)
      (nnweb-decode-entities)
      (goto-char (point-min))
      (delete-blank-lines)
      (goto-char (point-min))
      (while (re-search-forward "^\040+\\|\040+$" nil t)
	(replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\040+" nil t)
	(replace-match " "))
      (goto-char (point-max))
      (widen)
      (insert "\n\n")
      (setq p (point))
      (unless (search-forward "<!-- Data -->" nil t)
	(webmail-error "article@4"))
      (forward-line 14)
      (delete-region p (point))
      (goto-char (point-max))
      (unless (re-search-backward 
	       "[\040\t]*<br>[\040\t\r\n]*<br>[\040\t\r\n]*<form" p t)
	(webmail-error "article@5"))
      (delete-region (point) (point-max))
      (goto-char p)
      (while (search-forward
	      "<TABLE border=\"0\" WIDTH=\"98%\" cellpadding=0 cellspacing=0>"
	      nil t 2)
	(setq mime t)
	(unless (search-forward "</TABLE>" nil t)
	  (webmail-error "article@6"))
	(setq p1 (point))
	(if (search-backward "<IMG " p t)
	    (progn
	      (unless (re-search-forward "HREF=\"\\(/tpl/Attachment/[^/]+/\\([^/]+/[^\?]+\\)[^\"]+\\)\"" p1 t)
		(webmail-error "article@7"))
	      (setq attachment (match-string 1))
	      (setq type (match-string 2))
	      (unless (search-forward "</TABLE>" nil t)
		(webmail-error "article@8"))
	      (delete-region p (point))
	      (let (bufname);; Attachment
		(save-excursion
		  (set-buffer (generate-new-buffer " *webmail-att*"))
		  (nnweb-insert (concat (car webmail-open-url) attachment))
		  (push (current-buffer) webmail-buffer-list)
		  (setq bufname (buffer-name)))
		(insert "<#part type=" type)
		(insert " buffer=\"" bufname "\"")
		(insert " disposition=\"inline\"")
		(insert "><#/part>\n")
		(setq p (point))))
	  (delete-region p p1)
	  (narrow-to-region 
	   p
	   (if (search-forward 
		"<TABLE border=\"0\" WIDTH=\"98%\" cellpadding=0 cellspacing=0>"
		nil t)
	       (match-beginning 0)
	     (point-max)))
	  (webmail-netaddress-single-part)
	  (goto-char (point-max))
	  (setq p (point))
	  (widen)))
      (unless mime
	(narrow-to-region p (point-max))
	(setq mime (webmail-netaddress-single-part))
	(widen))
      (goto-char (point-min))
      ;; Some blank line to seperate mails.
      (insert "\n\nFrom nobody " (current-time-string) "\n")
      (if id
	  (insert (format "Message-ID: <%s@%s>\n" id webmail-address)))
      (unless (looking-at "$") 
	(if (search-forward "\n\n" nil t)
	    (forward-line -1)
	  (webmail-error "article@2")))
      (when mime
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (not (eobp))
	  (if (looking-at "MIME-Version\\|Content-Type")
	      (delete-region (point) 
			     (progn
			       (forward-line 1)
			       (if (re-search-forward "^[^ \t]" nil t)
				   (goto-char (match-beginning 0))
				 (point-max))))
	    (forward-line 1)))
	(goto-char (point-max))
	(widen)
	(narrow-to-region (point) (point-max))
	(insert "MIME-Version: 1.0\n"
		(prog1
		    (mml-generate-mime)
		  (delete-region (point-min) (point-max))))
	(goto-char (point-min))
	(widen))
      (let (case-fold-search)
	(while (re-search-forward "^From " nil t)
	  (beginning-of-line)
	  (insert ">"))))
    (mm-append-to-file (point-min) (point-max) file)))

(defun webmail-netscape-article (file id)
  (let (p p1 attachment count mime type)
    (save-restriction
      (webmail-encode-8bit)
      (goto-char (point-min))
      (if (not (search-forward "Trash" nil t))
	  (webmail-error "article@1"))
      (if (not (search-forward "<form>" nil t))
	  (webmail-error "article@2"))
      (delete-region (point-min) (match-beginning 0)) 
      (if (not (search-forward "</form>" nil t))
	  (webmail-error "article@3"))
      (narrow-to-region (point-min) (match-end 0))
      (goto-char (point-min))
      (while (re-search-forward "[\040\t\r\n]+" nil t)
	(replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "<a href=[^>]*>[^<]*</a>" nil t)
	(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "<b>" nil t)
	(replace-match "\n"))
      (nnweb-remove-markup)
      (nnweb-decode-entities)
      (goto-char (point-min))
      (delete-blank-lines)
      (goto-char (point-min))
      (while (re-search-forward "^\040+\\|\040+$" nil t)
	(replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\040+" nil t)
	(replace-match " "))
      (goto-char (point-max))
      (widen)
      (insert "\n\n")
      (setq p (point))
      (unless (search-forward "<!-- Data -->" nil t)
	(webmail-error "article@4"))
      (forward-line 14)
      (delete-region p (point))
      (goto-char (point-max))
      (unless (re-search-backward 
	       "<form name=\"Transfer2\"" p t)
	(webmail-error "article@5"))
      (delete-region (point) (point-max))
      (goto-char p)
      (while (search-forward
	      "<TABLE border=\"0\" WIDTH=\"98%\" cellpadding=0 cellspacing=0>"
	      nil t 2)
	(setq mime t)
	(unless (search-forward "</TABLE>" nil t)
	  (webmail-error "article@6"))
	(setq p1 (point))
	(if (search-backward "<IMG " p t)
	    (progn
	      (unless (re-search-forward "HREF=\"\\(/tpl/Attachment/[^/]+/\\([^/]+/[^\?]+\\)[^\"]+\\)\"" p1 t)
		(webmail-error "article@7"))
	      (setq attachment (match-string 1))
	      (setq type (match-string 2))
	      (unless (search-forward "</TABLE>" nil t)
		(webmail-error "article@8"))
	      (delete-region p (point))
	      (let (bufname);; Attachment
		(save-excursion
		  (set-buffer (generate-new-buffer " *webmail-att*"))
		  (nnweb-insert (concat (car webmail-open-url) attachment))
		  (push (current-buffer) webmail-buffer-list)
		  (setq bufname (buffer-name)))
		(insert "<#part type=" type)
		(insert " buffer=\"" bufname "\"")
		(insert " disposition=\"inline\"")
		(insert "><#/part>\n")
		(setq p (point))))
	  (delete-region p p1)
	  (narrow-to-region 
	   p
	   (if (search-forward 
		"<TABLE border=\"0\" WIDTH=\"98%\" cellpadding=0 cellspacing=0>"
		nil t)
	       (match-beginning 0)
	     (point-max)))
	  (webmail-netaddress-single-part)
	  (goto-char (point-max))
	  (setq p (point))
	  (widen)))
      (unless mime
	(narrow-to-region p (point-max))
	(setq mime (webmail-netaddress-single-part))
	(widen))
      (goto-char (point-min))
      ;; Some blank line to seperate mails.
      (insert "\n\nFrom nobody " (current-time-string) "\n")
      (if id
	  (insert (format "Message-ID: <%s@%s>\n" id webmail-address)))
      (unless (looking-at "$") 
	(if (search-forward "\n\n" nil t)
	    (forward-line -1)
	  (webmail-error "article@2")))
      (when mime
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (not (eobp))
	  (if (looking-at "MIME-Version\\|Content-Type")
	      (delete-region (point) 
			     (progn
			       (forward-line 1)
			       (if (re-search-forward "^[^ \t]" nil t)
				   (goto-char (match-beginning 0))
				 (point-max))))
	    (forward-line 1)))
	(goto-char (point-max))
	(widen)
	(narrow-to-region (point) (point-max))
	(insert "MIME-Version: 1.0\n"
		(prog1
		    (mml-generate-mime)
		  (delete-region (point-min) (point-max))))
	(goto-char (point-min))
	(widen))
      (let (case-fold-search)
	(while (re-search-forward "^From " nil t)
	  (beginning-of-line)
	  (insert ">"))))
    (mm-append-to-file (point-min) (point-max) file)))

;;; my-deja

(defun webmail-my-deja-open ()
  (webmail-refresh-redirect)
  (goto-char (point-min))
  (if (re-search-forward "action=\"\\([^\"]+login_confirm\\.xp[^\"]*\\)\"" 
			 nil t)
      (setq webmail-aux (match-string 1))
    (webmail-error "open@1")))

(defun webmail-my-deja-list ()
  (let (item id newp)
    (goto-char (point-min))
    (when (re-search-forward 
	   "(\\([0-9]+\\) message(s), \\([0-9]+\\) new, \\([0-9]+\\)&nbsp;k )"
	   nil t) 
      (message "Found %s mail(s), %s unread, total size %s K" 
	       (match-string 1) (match-string 2) (match-string 3)))
    (goto-char (point-min))
    (while (re-search-forward 
	    "&#149; &nbsp;&nbsp;\\|\\(http:[^\"]+\\)/display_seemesg\\.femail\\?docid=\\([^&\"]+\\)"
	    nil t)
      (if (setq id (match-string 2))
	  (when (or newp (not webmail-newmail-only))
	    (push
	     (cons id (format "%s/gmm_multiplex.femail?docid=%s&femail_page_name=display_page&bool_next_on_disp_pg=true&bool_prev_on_disp_pg=false&display_all_headers=false&%%2Fgmm_save.femail=Download&femail_page_name=display_page&bool_next_on_disp_pg=true&bool_prev_on_disp_pg=false&display_all_headers=false"
			      (match-string 1) id))
	     webmail-articles)
	    (setq webmail-aux (match-string 1))
	    (setq newp nil))
	(setq newp t)))
    (setq webmail-articles (nreverse webmail-articles))))

(defun webmail-my-deja-article (file id)
  (let (url)
    (goto-char (point-min))
    (unless (re-search-forward "\\(http:[^\"]+/attachment/entire_message.txt[^\"]+\\)" nil t)
      (webmail-error "article@1"))
    (setq url (match-string 1))
    (erase-buffer)
    (mm-with-unibyte-current-buffer
      (nnweb-insert url))
    (goto-char (point-min))
    (while (search-forward "\r\n" nil t)
      (replace-match "\n"))
    (goto-char (point-min))
    (insert "\n\nFrom nobody " (current-time-string) "\n")
    (mm-append-to-file (point-min) (point-max) file)))

(provide 'webmail)

;;; webmail.el ends here

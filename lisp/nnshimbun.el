;;; -*- mode: Emacs-Lisp; coding: euc-japan; -*-
;;; $Id: nnshimbun.el,v 1.1.2.1 2000-05-21 04:46:52 tsuchiya Exp $

;;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;; Keywords: news

;;; Copyright:

;; This file is a part of Semi-Gnus.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Gnus backend to read newspapers on WEB.


;;; Defintinos:

(gnus-declare-backend "nnshimbun" 'address)

(eval-when-compile (require 'cl))

(require 'timezone)
(require 'nnheader)
(require 'nnmail)
(require 'nnoo)
(require 'gnus-bcklg)
(eval-when-compile
  (ignore-errors
    (require 'nnweb)))
;; Report failure to find w3 at load time if appropriate.
(eval '(require 'nnweb))


(nnoo-declare nnshimbun)

(defvar nnshimbun-default-type 'asahi)

(defvar nnshimbun-type-definition
  `((asahi
     (url . "http://spin.asahi.com/")
     (groups "national" "business" "politics" "international" "sports" "personal" "feneral")
     (coding-system . ,(if (boundp 'MULE) '*sjis* 'shift_jis))
     (generate-nov  . nnshimbun-asahi-generate-nov-database)
     (make-contents . nnshimbun-asahi-make-contents))))

(defvoo nnshimbun-directory (nnheader-concat gnus-directory "shimbun/")
  "Where nnshimbun will save its files.")

(defvoo nnshimbun-nov-is-evil nil
  "*Non-nil means that nnshimbun will never retrieve NOV headers.")

(defvoo nnshimbun-nov-file-name ".overview")

;; set by nnshimbun-possibly-change-server
(defvoo nnshimbun-current-directory nil)
(defvoo nnshimbun-current-group nil)

;; set by nnoo-change-server
(defvoo nnshimbun-address nil)
(defvoo nnshimbun-type nil)

;; set by nnshimbun-possibly-change-server
(defvoo nnshimbun-server-directory nil)
(defvoo nnshimbun-buffer nil)

;; set by nnshimbun-open-server
(defvoo nnshimbun-url nil)
(defvoo nnshimbun-coding-system nil)
(defvoo nnshimbun-groups nil)
(defvoo nnshimbun-generate-nov nil)
(defvoo nnshimbun-make-contents nil)

(defvoo nnshimbun-status-string "")
(defvoo nnshimbun-nov-buffer-alist nil)
(defvoo nnshimbun-nov-buffer-file-name nil)

(defvoo nnshimbun-keep-backlog 300)
(defvoo nnshimbun-backlog-articles nil)
(defvoo nnshimbun-backlog-hashtb nil)



;;; backlog
(defmacro nnshimbun-backlog (&rest form)
  `(let ((gnus-keep-backlog nnshimbun-keep-backlog)
	 (gnus-backlog-buffer (format " *nnshimbun backlog %s*" nnshimbun-address))
	 (gnus-backlog-articles nnshimbun-backlog-articles)
	 (gnus-backlog-hashtb nnshimbun-backlog-hashtb))
     (unwind-protect
	 (progn ,@form)
       (setq nnshimbun-backlog-articles gnus-backlog-articles
	     nnshimbun-backlog-hashtb gnus-backlog-hashtb))))
(put 'nnshimbun-backlog 'lisp-indent-function 0)
(put 'nnshimbun-backlog 'edebug-form-spec '(form body))



;;; Interface Functions
(nnoo-define-basics nnshimbun)

(deffoo nnshimbun-open-server (server &optional defs)
  (let* ((type (or (car (cdr (assq 'nnshimbun-type defs)))
		   (if (not (equal server "")) (intern server))
		   nnshimbun-default-type))
	 (defaults (cdr (assq type nnshimbun-type-definition))))
    (if (not defaults)
	(nnheader-report 'nnshimbun "Unknown server type: %s" type)
      (unless (assq 'nnshimbun-type defs)
	(setq defs (append defs (list (list 'nnshimbun-type type)))))
      (unless (assq 'nnshimbun-address defs)
	(setq defs (append defs (list (list 'nnshimbun-address
					    (if (equal server "")
						(symbol-name type)
					      server))))))
      (nnoo-change-server 'nnshimbun server defs)
      ;; Set default vaules for defined server.
      (dolist (default defaults)
	(let ((symbol (intern (concat "nnshimbun-" (symbol-name (car default))))))
	  (unless (assq symbol defs)
	    (set symbol (cdr default)))))
      (nnshimbun-possibly-change-server nil server)
      (when (not (file-exists-p nnshimbun-directory))
	(ignore-errors (make-directory nnshimbun-directory t)))
      (cond
       ((not (file-exists-p nnshimbun-directory))
	(nnshimbun-close-server)
	(nnheader-report 'nnshimbun "Couldn't create directory: %s" nnshimbun-directory))
       ((not (file-directory-p (file-truename nnshimbun-directory)))
	(nnshimbun-close-server)
	(nnheader-report 'nnshimbun "Not a directory: %s" nnshimbun-directory))
       (t
	(when (not (file-exists-p nnshimbun-server-directory))
	  (ignore-errors (make-directory nnshimbun-server-directory t)))
	(cond
	 ((not (file-exists-p nnshimbun-server-directory))
	  (nnshimbun-close-server)
	  (nnheader-report 'nnshimbun "Couldn't create directory: %s" nnshimbun-server-directory))
	 ((not (file-directory-p (file-truename nnshimbun-server-directory)))
	  (nnshimbun-close-server)
	  (nnheader-report 'nnshimbun "Not a directory: %s" nnshimbun-server-directory))
	 (t
	  (nnheader-report 'nnshimbun "Opened server %s using directory %s"
			   server nnshimbun-server-directory)
	  t)))))))

(deffoo nnshimbun-close-server (&optional server)
  (when (and (nnshimbun-server-opened server)
	     (gnus-buffer-live-p nnshimbun-buffer))
    (save-excursion
      (set-buffer nnshimbun-buffer)
      (kill-buffer nnshimbun-buffer)))
  (nnshimbun-backlog
    (gnus-backlog-shutdown))
  (nnshimbun-save-nov)
  (nnoo-close-server 'nnshimbun server)
  t)

(defun nnshimbun-get-url (url)
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary))
    (set-buffer-multibyte nil)
    (nnweb-insert url)
    (decode-coding-region (point-min) (point-max) nnshimbun-coding-system)
    (set-buffer-multibyte t)))

(deffoo nnshimbun-request-article (article &optional group server to-buffer)
  (when (nnshimbun-possibly-change-server group server)
    (if (stringp article)
	(setq article (nnshimbun-search-id group article)))
    (if (integerp article)
	(if (nnshimbun-backlog
	      (gnus-backlog-request-article group article 
					    (or to-buffer nntp-server-buffer)))
	    (cons group article)
	  (let (header contents)
	    (when (setq header (save-excursion
				 (set-buffer (nnshimbun-open-nov group))
				 (and (nnheader-find-nov-line article)
				      (nnheader-parse-nov))))
	      (let ((xref (substring (mail-header-xref header) 6)))
		(save-excursion
		  (set-buffer nnshimbun-buffer)
		  (erase-buffer)
		  (nnshimbun-get-url xref)
		  (nnheader-message 6 "nnshimbun: Make contents...")
		  (setq contents (funcall nnshimbun-make-contents header))
		  (nnheader-message 6 "nnshimbun: Make contents...done"))))
	    (when contents
	      (save-excursion
		(set-buffer (or to-buffer nntp-server-buffer))
		(erase-buffer)
		(insert contents)
		(nnshimbun-backlog
		  (gnus-backlog-enter-article group article (current-buffer)))
		(nnheader-report 'nnshimbun "Article %s retrieved" (mail-header-id header))
		(cons group (mail-header-number header))))))
      (nnheader-report 'nnml "Couldn't retrieve article: %s" (prin1-to-string article))
      nil)))

(deffoo nnshimbun-request-group (group &optional server dont-check)
  (let ((pathname-coding-system 'binary))
    (cond
     ((not (nnshimbun-possibly-change-server group server))
      (nnheader-report 'nnshimbun "Invalid group (no such directory)"))
     ((not (file-exists-p nnshimbun-current-directory))
      (nnheader-report 'nnshimbun "Directory %s does not exist"
		       nnshimbun-current-directory))
     ((not (file-directory-p nnshimbun-current-directory))
      (nnheader-report 'nnshimbun "%s is not a directory" nnshimbun-current-directory))
     (dont-check
      (nnheader-report 'nnshimbun "Group %s selected" group)
      t)
     (t
      (let (beg end lines)
	(save-excursion
	  (set-buffer (nnshimbun-open-nov group))
	  (goto-char (point-min))
	  (setq beg (ignore-errors (read (current-buffer))))
	  (goto-char (point-max))
	  (forward-line -1)
	  (setq end (ignore-errors (read (current-buffer)))
		lines (count-lines (point-min) (point-max))))
	(nnheader-report 'nnshimbunw "Selected group %s" group)
	(nnheader-insert "211 %d %d %d %s\n"
			 lines (or beg 0) (or end 0) group))))))

(deffoo nnshimbun-request-scan (&optional group server)
  (nnshimbun-possibly-change-server group server)
  (nnshimbun-generate-nov-database group))

(deffoo nnshimbun-close-group (group &optional server)
  t)

(deffoo nnshimbun-request-list (&optional server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (dolist (group nnshimbun-groups)
      (when (nnshimbun-possibly-change-server group server)
	(let (beg end)
	  (save-excursion
	    (set-buffer (nnshimbun-open-nov group))
	    (goto-char (point-min))
	    (setq beg (ignore-errors (read (current-buffer))))
	    (goto-char (point-max))
	    (forward-line -1)
	    (setq end (ignore-errors (read (current-buffer)))))
	  (insert (format "%s %d %d n\n" group (or end 0) (or beg 0)))))))
  t) ; return value

(eval-and-compile
  (if (fboundp 'mime-entity-fetch-field)
      ;; For Semi-Gnus.
      (defun nnshimbun-insert-header (header)
	(insert "Subject: " (or (mime-entity-fetch-field header 'Subject) "(none)") "\n"
		"From: " (or (mime-entity-fetch-field header 'From) "(nobody)") "\n"
		"Date: " (or (mail-header-date header) "") "\n"
		"Message-ID: " (or (mail-header-id header) (nnmail-message-id)) "\n"
		"References: " (or (mail-header-references header) "") "\n"
		"Lines: ")
	(princ (or (mail-header-lines header) 0) (current-buffer))
	(insert "\n"))
    ;; For pure Gnus.
    (defun nnshimbun-insert-header (header)
      (nnheader-insert-header header)
      (delete-char -1))))

(deffoo nnshimbun-retrieve-headers (articles &optional group server fetch-old)
  (when (nnshimbun-possibly-change-server group server)
    (if (nnshimbun-retrieve-headers-with-nov articles fetch-old)
	'nov
      (save-excursion
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(let (header)
	  (dolist (art articles)
	    (if (stringp art)
		(setq art (nnshimbun-search-id group art)))
	    (if (integerp art)
		(when (setq header
			    (save-excursion
			      (set-buffer (nnshimbun-open-nov group))
			      (and (nnheader-find-nov-line art)
				   (nnheader-parse-nov))))
		  (insert (format "220 %d Article retrieved.\n" art))
		  (nnshimbun-insert-header header)
		  (insert ".\n")
		  (delete-region (point) (point-max))))))
	'header))))

(defun nnshimbun-retrieve-headers-with-nov (articles &optional fetch-old)
  (if (or gnus-nov-is-evil nnshimbun-nov-is-evil)
      nil
    (let ((nov (expand-file-name nnshimbun-nov-file-name nnshimbun-current-directory)))
      (when (file-exists-p nov)
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (erase-buffer)
	  (nnheader-insert-file-contents nov)
	  (if (and fetch-old
		   (not (numberp fetch-old)))
	      t				; Don't remove anything.
	    (nnheader-nov-delete-outside-range
	     (if fetch-old (max 1 (- (car articles) fetch-old))
	       (car articles))
	     (car (last articles)))
	    t))))))



;;; Nov Database Operations

(defun nnshimbun-generate-nov-database (group)
  (prog1 (funcall nnshimbun-generate-nov group)
    (save-excursion
      (set-buffer (nnshimbun-open-nov group))
      (when (buffer-modified-p)
	(nnmail-write-region 1 (point-max) nnshimbun-nov-buffer-file-name
			     nil 'nomesg)))))

(defun nnshimbun-search-id (group id)
  (save-excursion
    (set-buffer (nnshimbun-open-nov group))
    (goto-char (point-min))
    (let (number found)
      (while (and (not found)
		  (search-forward id nil t)) ; We find the ID.
	;; And the id is in the fourth field.
	(if (not (and (search-backward "\t" nil t 4)
		      (not (search-backward "\t" (gnus-point-at-bol) t))))
	    (forward-line 1)
	  (beginning-of-line)
	  (setq found t)
	  ;; We return the article number.
	  (setq number (ignore-errors (read (current-buffer))))))
      number)))

(defun nnshimbun-open-nov (group)
  (let ((buffer (cdr (assoc group nnshimbun-nov-buffer-alist))))
    (if (buffer-live-p buffer)
	buffer
      (setq buffer (gnus-get-buffer-create
		    (format " *nnshimbun overview %s %s*"
			    nnshimbun-address group)))
      (save-excursion
	(set-buffer buffer)
	(set (make-local-variable 'nnshimbun-nov-buffer-file-name)
	     (expand-file-name
	      nnshimbun-nov-file-name
	      (nnmail-group-pathname group nnshimbun-server-directory)))
	(erase-buffer)
	(when (file-exists-p nnshimbun-nov-buffer-file-name)
	  (nnheader-insert-file-contents nnshimbun-nov-buffer-file-name))
	(set-buffer-modified-p nil))
      (push (cons group buffer) nnshimbun-nov-buffer-alist)
      buffer)))

(defun nnshimbun-save-nov ()
  (save-excursion
    (while nnshimbun-nov-buffer-alist
      (when (buffer-name (cdar nnshimbun-nov-buffer-alist))
	(set-buffer (cdar nnshimbun-nov-buffer-alist))
	(when (buffer-modified-p)
	  (nnmail-write-region 1 (point-max) nnshimbun-nov-buffer-file-name
			       nil 'nomesg))
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer)))
      (setq nnshimbun-nov-buffer-alist (cdr nnshimbun-nov-buffer-alist)))))



;;; Server Initialize
(defun nnshimbun-possibly-change-server (group &optional server)
  (when server
    (unless (nnshimbun-server-opened server)
      (nnshimbun-open-server server)))
  (setq nnshimbun-server-directory
	(nnheader-concat nnshimbun-directory (concat nnshimbun-address "/")))
  (unless (gnus-buffer-live-p nnshimbun-buffer)
    (setq nnshimbun-buffer
	  (save-excursion
	    (nnheader-set-temp-buffer
	     (format " *nnshimbun %s %s*" nnshimbun-type server)))))
  (if (not group)
      t
    (let ((pathname (nnmail-group-pathname group nnshimbun-server-directory))
	  (pathname-coding-system 'binary))
      (when (not (equal pathname nnshimbun-current-directory))
	(setq nnshimbun-current-directory pathname
	      nnshimbun-current-group group))
      (when (not (file-exists-p nnshimbun-current-directory))
	(ignore-errors (make-directory nnshimbun-current-directory t)))
      (cond
       ((not (file-exists-p nnshimbun-current-directory))
	(nnheader-report 'nnshimbun "Couldn't create directory: %s" nnshimbun-current-directory))
       ((not (file-directory-p (file-truename nnshimbun-current-directory)))
	(nnheader-report 'nnshimbun "Not a directory: %s" nnshimbun-current-directory))
       (t t)))))



;;; Misc Functions

(eval-and-compile
  (if (fboundp 'eword-encode-string)
      ;; For Semi-Gnus.
      (defun nnshimbun-mime-encode-string (string)
	(mapconcat
	 #'identity
	 (split-string (eword-encode-string (nnweb-decode-entities-string string)) "\n")
	 ""))
    ;; For pure Gnus.
    (defun nnshimbun-mime-encode-string (string)
      (mapconcat
       #'identity
       (split-string 
	(with-temp-buffer
	  (insert (nnweb-decode-entities-string string))
	  (rfc2047-encode-region (point-min) (point-max))
	  (buffer-substring (point-min) (point-max)))
	"\n"))
      "")))

(defun nnshimbun-lapse-seconds (time)
  (let ((now (current-time)))
    (+ (* (- (car now) (car time)) 65536)
       (- (nth 1 now) (nth 1 time)))))


;; Fast fill-region function

(defvar nnshimbun-fill-column (min 80 (- (frame-width) 4)))

(defconst nnshimbun-kinsoku-bol-list
  (funcall
   (if (fboundp 'string-to-char-list)
       'string-to-char-list
     'string-to-list) "\
!)-_~}]:;',.?¡¢¡£¡¤¡¥¡¦¡§¡¨¡©¡ª¡«¡¬¡­¡®¡¯¡°¡±¡²¡³¡´¡µ¡¶¡·¡¸¡¹¡º¡»¡¼¡½¡¾¡¿¡À¡Á\
¡Â¡Ã¡Ä¡Å¡Ç¡É¡Ë¡Í¡Ï¡Ñ¡Ó¡Õ¡×¡Ù¡Û¡ë¡ì¡í¡î¤¡¤£¤¥¤§¤©¤Ã¤ã¤å¤ç¤î¥¡¥£¥¥¥§¥©¥Ã¥ã¥å¥ç¥î¥õ¥ö"))

(defconst nnshimbun-kinsoku-eol-list
  (funcall
   (if (fboundp 'string-to-char-list)
       'string-to-char-list
     'string-to-list)
   "({[`¡Æ¡È¡Ê¡Ì¡Î¡Ð¡Ò¡Ô¡Ö¡Ø¡Ú¡ë¡ì¡í¡ø"))

(defun nnshimbun-fill-line ()
  (forward-line 0)
  (let ((top (point)) chr)
    (while (if (>= (move-to-column fill-column) fill-column)
	       (not (progn
		      (if (memq (preceding-char) nnshimbun-kinsoku-eol-list)
			  (progn
			    (backward-char)
			    (while (memq (preceding-char) nnshimbun-kinsoku-eol-list)
			      (backward-char))
			    (insert "\n"))
			(while (memq (setq chr (following-char)) nnshimbun-kinsoku-bol-list)
			  (forward-char))
			(if (looking-at "\\s-+")
			    (or (eolp) (delete-region (point) (match-end 0)))
			  (or (> (char-width chr) 1)
			      (re-search-backward "\\<" top t)
			      (end-of-line)))
			(or (eolp) (insert "\n"))))))
      (setq top (point))))
  (forward-char)
  (not (eobp)))



;;; www.asahi.com

(defun nnshimbun-asahi-get-headers (group)
  (save-excursion
    (set-buffer nnshimbun-buffer)
    (erase-buffer)
    (nnshimbun-get-url (format "%sp%s.html" nnshimbun-url group))
    (goto-char (point-min))
    (when (search-forward "\n<!-- Start of past -->\n" nil t)
      (delete-region (point-min) (point))
      (when (search-forward "\n<!-- End of past -->\n" nil t)
	(forward-line -1)
	(delete-region (point) (point-max))
	(goto-char (point-min))
	(let (headers)
	  (while (re-search-forward
		  "^¢£<a href=\"\\(\\([0-9][0-9][0-9][0-9]\\)/past/\\([A-z]*[0-9]*\\)\\.html\\)\"> *"
		  nil t)
	    (let ((id (format "<%s%s%%%s>" (match-string 2) (match-string 3) group))
		  (url (match-string 1)))
	      (push (make-full-mail-header
		     0
		     (nnshimbun-mime-encode-string
		      (mapconcat 'identity
				 (split-string
				  (buffer-substring
				   (match-end 0)
				   (progn (search-forward "<br>" nil t) (point)))
				  "<[^>]+>")
				 ""))
		     "webmaster@www.asahi.com"
		     "" id "" 0 0 (concat nnshimbun-url url))
		    headers)))
	  (setq headers (nreverse headers))
	  (let ((i 0))
	    (while (and (nth i headers)
			(re-search-forward
			 "^\\[\\([0-9][0-9]\\)/\\([0-9][0-9]\\) \\([0-9][0-9]:[0-9][0-9]\\)\\]"
			 nil t))
	      (let ((month (string-to-number (match-string 1)))
		    (day (string-to-number (match-string 2)))
		    (time (match-string 3))
		    (date (decode-time (current-time))))
		(mail-header-set-date
		 (nth i headers)
		 (timezone-make-arpa-date (if (and (= 12 month) (= 1 (nth 4 date)))
					      (1- (nth 5 date))
					    (nth 5 date))
					  month
					  day
					  time
					  '(32400))))
	      (setq i (1+ i))))
	  (nreverse headers))))))

(defun nnshimbun-asahi-generate-nov-database (group)
  (save-excursion
    (set-buffer (nnshimbun-open-nov group))
    (let (i)
      (goto-char (point-max))
      (forward-line -1)
      (setq i (or (ignore-errors (read (current-buffer))) 0))
      (goto-char (point-max))
      (dolist (header (nnshimbun-asahi-get-headers group))
	(unless (nnshimbun-search-id group (mail-header-id header))
	  (mail-header-set-number header (setq i (1+ i)))
	  (nnheader-insert-nov header))))))

(defun nnshimbun-asahi-make-contents (header)
  (goto-char (point-min))
  (let (start (html t))
    (when (and (search-forward "\n<!-- Start of kiji -->\n" nil t)
	       (setq start (point))
	       (search-forward "\n<!-- End of kiji -->\n" nil t))
      (delete-region (point-min) start)
      (forward-line -1)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (while (search-forward "<p>" nil t)
	(insert "\n"))
      (nnweb-remove-markup)
      (nnweb-decode-entities)
      (goto-char (point-min))
      (while (not (eobp))
	;(fill-region (point) (gnus-point-at-eol))
	(nnshimbun-fill-line)
	(forward-line 1))
      (setq html nil))
    (goto-char (point-min))
    (nnshimbun-insert-header header)
    (insert "Content-Type: " (if html "text/html" "text/plain")
	    "; charset=ISO-2022-JP\nMIME-Version: 1.0\n\n")
    (encode-coding-string (buffer-string)
			  (if (boundp 'MULE) '*iso-2022-jp* 'iso-2022-jp))))



(provide 'nnshimbun)
;;; nnshimbun.el ends here.

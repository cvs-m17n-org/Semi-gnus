;;; -*- mode: Emacs-Lisp; coding: junet -*-

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

(defvar nnshimbun-check-interval 300)

(defvar nnshimbun-type-definition
  `(("asahi"
     (url . "http://spin.asahi.com/")
     (groups "national" "business" "politics" "international" "sports" "personal" "feneral")
     (coding-system  . ,(if (boundp 'MULE) '*sjis* 'shift_jis))
     (generate-nov   . nnshimbun-generate-nov-for-each-group)
     (get-headers    . nnshimbun-asahi-get-headers)
     (index-url      . (format "%sp%s.html" nnshimbun-url nnshimbun-current-group))
     (from-address   . "webmaster@www.asahi.com")
     (make-contents  . nnshimbun-make-text-or-html-contents)
     (contents-start . "\n<!-- Start of kiji -->\n")
     (contents-end   . "\n<!-- End of kiji -->\n"))
    ("sponichi"
     (url . "http://www.sponichi.co.jp/")
     (groups "baseball" "soccer" "usa" "others" "society" "entertainment" "horseracing")
     (coding-system  . ,(if (boundp 'MULE) '*sjis* 'shift_jis))
     (generate-nov   . nnshimbun-generate-nov-for-each-group)
     (get-headers    . nnshimbun-sponichi-get-headers)
     (index-url      . (format "%s%s/index.html" nnshimbun-url nnshimbun-current-group))
     (from-address   . "webmaster@www.sponichi.co.jp")
     (make-contents  . nnshimbun-make-text-or-html-contents)
     (contents-start . "\n<span class=\"text\">　")
     (contents-end   . "\n"))
    ("cnet"
     (url . "http://cnet.sphere.ne.jp/")
     (groups "comp")
     (coding-system  . ,(if (boundp 'MULE) '*sjis* 'shift_jis))
     (generate-nov   . nnshimbun-generate-nov-for-each-group)
     (get-headers    . nnshimbun-cnet-get-headers)
     (index-url      . (format "%s/News/Oneweek/" nnshimbun-url))
     (from-address   . "cnet@sphere.ad.jp")
     (make-contents  . nnshimbun-make-html-contents)
     (contents-start . "\n<!--KIJI-->\n")
     (contents-end   . "\n<!--/KIJI-->\n"))
    ("wired"
     (url . "http://www.hotwired.co.jp/")
     (groups "business" "culture" "technology")
     (coding-system  . ,(if (boundp 'MULE) '*euc-japan* 'euc-jp))
     (generate-nov   . nnshimbun-generate-nov-for-all-groups)
     (get-headers    . nnshimbun-wired-get-all-headers)
     (index-url)
     (from-address   . "webmaster@www.hotwired.co.jp")
     (make-contents  . nnshimbun-make-html-contents)
     (contents-start . "\n<!-- START_OF_BODY -->\n")
     (contents-end   . "\n<!-- END_OF_BODY -->\n"))
    ("yomiuri"
     (url . "http://www.yomiuri.co.jp/")
     (groups "shakai" "sports" "seiji" "keizai" "kokusai" "fuho")
     (coding-system  . ,(if (boundp 'MULE) '*sjis* 'shift_jis))
     (generate-nov   . nnshimbun-generate-nov-for-all-groups)
     (get-headers    . nnshimbun-yomiuri-get-all-headers)
     (index-url      . (concat nnshimbun-url "main.htm"))
     (from-address   . "webmaster@www.yomiuri.co.jp")
     (make-contents  . nnshimbun-make-text-or-html-contents)
     (contents-start . "\n<!--  honbun start  -->\n")
     (contents-end   . "\n<!--  honbun end  -->\n"))
    ("zdnet"
     (url . "http://zdseek.pub.softbank.co.jp/news/")
     (groups "comp")
     (coding-system  . ,(if (boundp 'MULE) '*sjis* 'shift_jis))
     (generate-nov   . nnshimbun-generate-nov-for-each-group)
     (get-headers    . nnshimbun-zdnet-get-headers)
     (index-url      . nnshimbun-url)
     (from-address   . "zdnn@softbank.co.jp")
     (make-contents  . nnshimbun-make-html-contents)
     (contents-start . "<!--BODY-->")
     (contents-end   . "<!--BODYEND-->"))
    ))

(defvar nnshimbun-x-face-alist
  '(("default" .
     (("default" .
       "X-Face: Ygq$6P.,%Xt$U)DS)cRY@k$VkW!7(X'X'?U{{osjjFG\"E]hND;SPJ-J?O?R|a?L
	g2$0rVng=O3Lt}?~IId8Jj&vP^3*o=LKUyk(`t%0c!;t6REk=JbpsEn9MrN7gZ%"))))
  "Alist of server vs. alist of group vs. X-Face field.  It looks like:

\((\"asahi\" . ((\"national\" . \"X-face: ***\")
	     (\"business\" . \"X-Face: ***\")
		;;
		;;
	     (\"default\" . \"X-face: ***\")))
 (\"sponichi\" . ((\"baseball\" . \"X-face: ***\")
		(\"soccer\" . \"X-Face: ***\")
		;;
		;;
		(\"default\" . \"X-face: ***\")))
		;;
 (\"default\" . ((\"default\" . \"X-face: ***\")))")

(defvoo nnshimbun-directory (nnheader-concat gnus-directory "shimbun/")
  "Where nnshimbun will save its files.")

(defvoo nnshimbun-nov-is-evil nil
  "*Non-nil means that nnshimbun will never retrieve NOV headers.")

(defvoo nnshimbun-nov-file-name ".overview")

(defvoo nnshimbun-pre-fetch-article nil
  "*Non nil means that nnshimbun fetch unread articles when scanning groups.")

;; set by nnshimbun-possibly-change-group
(defvoo nnshimbun-buffer nil)
(defvoo nnshimbun-current-directory nil)
(defvoo nnshimbun-current-group nil)

;; set by nnshimbun-open-server
(defvoo nnshimbun-url nil)
(defvoo nnshimbun-coding-system nil)
(defvoo nnshimbun-groups nil)
(defvoo nnshimbun-generate-nov nil)
(defvoo nnshimbun-get-headers nil)
(defvoo nnshimbun-index-url nil)
(defvoo nnshimbun-from-address nil)
(defvoo nnshimbun-make-contents nil)
(defvoo nnshimbun-contents-start nil)
(defvoo nnshimbun-contents-end nil)
(defvoo nnshimbun-server-directory nil)

(defvoo nnshimbun-status-string "")
(defvoo nnshimbun-nov-last-check nil)
(defvoo nnshimbun-nov-buffer-alist nil)
(defvoo nnshimbun-nov-buffer-file-name nil)

(defvoo nnshimbun-keep-backlog 300)
(defvoo nnshimbun-backlog-articles nil)
(defvoo nnshimbun-backlog-hashtb nil)



;;; backlog
(defmacro nnshimbun-backlog (&rest form)
  `(let ((gnus-keep-backlog nnshimbun-keep-backlog)
	 (gnus-backlog-buffer (format " *nnshimbun backlog %s*" (nnoo-current-server 'nnshimbun)))
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
  ;; Set default values.
  (dolist (default (cdr (assoc server nnshimbun-type-definition)))
    (let ((symbol (intern (concat "nnshimbun-" (symbol-name (car default))))))
      (unless (assq symbol defs)
	(push (list symbol (cdr default)) defs))))
  ;; Set directory for server working files.
  (push (list 'nnshimbun-server-directory
	      (file-name-as-directory
	       (expand-file-name server nnshimbun-directory)))
	defs)
  (nnoo-change-server 'nnshimbun server defs)
  (nnshimbun-possibly-change-group nil server)
  ;; Make directories.
  (unless (file-exists-p nnshimbun-directory)
    (ignore-errors (make-directory nnshimbun-directory t)))
  (cond
   ((not (file-exists-p nnshimbun-directory))
    (nnshimbun-close-server)
    (nnheader-report 'nnshimbun "Couldn't create directory: %s" nnshimbun-directory))
   ((not (file-directory-p (file-truename nnshimbun-directory)))
    (nnshimbun-close-server)
    (nnheader-report 'nnshimbun "Not a directory: %s" nnshimbun-directory))
   (t
    (unless (file-exists-p nnshimbun-server-directory)
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
      t)))))

(deffoo nnshimbun-close-server (&optional server)
  (and (nnshimbun-server-opened server)
       (gnus-buffer-live-p nnshimbun-buffer)
       (kill-buffer nnshimbun-buffer))
  (nnshimbun-backlog (gnus-backlog-shutdown))
  (nnshimbun-save-nov)
  (nnoo-close-server 'nnshimbun server)
  t)

(defun nnshimbun-retrieve-url (url &optional no-cache)
  "Rertrieve URL contents and insert to current buffer."
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary))
    (set-buffer-multibyte nil)
    ;; Following code is imported from `url-insert-file-contents'.
    (save-excursion
      (let ((old-asynch (default-value 'url-be-asynchronous))
	    (old-caching (default-value 'url-automatic-caching))
	    (old-mode (default-value 'url-standalone-mode)))
	(unwind-protect
	    (progn
	      (setq-default url-be-asynchronous nil)
	      (when no-cache
		(setq-default url-automatic-caching nil)
		(setq-default url-standalone-mode nil))
	      (let ((buf (current-buffer))
		    (url-working-buffer (cdr (url-retrieve url no-cache))))
		(set-buffer url-working-buffer)
		(url-uncompress)
		(set-buffer buf)
		(insert-buffer url-working-buffer)
		(save-excursion
		  (set-buffer url-working-buffer)
		  (set-buffer-modified-p nil))
		(kill-buffer url-working-buffer)))
	  (setq-default url-be-asynchronous old-asynch)
	  (setq-default url-automatic-caching old-caching)
	  (setq-default url-standalone-mode old-mode))))
    ;; Modify buffer coding system.
    (decode-coding-region (point-min) (point-max) nnshimbun-coding-system)
    (set-buffer-multibyte t)))

(deffoo nnshimbun-request-article (article &optional group server to-buffer)
  (when (nnshimbun-possibly-change-group group server)
    (if (stringp article)
	(setq article (nnshimbun-search-id group article)))
    (if (integerp article)
	(nnshimbun-request-article-1 article group server to-buffer)
      (nnheader-report 'nnml "Couldn't retrieve article: %s" (prin1-to-string article))
      nil)))

(defun nnshimbun-request-article-1 (article &optional group server to-buffer)
  (if (nnshimbun-backlog
	(gnus-backlog-request-article
	 group article (or to-buffer nntp-server-buffer)))
      (cons group article)
    (let (header contents)
      (when (setq header (save-excursion
			   (set-buffer (nnshimbun-open-nov group))
			   (and (nnheader-find-nov-line article)
				(nnheader-parse-nov))))
	(let* ((xref (substring (mail-header-xref header) 6))
	       (x-faces (cdr (or (assoc server nnshimbun-x-face-alist)
				 (assoc "default" nnshimbun-x-face-alist))))
	       (x-face (cdr (or (assoc group x-faces)
				(assoc "default" x-faces)))))
	  (save-excursion
	    (set-buffer nnshimbun-buffer)
	    (erase-buffer)
	    (nnshimbun-retrieve-url xref)
	    (nnheader-message 6 "nnshimbun: Make contents...")
	    (goto-char (point-min))
	    (setq contents (funcall nnshimbun-make-contents header x-face))
	    (nnheader-message 6 "nnshimbun: Make contents...done"))))
      (when contents
	(save-excursion
	  (set-buffer (or to-buffer nntp-server-buffer))
	  (erase-buffer)
	  (insert contents)
	  (nnshimbun-backlog
	    (gnus-backlog-enter-article group article (current-buffer)))
	  (nnheader-report 'nnshimbun "Article %s retrieved" (mail-header-id header))
	  (cons group (mail-header-number header)))))))

(deffoo nnshimbun-request-group (group &optional server dont-check)
  (let ((pathname-coding-system 'binary))
    (cond
     ((not (nnshimbun-possibly-change-group group server))
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
  (nnshimbun-possibly-change-group group server)
  (nnshimbun-generate-nov-database group))

(deffoo nnshimbun-close-group (group &optional server)
  (nnshimbun-write-nov group)
  t)

(deffoo nnshimbun-request-list (&optional server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (dolist (group nnshimbun-groups)
      (when (nnshimbun-possibly-change-group group server)
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
	(insert "\n")
	(if (mail-header-xref header)
	    (insert (mail-header-xref header) "\n")))
    ;; For pure Gnus.
    (defun nnshimbun-insert-header (header)
      (nnheader-insert-header header)
      (delete-char -1)
      (if (mail-header-xref header)
	  (insert (mail-header-xref header) "\n")))))

(deffoo nnshimbun-retrieve-headers (articles &optional group server fetch-old)
  (when (nnshimbun-possibly-change-group group server)
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
	  (if (and fetch-old (not (numberp fetch-old)))
	      t				; Don't remove anything.
	    (nnheader-nov-delete-outside-range
	     (if fetch-old (max 1 (- (car articles) fetch-old))
	       (car articles))
	     (car (last articles)))
	    t))))))



;;; Nov Database Operations

(defun nnshimbun-generate-nov-database (group)
  (prog1 (funcall nnshimbun-generate-nov group)
    (nnshimbun-write-nov group)))

(defun nnshimbun-generate-nov-for-each-group (group)
  (nnshimbun-possibly-change-group group)
  (save-excursion
    (set-buffer (nnshimbun-open-nov group))
    (let (i)
      (goto-char (point-max))
      (forward-line -1)
      (setq i (or (ignore-errors (read (current-buffer))) 0))
      (dolist (header (save-excursion
			(set-buffer nnshimbun-buffer)
			(erase-buffer)
			(nnshimbun-retrieve-url (eval nnshimbun-index-url) t)
			(goto-char (point-min))
			(funcall nnshimbun-get-headers)))
	(unless (nnshimbun-search-id group (mail-header-id header))
	  (mail-header-set-number header (setq i (1+ i)))
	  (goto-char (point-max))
	  (nnheader-insert-nov header)
	  (if nnshimbun-pre-fetch-article
	      (nnshimbun-request-article-1 i group nil nnshimbun-buffer)))))))

(defun nnshimbun-generate-nov-for-all-groups (&rest args)
  (unless (and nnshimbun-nov-last-check
	       (< (nnshimbun-lapse-seconds nnshimbun-nov-last-check)
		  nnshimbun-check-interval))
    (save-excursion
      (dolist (list (funcall nnshimbun-get-headers))
	(let ((group (car list)))
	  (nnshimbun-possibly-change-group group)
	  (when (cdr list)
	    (set-buffer (nnshimbun-open-nov group))
	    (let (i)
	      (goto-char (point-max))
	      (forward-line -1)
	      (setq i (or (ignore-errors (read (current-buffer))) 0))
	      (dolist (header (cdr list))
		(unless (nnshimbun-search-id group (mail-header-id header))
		  (mail-header-set-number header (setq i (1+ i)))
		  (goto-char (point-max))
		  (nnheader-insert-nov header)
		  (if nnshimbun-pre-fetch-article
		      (nnshimbun-request-article-1 i group nil nnshimbun-buffer))))))))
      (nnshimbun-save-nov)
      (setq nnshimbun-nov-last-check (current-time)))))

(defun nnshimbun-search-id (group id &optional nov)
  (save-excursion
    (set-buffer (nnshimbun-open-nov group))
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
		  (search-forward id nil t)) ; We find the ID.
	;; And the id is in the fourth field.
	(if (not (and (search-backward "\t" nil t 4)
		      (not (search-backward "\t" (gnus-point-at-bol) t))))
	    (forward-line 1)
	  (forward-line 0)
	  (setq found t)))
      (unless found
	(goto-char (point-min))
	(when (search-forward (concat "X-Nnshimbun-Original-Id: " id) nil t)
	  (forward-line 0)
	  (setq found t)))
      (if found
	  (if nov
	      (nnheader-parse-nov)
	    ;; We return the article number.
	    (ignore-errors (read (current-buffer))))))))

(defun nnshimbun-nov-fix-header (group header args)
  (save-excursion
    (set-buffer (nnshimbun-open-nov group))
    (when (nnheader-find-nov-line (mail-header-number header))
      (dolist (arg args)
	(if (eq (car arg) 'id)
	    (let ((extra (mail-header-extra header)) x)
	      (when (setq x (assq 'X-Nnshimbun-Original-Id extra))
		(setq extra (delq x extra)))
	      (mail-header-set-extra
	       header
	       (cons (cons 'X-Nnshimbun-Original-Id (cdr arg)) extra)))
	  (let ((func (intern (concat "mail-header-set-" (symbol-name (car arg))))))
	    (if (cdr arg) (eval (list func header (cdr arg)))))))
      (let ((xref (mail-header-xref header)))
	(when (string-match "^Xref: " xref)
	  (mail-header-set-xref header (substring xref 6))))
      (delete-region (point) (progn (forward-line 1) (point)))
      (nnheader-insert-nov header))))

(defun nnshimbun-open-nov (group)
  (let ((buffer (cdr (assoc group nnshimbun-nov-buffer-alist))))
    (if (buffer-live-p buffer)
	buffer
      (setq buffer (gnus-get-buffer-create
		    (format " *nnshimbun overview %s %s*"
			    (nnoo-current-server 'nnshimbun) group)))
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

(defun nnshimbun-write-nov (group)
  (let ((buffer (cdr (assoc group nnshimbun-nov-buffer-alist))))
    (when (buffer-live-p buffer)
      (save-excursion
	(set-buffer buffer)
	(buffer-modified-p)
	(nnmail-write-region 1 (point-max) nnshimbun-nov-buffer-file-name
			     nil 'nomesg)))))

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
(defun nnshimbun-possibly-change-group (group &optional server)
  (when server
    (unless (nnshimbun-server-opened server)
      (nnshimbun-open-server server)))
  (unless (gnus-buffer-live-p nnshimbun-buffer)
    (setq nnshimbun-buffer
	  (save-excursion
	    (nnheader-set-temp-buffer
	     (format " *nnshimbun %s*" (nnoo-current-server 'nnshimbun))))))
  (if (not group)
      t
    (let ((pathname (nnmail-group-pathname group nnshimbun-server-directory))
	  (pathname-coding-system 'binary))
      (unless (equal pathname nnshimbun-current-directory)
	(setq nnshimbun-current-directory pathname
	      nnshimbun-current-group group))
      (unless (file-exists-p nnshimbun-current-directory)
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
	"\n")
       ""))))

(defun nnshimbun-lapse-seconds (time)
  (let ((now (current-time)))
    (+ (* (- (car now) (car time)) 65536)
       (- (nth 1 now) (nth 1 time)))))

(defun nnshimbun-make-date-string (year month day &optional time)
  (format "%02d %s %04d %s +0900"
	  day
	  (aref [nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
		     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
		month)
	  (cond ((< year 69)
		 (+ year 2000))
		((< year 100)
		 (+ year 1900))
		((< year 1000)	; possible 3-digit years.
		 (+ year 1900))
		(t year))
	  (or time "00:00")))

(if (fboundp 'regexp-opt)
    (defalias 'nnshimbun-regexp-opt 'regexp-opt)
  (defun nnshimbun-regexp-opt (strings &optional paren)
    "Return a regexp to match a string in STRINGS.
Each string should be unique in STRINGS and should not contain any regexps,
quoted or not.  If optional PAREN is non-nil, ensure that the returned regexp
is enclosed by at least one regexp grouping construct."
    (let ((open-paren (if paren "\\(" "")) (close-paren (if paren "\\)" "")))
      (concat open-paren (mapconcat 'regexp-quote strings "\\|") close-paren))))


;; Fast fill-region function

(defvar nnshimbun-fill-column (min 80 (- (frame-width) 4)))

(defconst nnshimbun-kinsoku-bol-list
  (funcall
   (if (fboundp 'string-to-char-list)
       'string-to-char-list
     'string-to-list) "\
!)-_~}]:;',.?、。，．・：；？！゛゜´｀¨＾￣＿ヽヾゝゞ〃仝々〆〇ー―‐／＼〜\
‖｜…‥’”）〕］｝〉》」』】°′″℃ぁぃぅぇぉっゃゅょゎァィゥェォッャュョヮヵヶ"))

(defconst nnshimbun-kinsoku-eol-list
  (funcall
   (if (fboundp 'string-to-char-list)
       'string-to-char-list
     'string-to-list)
   "({[`‘“（〔［｛〈《「『【°′″§"))

(defun nnshimbun-fill-line ()
  (forward-line 0)
  (let ((top (point)) chr)
    (while (if (>= (move-to-column nnshimbun-fill-column)
		   nnshimbun-fill-column)
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
  (forward-line 1)
  (not (eobp)))

(defsubst nnshimbun-shallow-rendering ()
  (goto-char (point-min))
  (while (search-forward "<p>" nil t)
    (insert "\n\n"))
  (goto-char (point-min))
  (while (search-forward "<br>" nil t)
    (insert "\n"))
  (nnweb-remove-markup)
  (nnweb-decode-entities)
  (goto-char (point-min))
  (while (nnshimbun-fill-line))
  (goto-char (point-min))
  (when (skip-chars-forward "\n")
    (delete-region (point-min) (point)))
  (while (search-forward "\n\n" nil t)
    (let ((p (point)))
      (when (skip-chars-forward "\n")
	(delete-region p (point)))))
  (goto-char (point-max))
  (when (skip-chars-backward "\n")
    (delete-region (point) (point-max)))
  (insert "\n"))

(defun nnshimbun-make-text-or-html-contents (header &optional x-face)
  (let ((case-fold-search t) (html t) (start))
    (when (and (search-forward nnshimbun-contents-start nil t)
	       (setq start (point))
	       (search-forward nnshimbun-contents-end nil t))
      (delete-region (point-min) start)
      (delete-region (- (point) (length nnshimbun-contents-end)) (point-max))
      (nnshimbun-shallow-rendering)
      (setq html nil))
    (goto-char (point-min))
    (nnshimbun-insert-header header)
    (insert "Content-Type: " (if html "text/html" "text/plain")
	    "; charset=ISO-2022-JP\nMIME-Version: 1.0\n")
    (when x-face
      (insert x-face)
      (unless (bolp)
	(insert "\n")))
    (insert "\n")
    (encode-coding-string (buffer-string)
			  (mime-charset-to-coding-system "ISO-2022-JP"))))

(defun nnshimbun-make-html-contents (header &optional x-face)
  (let (start)
    (when (and (search-forward nnshimbun-contents-start nil t)
	       (setq start (point))
	       (search-forward nnshimbun-contents-end nil t))
      (delete-region (point-min) start)
      (delete-region (- (point) (length nnshimbun-contents-end)) (point-max)))
    (goto-char (point-min))
    (nnshimbun-insert-header header)
    (insert "Content-Type: text/html; charset=ISO-2022-JP\n"
	    "MIME-Version: 1.0\n")
    (when x-face
      (insert x-face)
      (unless (bolp)
	(insert "\n")))
    (insert "\n")
    (encode-coding-string (buffer-string)
			  (mime-charset-to-coding-system "ISO-2022-JP"))))



;;; www.asahi.com

(defun nnshimbun-asahi-get-headers ()
  (when (search-forward "\n<!-- Start of past -->\n" nil t)
    (delete-region (point-min) (point))
    (when (search-forward "\n<!-- End of past -->\n" nil t)
      (forward-line -1)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (let (headers)
	(while (re-search-forward
		"^■<a href=\"\\(\\([0-9][0-9][0-9][0-9]\\)/past/\\([A-z]*[0-9]*\\)\\.html\\)\"> *"
		nil t)
	  (let ((id (format "<%s%s%%%s>"
			    (match-string 2)
			    (match-string 3)
			    nnshimbun-current-group))
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
		   nnshimbun-from-address
		   "" id "" 0 0 (concat nnshimbun-url url))
		  headers)))
	(setq headers (nreverse headers))
	(let ((i 0))
	  (while (and (nth i headers)
		      (re-search-forward
		       "^\\[\\([0-9][0-9]\\)/\\([0-9][0-9]\\) \\([0-9][0-9]:[0-9][0-9]\\)\\]"
		       nil t))
	    (let ((month (string-to-number (match-string 1)))
		  (date (decode-time (current-time))))
	      (mail-header-set-date
	       (nth i headers)
	       (nnshimbun-make-date-string
		(if (and (eq 12 month) (eq 1 (nth 4 date)))
		    (1- (nth 5 date))
		  (nth 5 date))
		month
		(string-to-number (match-string 2))
		(match-string 3))))
	    (setq i (1+ i))))
	(nreverse headers)))))



;;; www.sponichi.co.jp

(defun nnshimbun-sponichi-get-headers ()
  (when (search-forward "ニュースインデックス" nil t)
    (delete-region (point-min) (point))
    (when (search-forward "アドタグ" nil t)
      (forward-line 2)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (let ((case-fold-search t) headers)
	(while (re-search-forward
		"^<a href=\"/\\(\\([A-z]*\\)/kiji/\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)\\.html\\)\">"
		nil t)
	  (let ((url (match-string 1))
		(id (format "<%s%s%s%s%%%s>"
			    (match-string 3)
			    (match-string 4)
			    (match-string 5)
			    (match-string 6)
			    nnshimbun-current-group))
		(date (nnshimbun-make-date-string
		       (string-to-number (match-string 3))
		       (string-to-number (match-string 4))
		       (string-to-number (match-string 5)))))
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
		   nnshimbun-from-address
		   date id "" 0 0 (concat nnshimbun-url url))
		  headers)))
	headers))))



;;; CNET Japan

(defun nnshimbun-cnet-get-headers ()
  (let ((case-fold-search t) headers)
    (while (search-forward "\n<!--*****見出し*****-->\n" nil t)
      (let ((subject (buffer-substring (point) (gnus-point-at-eol)))
	    (point (point)))
	(forward-line -2)
	(when (looking-at "<a href=\"/\\(News/\\([0-9][0-9][0-9][0-9]\\)/Item/\\([0-9][0-9]\\([0-9][0-9]\\)\\([0-9][0-9]\\)-[0-9]+\\).html\\)\">")
	  (let ((url (match-string 1))
		(id  (format "<%s%s%%%s>"
			     (match-string 2)
			     (match-string 3)
			     nnshimbun-current-group))
		(date (nnshimbun-make-date-string
		       (string-to-number (match-string 2))
		       (string-to-number (match-string 4))
		       (string-to-number (match-string 5)))))
	    (push (make-full-mail-header
		   0
		   (nnshimbun-mime-encode-string subject)
		   nnshimbun-from-address
		   date id "" 0 0 (concat nnshimbun-url url))
		  headers)))
	(goto-char point)))
    headers))



;;; Wired

(defun nnshimbun-wired-get-all-headers ()
  (save-excursion
    (set-buffer nnshimbun-buffer)
    (let ((group-header-alist (mapcar (lambda (g) (cons g nil)) nnshimbun-groups))
	  (case-fold-search t)
	  (regexp (format
		   "<a href=\"\\(%s\\|/\\)\\(news/news/\\(%s\\)/story/\\(\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)[0-9]+\\)\\.html\\)\"><b>"
		   (regexp-quote nnshimbun-url)
		   (nnshimbun-regexp-opt nnshimbun-groups))))
      (dolist (xover (list (concat nnshimbun-url "news/news/index.html")
			   (concat nnshimbun-url "news/news/last_seven.html")))
	(erase-buffer)
	(nnshimbun-retrieve-url xover t)
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (let* ((url   (concat nnshimbun-url (match-string 2)))
		 (group (downcase (match-string 3)))
		 (id    (format "<%s%%%s>" (match-string 4) group))
		 (date  (nnshimbun-make-date-string
			 (string-to-number (match-string 5))
			 (string-to-number (match-string 6))
			 (string-to-number (match-string 7))))
		 (header (make-full-mail-header
			  0
			  (nnshimbun-mime-encode-string
			   (mapconcat 'identity
				      (split-string
				       (buffer-substring
					(match-end 0)
					(progn (search-forward "</b>" nil t) (point)))
				       "<[^>]+>")
				      ""))
			  nnshimbun-from-address
			  date id "" 0 0 url))
		 (x (assoc group group-header-alist)))
	    (setcdr x (cons header (cdr x))))))
      group-header-alist)))



;;; www.yomiuri.co.jp

(defun nnshimbun-yomiuri-get-all-headers ()
  (save-excursion
    (set-buffer nnshimbun-buffer)
    (erase-buffer)
    (nnshimbun-retrieve-url (eval nnshimbun-index-url) t)
    (let ((case-fold-search t)
	  (group-header-alist (mapcar (lambda (g) (cons g nil)) nnshimbun-groups)))
      (dolist (group nnshimbun-groups)
	(let (start)
	  (goto-char (point-min))
	  (when (and (search-forward (format "\n<!-- /news/%s=start -->\n" group) nil t)
		     (setq start (point))
		     (search-forward (format "\n<!-- /news/%s=end -->\n" group) nil t))
	    (forward-line -1)
	    (save-restriction
	      (narrow-to-region start (point))
	      (goto-char start)
	      (while (re-search-forward
		      "<a href=\"/\\([0-9]+\\)/\\(\\(\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)[A-z0-9]+\\)\\.htm\\)\"[^>]*>"
		      nil t)
		(let ((url   (concat (match-string 1) "a/" (match-string 2)))
		      (id    (format "<%s%s%%%s>"
				     (match-string 1)
				     (match-string 3)
				     group))
		      (year  (string-to-number (match-string 4)))
		      (month (string-to-number (match-string 5)))
		      (day   (string-to-number (match-string 6)))
		      (subject (mapconcat
				'identity
				(split-string
				 (buffer-substring
				  (match-end 0)
				  (progn (search-forward "<br>" nil t) (point)))
				 "<[^>]+>")
				""))
		      date x)
		  (when (string-match "^◆" subject)
		    (setq subject (substring subject (match-end 0))))
		  (if (string-match "(\\([0-9][0-9]:[0-9][0-9]\\))$" subject)
		      (setq date (nnshimbun-make-date-string
				  year month day (match-string 1 subject))
			    subject (substring subject 0 (match-beginning 0)))
		    (setq date (nnshimbun-make-date-string year month day)))
		  (setcdr (setq x (assoc group group-header-alist))
			  (cons (make-full-mail-header
				 0
				 (nnshimbun-mime-encode-string subject)
				 nnshimbun-from-address
				 date id "" 0 0 (concat nnshimbun-url url))
				(cdr x)))))))))
      group-header-alist)))



;;; Zdnet Japan

(defun nnshimbun-zdnet-get-headers ()
  (let ((case-fold-search t) headers)
    (goto-char (point-min))
    (let (start)
      (while (and (search-forward "<!--" nil t)
		  (setq start (- (point) 4))
		  (search-forward "-->" nil t))
	(delete-region start (point))))
    (goto-char (point-min))
    (while (re-search-forward
	    "<a href=\"\\(\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([^\\.]+\\).html\\)\"><font size=\"4\"><strong>"
	    nil t)
      (let ((year  (+ 2000 (string-to-number (match-string 2))))
	    (month (string-to-number (match-string 3)))
	    (day   (string-to-number (match-string 4)))
	    (id    (format "<%s%s%s%s%%%s>"
			   (match-string 2)
			   (match-string 3)
			   (match-string 4)
			   (match-string 5)
			   nnshimbun-current-group))
	    (url (match-string 1)))
	(push (make-full-mail-header
	       0
	       (nnshimbun-mime-encode-string
		(mapconcat 'identity
			   (split-string
			    (buffer-substring
			     (match-end 0)
			     (progn (search-forward "</a>" nil t) (point)))
			    "<[^>]+>")
			   ""))
	       nnshimbun-from-address
	       (nnshimbun-make-date-string year month day)
	       id  "" 0 0 (concat nnshimbun-url url))
	      headers)))
    (nreverse headers)))



(provide 'nnshimbun)
;;; nnshimbun.el ends here.

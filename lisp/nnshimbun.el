;;; nnshimbun.el --- interfacing with web newspapers -*- coding: junet; -*-

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Akihiro Arisawa    <ari@atesoft.advantest.co.jp>,
;;          Katsumi Yamaoka    <yamaoka@jpl.org>,
;;          Yuuichi Teranishi  <teranisi@gohome.org>
;; Keywords: news

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

;;; Definitions:

(gnus-declare-backend "nnshimbun" 'address)

(eval-when-compile (require 'cl))
(eval-when-compile (require 'gnus-clfns))

(require 'nnheader)
(require 'nnmail)
(require 'nnoo)
(require 'gnus-bcklg)
(require 'shimbun)


(nnoo-declare nnshimbun)

(defvoo nnshimbun-directory (nnheader-concat gnus-directory "shimbun/")
  "Where nnshimbun will save its files.")

(defvoo nnshimbun-nov-is-evil nil
  "*Non-nil means that nnshimbun will never retrieve NOV headers.")

(defvoo nnshimbun-nov-file-name ".overview")

(defvoo nnshimbun-pre-fetch-article nil
  "*Non nil means that nnshimbun fetch unread articles when scanning groups.")

(defvoo nnshimbun-use-entire-index t
  "*Nil means that nnshimbun check the last index of articles.")

;; set by nnshimbun-possibly-change-group
(defvoo nnshimbun-buffer nil)
(defvoo nnshimbun-current-directory nil)
(defvoo nnshimbun-current-group nil)

;; set by nnshimbun-open-server
(defvoo nnshimbun-shimbun nil)
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
  (push (list 'nnshimbun-shimbun
	      (condition-case err
		  (shimbun-open server (luna-make-entity 'shimbun-gnus-mua))
		(error (nnheader-report 'nnshimbun "%s" (error-message-string err)))))
	defs)
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
  (shimbun-close nnshimbun-shimbun)
  (and (nnshimbun-server-opened server)
       (gnus-buffer-live-p nnshimbun-buffer)
       (kill-buffer nnshimbun-buffer))
  (nnshimbun-backlog (gnus-backlog-shutdown))
  (nnshimbun-save-nov)
  (nnoo-close-server 'nnshimbun server)
  t)

(defsubst nnshimbun-header-xref (x)
  (if (and (setq x (mail-header-xref x))
	   (string-match "^Xref: " x))
      (substring x 6)
    x))

(eval-and-compile
  (if (fboundp 'mime-entity-fetch-field)
      ;; For Semi-Gnus.
      (defun nnshimbun-make-shimbun-header (header)
	(shimbun-make-header
	 (mail-header-number header)
	 (mime-entity-fetch-field header 'Subject)
	 (mime-entity-fetch-field header 'From)
	 (mail-header-date header)
	 (or (cdr (assq 'X-Nnshimbun-Id (mail-header-extra header)))
	     (mail-header-id header))
	 (mail-header-references header)
	 (mail-header-chars header)
	 (mail-header-lines header)
	 (nnshimbun-header-xref header)))
    ;; For pure Gnus.
    (defun nnshimbun-make-shimbun-header (header)
      (shimbun-make-header
       (mail-header-number header)
       (mail-header-subject header)
       (mail-header-from header)
       (mail-header-date header)
       (or (cdr (assq 'X-Nnshimbun-Id (mail-header-extra header)))
	   (mail-header-id header))
       (mail-header-references header)
       (mail-header-chars header)
       (mail-header-lines header)
       (nnshimbun-header-xref header)))))

(defsubst nnshimbun-check-header (group header)
  (let (flag)
    ;; Check message-id.
    (let ((id (std11-field-body "message-id")))
      (when (and id (not (string= id (mail-header-id header))))
	(let ((extra (mail-header-extra header)))
	  (unless (assq 'X-Nnshimbun-Id extra)
	    (push (cons 'X-Nnshimbun-Id (mail-header-id header)) extra)
	    (mail-header-set-extra header extra)))
	(mail-header-set-id header id)
	(setq flag t)))
    ;; Check references.
    (when (string= "" (mail-header-references header))
      (let ((refs (std11-field-body "references")))
	(when refs
	  (mail-header-set-references header (std11-unfold-string refs))))
      (setq flag t))
    (when flag
      ;; Replace header.
      (with-current-buffer (nnshimbun-open-nov group)
	(when (nnheader-find-nov-line (mail-header-number header))
	  (mail-header-set-xref header (nnshimbun-header-xref header))
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (nnheader-insert-nov header))))))

(defun nnshimbun-request-article-1 (article &optional group server to-buffer)
  (if (nnshimbun-backlog
	(gnus-backlog-request-article
	 group article (or to-buffer nntp-server-buffer)))
      (cons group article)
    (let ((header (with-current-buffer (nnshimbun-open-nov group)
		    (and (nnheader-find-nov-line article)
			 (nnheader-parse-nov)))))
      (when header
	(with-current-buffer (or to-buffer nntp-server-buffer)
	  (delete-region (point-min) (point-max))
	  (shimbun-article nnshimbun-shimbun
			   (nnshimbun-make-shimbun-header header))
	  (when (> (buffer-size) 0)
	    (nnshimbun-check-header group header)
	    (nnshimbun-backlog
	      (gnus-backlog-enter-article group article (current-buffer)))
	    (nnheader-report 'nnshimbun "Article %s retrieved"
			     (mail-header-id header))
	    (cons group (mail-header-number header))))))))

(deffoo nnshimbun-request-article (article &optional group server to-buffer)
  (when (nnshimbun-possibly-change-group group server)
    (when (stringp article)
      (setq article (nnshimbun-search-id group article)))
    (if (integerp article)
	(nnshimbun-request-article-1 article group server to-buffer)
      (nnheader-report 'nnshimbun "Couldn't retrieve article: %s"
		       (prin1-to-string article))
      nil)))

(deffoo nnshimbun-request-group (group &optional server dont-check)
  (let ((file-name-coding-system nnmail-pathname-coding-system)
	(pathname-coding-system nnmail-pathname-coding-system))
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
  (with-current-buffer nntp-server-buffer
    (delete-region (point-min) (point-max))
    (dolist (group (shimbun-groups nnshimbun-shimbun))
      (when (nnshimbun-possibly-change-group group server)
	(let (beg end)
	  (with-current-buffer (nnshimbun-open-nov group)
	    (goto-char (point-min))
	    (setq beg (ignore-errors (read (current-buffer))))
	    (goto-char (point-max))
	    (forward-line -1)
	    (setq end (ignore-errors (read (current-buffer)))))
	  (insert (format "%s %d %d n\n" group (or end 0) (or beg 0)))))))
  t) ; return value

(deffoo nnshimbun-retrieve-headers (articles &optional group server fetch-old)
  (when (nnshimbun-possibly-change-group group server)
    (if (nnshimbun-retrieve-headers-with-nov articles fetch-old)
	'nov
      (with-current-buffer nntp-server-buffer
	(delete-region (point-min) (point-max))
	(let (header)
	  (dolist (art articles)
	    (if (stringp art)
		(setq art (nnshimbun-search-id group art)))
	    (if (integerp art)
		(when (setq header
			    (with-current-buffer (nnshimbun-open-nov group)
			      (and (nnheader-find-nov-line art)
				   (nnheader-parse-nov))))
		  (insert (format "220 %d Article retrieved.\n" art))
		  (shimbun-header-insert
		   nnshimbun-shimbun
		   (nnshimbun-make-shimbun-header header))
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
  (nnshimbun-possibly-change-group group)
  (let (i)
  (with-current-buffer (nnshimbun-open-nov group)
    (goto-char (point-max))
    (forward-line -1)
    (setq i (or (ignore-errors (read (current-buffer))) 0))
    (dolist (header (shimbun-headers nnshimbun-shimbun))
      (unless (nnshimbun-search-id group (shimbun-header-id header))
	(goto-char (point-max))
	(nnheader-insert-nov
	 (make-full-mail-header (setq i (1+ i))
				(shimbun-header-subject header)
				(shimbun-header-from header)
				(shimbun-header-date header)
				(shimbun-header-id header)
				(shimbun-header-references header)
				(shimbun-header-chars header)
				(shimbun-header-lines header)
				(shimbun-header-xref header)))
	(if nnshimbun-pre-fetch-article
	    (nnshimbun-request-article-1 i group nil nnshimbun-buffer)))))
  (nnshimbun-write-nov group)))

(defun nnshimbun-search-id (group id &optional nov)
  (with-current-buffer (nnshimbun-open-nov group)
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
	(setq id (concat "X-Nnshimbun-Id: " id))
	(while (and (not found)
		    (search-forward id nil t))
	  (if (not (search-backward "\t" (gnus-point-at-bol) t 8))
	      (forward-line 1)
	    (forward-line 0)
	    (setq found t))))
      (if found
	  (if nov
	      (nnheader-parse-nov)
	    ;; We return the article number.
	    (ignore-errors (read (current-buffer))))))))

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
    (condition-case err
	(shimbun-open-group nnshimbun-shimbun group)
      (error (nnheader-report 'nnshimbun "%s" (error-message-string err))))
    (let ((pathname (nnmail-group-pathname group nnshimbun-server-directory))
	  (file-name-coding-system nnmail-pathname-coding-system)
	  (pathname-coding-system nnmail-pathname-coding-system))
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



;;; shimbun-gnus-mua
(luna-define-class shimbun-gnus-mua (shimbun-mua) ())

(luna-define-method shimbun-mua-search-id ((mua shimbun-gnus-mua) id)
  (nnshimbun-search-id
   (shimbun-current-group-internal (shimbun-mua-shimbun-internal mua))
   id))

(luna-define-method shimbun-mua-use-entire-index ((mua shimbun-gnus-mua))
  nnshimbun-use-entire-index)


(provide 'nnshimbun)
;;; nnshimbun.el ends here.

;;; nnshimbun.el --- interfacing with web newspapers

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

;; Gnus (or gnus) backend to read newspapers on the World Wide Web.
;; This module requires the Emacs-W3M and the external command W3M.
;; Visit the following pages for more information.
;;
;;	http://namazu.org/~tsuchiya/emacs-w3m/
;;	http://ei5nazha.yz.yamagata-u.ac.jp/~aito/w3m/

;; If you would like to use this module in Gnus (not T-gnus), put this
;; file into the lisp/ directory in the Gnus source tree and run `make
;; install'.  And then, put the following expression into your ~/.gnus.
;;
;; (autoload 'gnus-group-make-shimbun-group "nnshimbun" nil t)


;;; Definitions:

(eval-when-compile (require 'cl))
(require 'nnheader)
(require 'nnmail)
(require 'nnoo)
(require 'gnus)
(require 'gnus-bcklg)
(require 'shimbun)
(require 'message)


;; Customize variables
(defgroup nnshimbun nil
  "Reading Web Newspapers with Gnus."
  :group 'gnus)

(defvar nnshimbun-group-parameters-custom
  '(list :format "%v"
	 (checklist :inline t
		    (list :inline t :format "%v"
			  (const :format "" index-range)
			  (choice :tag "Index range"
				  :value all
				  (const all)
				  (const last)
				  (integer :tag "days")))
		    (list :inline t :format "%v"
			  (const :format "" prefetch-articles)
			  (boolean :tag "Prefetch articles"
				   :on "on"
				   :off "off"))
		    (list :inline t :format "%v"
			  (const :format "" expiry-wait)
			  (choice :tag "Expire wait"
				  :value never
				  (const never)
				  (const immediate)
				  (number :tag "days"))))
	 (repeat :inline t :tag "Others"
		 (list :inline t :format "%v"
		       (symbol :tag "Keyword")
		       (sexp :tag "Value"))))
  "A type definition for customizing the nnshimbun group parameters.")

;; The following definition provides the group parameter
;; `nnshimbun-group-parameters', the user option
;; `nnshimbun-group-parameters-alist' and the function
;; `nnshimbun-find-group-parameters'.
;; The group parameter `nnshimbun-group-parameters' will have a
;; property list like the following:
;;
;; '(index-range all prefetch-articles t expiry-wait 6)

(gnus-define-group-parameter
 nnshimbun-group-parameters
 :type list
 :function nnshimbun-find-group-parameters
 :function-document "\
Return a nnshimbun GROUP's group parameters."
 :variable nnshimbun-group-parameters-alist
 :variable-default nil
 :variable-document "\
Alist of nnshimbun group parameters.  Each element should be a cons of
a group name regexp and a plist which consists of a keyword and a value
pairs like the following:

'(\"^nnshimbun\\\\+asahi:\" index-range all prefetch-articles t expiry-wait 6)

`index-range' specifies a range of header indices as described below:
      all: Retrieve all header indices.
     last: Retrieve the last header index.
integer N: Retrieve N pages of header indices.

`prefetch-articles' specifies whether to pre-fetch the unread articles
when scanning the group.

`expiry-wait' is similar to the generic group parameter `expiry-wait',
but it has a preference."
 :variable-group nnshimbun
 :variable-type `(repeat (cons :format "%v" (regexp :tag "Group name regexp"
						    :value "^nnshimbun\\+")
			       ,nnshimbun-group-parameters-custom))
 :parameter-type nnshimbun-group-parameters-custom
 :parameter-document "\
Group parameters for the nnshimbun group.

`Index range' specifies a range of header indices as described below:
      all: Retrieve all header indices.
     last: Retrieve the last header index.
integer N: Retrieve N pages of header indices.

`Prefetch articles' specifies whether to pre-fetch the unread articles
when scanning the group.

`Expire wait' is similar to the generic group parameter `expiry-wait',
but it has a preference.")

(defcustom nnshimbun-keep-unparsable-dated-articles t
  "*If non-nil, nnshimbun will never delete articles whose NOV date is unparsable."
  :group 'nnshimbun
  :type 'boolean)


;; Define backend
(gnus-declare-backend "nnshimbun" 'address)
(nnoo-declare nnshimbun)

(defvoo nnshimbun-directory (nnheader-concat gnus-directory "shimbun/")
  "Where nnshimbun will save its files.")

(defvoo nnshimbun-nov-is-evil nil
  "*Non-nil means that nnshimbun will never retrieve NOV headers.")

(defvoo nnshimbun-nov-file-name ".overview")

(defvoo nnshimbun-pre-fetch-article nil
  "*Non nil means that nnshimbun fetch unread articles when scanning
groups.  Note that this variable has just a default value for all the
nnshimbun groups.  You can specify the nnshimbun group parameter
`prefecth-articles' for each nnshimbun group.")

(defvoo nnshimbun-index-range nil
  "*Range of indices to detect new pages.  Note that this variable has
just a default value for all the nnshimbun groups.  You can specify
the nnshimbun group parameter `index-range' for each nnshimbun group.")

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
	 (gnus-backlog-buffer (format " *nnshimbun backlog %s*"
				      (nnoo-current-server 'nnshimbun)))
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
		(error (nnheader-report 'nnshimbun "%s" (error-message-string
							 err)))))
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
    (nnheader-report 'nnshimbun "Couldn't create directory: %s"
		     nnshimbun-directory))
   ((not (file-directory-p (file-truename nnshimbun-directory)))
    (nnshimbun-close-server)
    (nnheader-report 'nnshimbun "Not a directory: %s" nnshimbun-directory))
   (t
    (unless (file-exists-p nnshimbun-server-directory)
      (ignore-errors (make-directory nnshimbun-server-directory t)))
    (cond
     ((not (file-exists-p nnshimbun-server-directory))
      (nnshimbun-close-server)
      (nnheader-report 'nnshimbun "Couldn't create directory: %s"
		       nnshimbun-server-directory))
     ((not (file-directory-p (file-truename nnshimbun-server-directory)))
      (nnshimbun-close-server)
      (nnheader-report 'nnshimbun "Not a directory: %s"
		       nnshimbun-server-directory))
     (t
      (nnheader-report 'nnshimbun "Opened server %s using directory %s"
		       server nnshimbun-server-directory)
      t)))))

(deffoo nnshimbun-close-server (&optional server)
  (when (nnshimbun-server-opened server)
    (when nnshimbun-shimbun
      (shimbun-close nnshimbun-shimbun))
    (when (gnus-buffer-live-p nnshimbun-buffer)
      (kill-buffer nnshimbun-buffer)))
  (nnshimbun-backlog (gnus-backlog-shutdown))
  (nnshimbun-save-nov)
  (nnoo-close-server 'nnshimbun server)
  t)

(eval-and-compile
  (let ((Gnus-p
	 (eval-when-compile
	   (let ((gnus (locate-library "gnus")))
	     (and gnus
		  ;; Gnus has mailcap.el in the same directory of gnus.el.
		  (file-exists-p (expand-file-name
				  "mailcap.el"
				  (file-name-directory gnus))))))))
    (if Gnus-p
	(progn
	  (defmacro nnshimbun-mail-header-subject (header)
	    `(mail-header-subject ,header))
	  (defmacro nnshimbun-mail-header-from (header)
	    `(mail-header-from ,header)))
      (defmacro nnshimbun-mail-header-subject (header)
	`(mime-entity-fetch-field ,header 'Subject))
      (defmacro nnshimbun-mail-header-from (header)
	`(mime-entity-fetch-field ,header 'From)))))

(defun nnshimbun-make-shimbun-header (header)
  (shimbun-make-header
   (mail-header-number header)
   (nnshimbun-mail-header-subject header)
   (nnshimbun-mail-header-from header)
   (mail-header-date header)
   (or (cdr (assq 'X-Nnshimbun-Id (mail-header-extra header)))
       (mail-header-id header))
   (mail-header-references header)
   (mail-header-chars header)
   (mail-header-lines header)
   (let ((xref (mail-header-xref header)))
     (if (and xref (string-match "^Xref: " xref))
	 (substring xref 6)
       xref))))

(eval-when-compile
  (require 'gnus-sum));; For the macro `gnus-summary-article-header'.

(defun nnshimbun-request-article-1 (article &optional group server to-buffer)
  (if (nnshimbun-backlog
	(gnus-backlog-request-article
	 group article (or to-buffer nntp-server-buffer)))
      (cons group article)
    (let* ((header (with-current-buffer (nnshimbun-open-nov group)
		     (and (nnheader-find-nov-line article)
			  (nnshimbun-make-shimbun-header
			   (nnheader-parse-nov)))))
	   (original-id (shimbun-header-id header)))
      (when header
	(with-current-buffer (or to-buffer nntp-server-buffer)
	  (delete-region (point-min) (point-max))
	  (shimbun-article nnshimbun-shimbun header)
	  (when (> (buffer-size) 0)
	    ;; Kludge! replace a date string in `gnus-newsgroup-data'
	    ;; based on the newly retrieved article.
	    (let ((x (gnus-summary-article-header article)))
	      (when x
		(mail-header-set-date x (shimbun-header-date header))))
	    (nnshimbun-replace-nov-entry group article header original-id)
	    (nnshimbun-backlog
	      (gnus-backlog-enter-article group article (current-buffer)))
	    (nnheader-report 'nnshimbun "Article %s retrieved"
			     (shimbun-header-id header))
	    (cons group article)))))))

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
      (nnheader-report 'nnshimbun "%s is not a directory"
		       nnshimbun-current-directory))
     (dont-check
      (nnheader-report 'nnshimbun "Group %s selected" group)
      t)
     (t
      (let (beg end lines)
	(with-current-buffer (nnshimbun-open-nov group)
	  (goto-char (point-min))
	  (setq beg (ignore-errors (read (current-buffer))))
	  (goto-char (point-max))
	  (forward-line -1)
	  (setq end (ignore-errors (read (current-buffer)))
		lines (count-lines (point-min) (point-max))))
	(nnheader-report 'nnshimbun "Selected group %s" group)
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
    (let ((nov (expand-file-name nnshimbun-nov-file-name
				 nnshimbun-current-directory)))
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
	     (nth (1- (length articles)) articles))
	    t))))))



;;; Nov Database Operations

(defvar nnshimbun-tmp-string nil
  "Internal variable used to just a rest for a temporary string.  The
macro `nnshimbun-string-or' uses it exclusively.")

(defmacro nnshimbun-string-or (&rest strings)
  "Return the first element of STRINGS that is a non-blank string.  It
should run fast, especially if two strings are given.  Each string can
also be nil."
  (cond ((null strings)
	 nil)
	((= 1 (length strings))
	 ;; Return irregularly nil if one blank string is given.
	 `(unless (zerop (length (setq nnshimbun-tmp-string ,(car strings))))
	    nnshimbun-tmp-string))
	((= 2 (length strings))
	 ;; Return the second string when the first string is blank.
	 `(if (zerop (length (setq nnshimbun-tmp-string ,(car strings))))
	      ,(cadr strings)
	    nnshimbun-tmp-string))
	(t
	 `(let ((strings (list ,@strings)))
	    (while strings
	      (setq strings (if (zerop (length (setq nnshimbun-tmp-string
						     (car strings))))
				(cdr strings))))
	    nnshimbun-tmp-string))))

(defsubst nnshimbun-insert-nov (number header &optional id)
  (insert "\n")
  (backward-char 1)
  (let ((header-id (nnshimbun-string-or (shimbun-header-id header)))
	;; Force `princ' to work in the current buffer.
	(standard-output (current-buffer))
	(xref (nnshimbun-string-or (shimbun-header-xref header)))
	(start (point)))
    (and (stringp id)
	 header-id
	 (string-equal id header-id)
	 (setq id nil))
    (princ number)
    (insert
     "\t"
     (nnshimbun-string-or (shimbun-header-subject header) "(none)") "\t"
     (nnshimbun-string-or (shimbun-header-from header) "(nobody)") "\t"
     (nnshimbun-string-or (shimbun-header-date header) (message-make-date))
     "\t"
     (or header-id (nnmail-message-id)) "\t"
     (or (shimbun-header-references header) "") "\t")
    (princ (or (shimbun-header-chars header) 0))
    (insert "\t")
    (princ (or (shimbun-header-lines header) 0))
    (insert "\t")
    (if xref
	(progn
	  (insert "Xref: " xref "\t")
	  (when id
	    (insert "X-Nnshimbun-Id: " id "\t")))
      (when id
	(insert "\tX-Nnshimbun-Id: " id "\t")))
    ;; Replace newlines with spaces in the current NOV line.
    (while (progn
	     (forward-line 0)
	     (> (point) start))
      (backward-delete-char 1)
      (insert " "))
    (forward-line 1)))

(defmacro nnshimbun-find-parameter (group symbol &optional full-name-p)
  "Return the value of a nnshimbun group parameter for GROUP which is
associated with SYMBOL.  If FULL-NAME-P is non-nil, it treats that
GROUP has a full name."
  (let ((name (if full-name-p
		  group
		`(concat "nnshimbun+" (nnoo-current-server 'nnshimbun)
			 ":" ,group))))
    (cond ((eq 'index-range (eval symbol))
	   `(or (plist-get (nnshimbun-find-group-parameters ,name)
			   'index-range)
		nnshimbun-index-range))
	  ((eq 'prefetch-articles (eval symbol))
	   `(or (plist-get (nnshimbun-find-group-parameters ,name)
			   'prefetch-articles)
		nnshimbun-pre-fetch-article))
	  ((eq 'expiry-wait (eval symbol))
	   (if full-name-p
	       `(or (plist-get (nnshimbun-find-group-parameters ,group)
			       'expiry-wait)
		    (gnus-group-find-parameter ,group 'expiry-wait))
	     `(let ((name ,name))
		(or (plist-get (nnshimbun-find-group-parameters name)
			       'expiry-wait)
		    (gnus-group-find-parameter name 'expiry-wait)))))
	  (t
	   `(plist-get (nnshimbun-find-group-parameters ,name) ,symbol)))))

(defun nnshimbun-generate-nov-database (group)
  (nnshimbun-possibly-change-group group)
  (with-current-buffer (nnshimbun-open-nov group)
    (goto-char (point-max))
    (forward-line -1)
    (let* ((i (or (ignore-errors (read (current-buffer))) 0))
	   (name (concat "nnshimbun+" (nnoo-current-server 'nnshimbun)
			 ":" group))
	   (pre-fetch (nnshimbun-find-parameter name 'prefetch-articles t)))
      (dolist (header
	       (shimbun-headers
		nnshimbun-shimbun
		(nnshimbun-find-parameter name 'index-range t)))
	(unless (nnshimbun-search-id group (shimbun-header-id header))
	  (goto-char (point-max))
	  (nnshimbun-insert-nov (setq i (1+ i)) header)
	  (when pre-fetch
	    (nnshimbun-request-article-1 i group nil nnshimbun-buffer)))))
    (nnshimbun-write-nov group)))

(defun nnshimbun-replace-nov-entry (group article header &optional id)
  (with-current-buffer (nnshimbun-open-nov group)
    (when (nnheader-find-nov-line article)
      (delete-region (point) (progn (forward-line 1) (point)))
      (nnshimbun-insert-nov article header id))))

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
      (when found
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

(deffoo nnshimbun-request-expire-articles (articles group
						    &optional server force)
  "Do expiration for the specified ARTICLES in the nnshimbun GROUP.
Notice that nnshimbun does not actually delete any articles, it just
delete the corresponding entries in the NOV database locally.  The
expiration will be performed only when the current SERVER is specified
and the NOV is open.  The optional fourth argument FORCE is ignored."
  (let ((buffer (cdr (assoc group nnshimbun-nov-buffer-alist))))
    (if (and server
	     ;; Don't use 'string-equal' in the following.
	     (equal server (nnoo-current-server 'nnshimbun))
	     (buffer-live-p buffer))
	(let* ((expirable (copy-sequence articles))
	       (name (concat "nnshimbun+" server ":" group))
	       ;; If the group's parameter `expiry-wait' is non-nil,
	       ;; `nnmail-expiry-wait' is bound to that value, and
	       ;; `nnmail-expiry-wait-function' is bound to nil.
	       ;; See the source code of `gnus-summary-expire-articles'.
	       ;; Prefer the shimbun's default to `nnmail-expiry-wait'
	       ;; only when the group's parameter is nil.
	       (nnmail-expiry-wait
		(or (nnshimbun-find-parameter name 'expiry-wait t)
		    (shimbun-article-expiration-days nnshimbun-shimbun)
		    nnmail-expiry-wait))
	       article end time)
	  (save-excursion
	    (set-buffer buffer)
	    (while expirable
	      (setq article (pop expirable))
	      (when (and (nnheader-find-nov-line article)
			 (setq end (line-end-position))
			 (not (= (point-max) (1+ end))))
		(setq time (and (search-forward "\t" end t)
				(search-forward "\t" end t)
				(search-forward "\t" end t)
				(parse-time-string
				 (buffer-substring
				  (point)
				  (if (search-forward "\t" end t)
				      (1- (point))
				    end)))))
		(when (cond ((setq time (condition-case nil
					    (apply 'encode-time time)
					  (error nil)))
			     (nnmail-expired-article-p name time nil))
			    (t
			     ;; Inhibit expiration if there's no parsable
			     ;; date and the following option is non-nil.
			     (not nnshimbun-keep-unparsable-dated-articles)))
		  (beginning-of-line)
		  (delete-region (point) (1+ end))
		  (setq articles (delq article articles)))))
	    (when (buffer-modified-p)
	      (nnmail-write-region 1 (point-max)
				   nnshimbun-nov-buffer-file-name
				   nil 'nomesg)
	      (set-buffer-modified-p nil))
	    articles))
      t)))



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
	(nnheader-report 'nnshimbun "Couldn't create directory: %s"
			 nnshimbun-current-directory))
       ((not (file-directory-p (file-truename nnshimbun-current-directory)))
	(nnheader-report 'nnshimbun "Not a directory: %s"
			 nnshimbun-current-directory))
       (t t)))))



;;; shimbun-gnus-mua
(luna-define-class shimbun-gnus-mua (shimbun-mua) ())

(luna-define-method shimbun-mua-search-id ((mua shimbun-gnus-mua) id)
  (nnshimbun-search-id
   (shimbun-current-group-internal (shimbun-mua-shimbun-internal mua))
   id))



;;; Command to create nnshimbun group

(defvar nnshimbun-server-history nil)

;;;###autoload
(defun gnus-group-make-shimbun-group ()
  "Create a nnshimbun group."
  (interactive)
  (let* ((minibuffer-setup-hook
	  (append minibuffer-setup-hook '(beginning-of-line)))
	 (alist
	  (apply 'nconc
		 (mapcar
		  (lambda (d)
		    (and (stringp d)
			 (file-directory-p d)
			 (delq nil
			       (mapcar
				(lambda (f)
				  (and (string-match "^sb-\\(.*\\)\\.el$" f)
				       (list (match-string 1 f))))
				(directory-files d)))))
		  load-path)))
	 (server (completing-read
		  "Shimbun address: "
		  alist nil t
		  (or (car nnshimbun-server-history)
		      (caar alist))
		  'nnshimbun-server-history))
	 (groups)
	 (nnshimbun-pre-fetch-article))
    (require (intern (concat "sb-" server)))
    (when (setq groups (intern-soft (concat "shimbun-" server "-groups")))
      (gnus-group-make-group
       (completing-read "Group name: "
			(mapcar 'list (symbol-value groups))
			nil t nil)
       (list 'nnshimbun server)))))


(provide 'nnshimbun)
;;; nnshimbun.el ends here.

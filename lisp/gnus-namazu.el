;;; gnus-namazu.el --- Search mail with Namazu.

;; Copyright (C) 2000,2001 Tsuchiya Masatoshi <tsuchiya@namazu.org>

;; Author: Tsuchiya Masatoshi <tsuchiya@namazu.org>
;; Keywords: mail searching namazu

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

;; This file defines the command to search mails with Namazu and
;; browse its results with Gnus.  This module requires the external
;; command Namazu.  Visit the following page for more information.
;;
;;     http://www.namazu.org/


;;; Install:

;; Make index of articles with Namzu before using this module.
;;
;;	 % mkdir ~/News/namazu
;;       % mknmz -a -h -O ~/News/namazu ~/Mail
;;
;; Furthermore, put these expressions to your ~/.gnus, to set the path
;; of the index files to `gnus-namazu-index-directories'.
;;
;;    (setq gnus-namazu-index-directories
;;          (list (expand-file-name "~/News/namazu")))

;; If you would like to use this module in Gnus (not T-gnus), put this
;; file into the lisp/ directory in the Gnus source tree and run `make
;; install'.  And then, put the following expressions into your ~/.gnus.
;;
;;    (require 'gnus-namazu)
;;    (gnus-namazu-insinuate)


;;; Usage:

;; In group buffer or in summary buffer, type C-c C-n query RET.


;;; Code:

(eval-when-compile (require 'cl))
(require 'nnoo)
(require 'nnheader)
(require 'nnmail)
(require 'gnus-sum)

;; It is required for Mule 2.3.  See the file Mule23@1934.en.
(eval-and-compile
  (autoload 'regexp-opt "regexp-opt"))

;; To suppress byte-compile warning.
(eval-when-compile
  (defvar nnml-directory)
  (defvar nnmh-directory))

(defgroup gnus-namazu nil
  "Search nnmh and nnml groups in Gnus with Namazu."
  :group 'namazu
  :group 'gnus
  :prefix "gnus-namazu-")

(defcustom gnus-namazu-indexed-servers nil
  "*List of servers indexed with Namazu."
  :type '(repeat gnus-select-method)
  :group 'gnus-namazu)

(defcustom gnus-namazu-index-directories
  (list
   (cond
    ((boundp 'nnir-namazu-index-directory)
     (symbol-value 'nnir-namazu-index-directory))
    ((boundp 'gnus-namazu-index-directory)
     (symbol-value 'gnus-namazu-index-directory))
    (t
     (expand-file-name "namazu" gnus-directory))))
  "*Index directory of Namazu."
  :type '(repeat directory)
  :group 'gnus-namazu)

(defcustom gnus-namazu-command
  (cond
   ((boundp 'namazu-command)
    (symbol-value 'namazu-command))
   ((boundp 'nnir-namazu-program)
    (symbol-value 'nnir-namazu-program))
   (t "namazu"))
  "*Name of the executable file of Namazu."
  :group 'gnus-namazu
  :type 'string)

(defcustom gnus-namazu-additional-arguments nil
  "*Additional arguments of Namazu.
The options `-q', `-a', and `-l' are always used, very few other
options make any sense in this context."
  :type '(repeat string)
  :group 'gnus-namazu)

(defcustom gnus-namazu-field-keywords
  '("date" "from" "newsgroups" "size" "subject" "summary" "to" "uri")
  "*List of keywords to do field-search."
  :type '(repeat string)
  :group 'gnus-namazu)

(defcustom gnus-namazu-coding-system
  (if (memq system-type '(windows-nt OS/2 emx))
      (if (>= emacs-major-version 20) 'shift_jis '*sjis*)
    (if (>= emacs-major-version 20) 'euc-japan '*euc-japan*))
  "*Coding system for Namazu process."
  :type 'coding-system
  :group 'gnus-namazu)

(defcustom gnus-namazu-need-path-normalization
  (eq system-type 'windows-nt)
  "*Non-nil means that outputs of namazu may contain a not normalized path."
  :type 'boolean
  :group 'gnus-namazu)


(defmacro gnus-namazu/make-article (group number)
  `(cons ,group ,number))
(defmacro gnus-namazu/article-group  (x) `(car ,x))
(defmacro gnus-namazu/article-number (x) `(cdr ,x))


(defun gnus-namazu/setup ()
  (unless gnus-namazu-indexed-servers
    (setq gnus-namazu-indexed-servers
	  (delq nil
		(mapcar (lambda (method)
			  (when (memq (car method) '(nnml nnmh))
			    method))
			(cons gnus-select-method
			      gnus-secondary-select-methods)))))
  (unless gnus-namazu-indexed-servers
    (error "%s" "Can't find either nnml backend or nnmh backend"))
  (when (or (not gnus-namazu-index-directories)
	    (memq nil (mapcar
		       (lambda (dir)
			 (and (stringp dir)
			      (file-directory-p dir)
			      (file-readable-p
			       (expand-file-name "NMZ.i" dir))
			      dir))
		       gnus-namazu-index-directories)))
    (error "%s" "Can't find index.  Check `gnus-namazu-index-directories'")))

(defun gnus-namazu/server-directory (server)
  "Return the top directory of articles in SERVER."
  (and (memq (car server) '(nnml nnmh))
       (nnoo-change-server (car server) (nth 1 server) (nthcdr 2 server))
       (file-name-as-directory
	(expand-file-name (if (eq 'nnml (car server))
			      nnml-directory
			    nnmh-directory)))))

;;; Functions to call Namazu.
(defsubst gnus-namazu/normalize-results ()
  (goto-char (point-min))
  (while (not (eobp))
    (when (if gnus-namazu-need-path-normalization
	      (or (not (looking-at "/\\(.\\)|/"))
		  (replace-match "\\1:/"))
	    (eq ?~ (char-after (point))))
      (insert (expand-file-name
	       (buffer-substring (gnus-point-at-bol) (gnus-point-at-eol))))
      (delete-region (point) (gnus-point-at-eol)))
    (forward-line 1)))

(defsubst gnus-namazu/call-namazu (query)
  (let ((coding-system-for-read gnus-namazu-coding-system)
	(coding-system-for-write gnus-namazu-coding-system)
	(default-process-coding-system
	  (cons gnus-namazu-coding-system gnus-namazu-coding-system))
	(file-name-coding-system gnus-namazu-coding-system)
	(pathname-coding-system gnus-namazu-coding-system))
    (apply 'call-process
	   `(,gnus-namazu-command
	     nil			; input from /dev/null
	     t				; output
	     nil			; don't redisplay
	     "-q"			; don't be verbose
	     "-a"			; show all matches
	     "-l"			; use list format
	     ,@gnus-namazu-additional-arguments
	     ,query
	     ,@gnus-namazu-index-directories))))

(defun gnus-namazu/search (groups query)
  (with-temp-buffer
    (when (zerop (gnus-namazu/call-namazu query))
      (let* ((articles)
	     (server-alist
	      (delq nil
		    (let (dir)
		      (mapcar
		       (lambda (s)
			 (when (setq dir (gnus-namazu/server-directory s))
			   (cons (file-name-as-directory dir) s)))
		       gnus-namazu-indexed-servers))))
	     (topdir-regexp
	      (regexp-opt (mapcar 'car server-alist) t)))
	(gnus-namazu/normalize-results)
	(goto-char (point-min))
	(while (not (eobp))
	  (let (server group file)
	    (and (looking-at topdir-regexp)
		 (setq server (cdr (assoc (match-string 1) server-alist))
		       file (buffer-substring-no-properties
			     (match-end 0) (point-at-eol)))
		 (string-match "/\\([0-9]+\\)\\'" file)
		 (progn
		   (setq group (substring file 0 (match-beginning 0))
			 file (match-string 1 file))
		   (setq group
			 (gnus-group-prefixed-name
			  (nnheader-replace-chars-in-string group ?/ ?.)
			  server))
		   (when (or (not groups)
			     (member group groups))
		     (push (gnus-namazu/make-article
			    group (string-to-number file))
			   articles)))))
	  (forward-line 1))
	(nreverse articles)))))


(defun gnus-namazu/get-target-groups ()
  (cond
   ((eq major-mode 'gnus-group-mode)
    ;; In Group buffer.
    (cond
     (current-prefix-arg
      (gnus-group-process-prefix current-prefix-arg))
     (gnus-group-marked
      (prog1 gnus-group-marked (gnus-group-unmark-all-groups)))))
   ((eq major-mode 'gnus-summary-mode)
    ;; In Summary buffer.
    (if current-prefix-arg
	(list (gnus-read-group "Group: "))
      (if (and (gnus-ephemeral-group-p gnus-newsgroup-name)
	       (string-match "\\`nnvirtual:namazu-search" gnus-newsgroup-name))
	  (cadr (assq 'gnus-namazu-target-groups
		      (gnus-info-method (gnus-get-info gnus-newsgroup-name))))
	(list gnus-newsgroup-name))))))

(defun gnus-namazu/get-current-query ()
  (and (eq major-mode 'gnus-summary-mode)
       (gnus-ephemeral-group-p gnus-newsgroup-name)
       (string-match "\\`nnvirtual:namazu-search" gnus-newsgroup-name)
       (cadr (assq 'gnus-namazu-current-query
		   (gnus-info-method (gnus-get-info gnus-newsgroup-name))))))

(defvar gnus-namazu/read-query-original-buffer nil)
(defvar gnus-namazu/read-query-prompt nil)
(defvar gnus-namazu/read-query-history nil)

(defun gnus-namazu/get-current-subject ()
  (and gnus-namazu/read-query-original-buffer
       (bufferp gnus-namazu/read-query-original-buffer)
       (with-current-buffer gnus-namazu/read-query-original-buffer
	 (when (eq major-mode 'gnus-summary-mode)
	   (let ((s (gnus-summary-article-subject)))
	     ;; Remove typically prefixes of mailing lists.
	     (when (string-match
		    "^\\(\\[[^]]*[0-9]+\\]\\|([^)]*[0-9]+)\\)\\s-*" s)
	       (setq s (substring s (match-end 0))))
	     (when (string-match
		    "^\\(Re\\(\\^?\\([0-9]+\\|\\[[0-9]+\\]\\)\\)?:\\s-*\\)+" s)
	       (setq s (substring s (match-end 0))))
	     (when (string-match "\\s-*(\\(re\\|was\\)\\b" s)
	       (setq s (substring s 0 (match-beginning 0))))
	     s)))))

(defun gnus-namazu/get-current-from ()
  (and gnus-namazu/read-query-original-buffer
       (bufferp gnus-namazu/read-query-original-buffer)
       (with-current-buffer gnus-namazu/read-query-original-buffer
	 (when (eq major-mode 'gnus-summary-mode)
	   (cadr (mail-extract-address-components
		  (mail-header-from
		   (gnus-summary-article-header))))))))

(defmacro gnus-namazu/minibuffer-prompt-end ()
  (if (fboundp 'minibuffer-prompt-end)
      '(minibuffer-prompt-end)
    '(point-min)))

(defun gnus-namazu/message (string &rest arguments)
  (let* ((s1 (concat
	      gnus-namazu/read-query-prompt
	      (buffer-substring (gnus-namazu/minibuffer-prompt-end)
				(point-max))))
	 (s2 (apply (function format) string arguments))
	 (w (- (window-width)
	       (string-width s1)
	       (string-width s2)
	       1)))
    (message (if (>= w 0)
		 (concat s1 (make-string w ?\ ) s2)
	       s2))
    (if (sit-for 0.3) (message s1))
    s2))

(defun gnus-namazu/complete-query ()
  (interactive)
  (let ((pos (point)))
    (cond
     ((and (re-search-backward "\\+\\([a-z]*\\)" nil t)
	   (= pos (match-end 0)))
      (let* ((partial (match-string 1))
	     (completions
	      (all-completions
	       partial
	       (mapcar 'list gnus-namazu-field-keywords))))
	(cond
	 ((null completions)
	  (gnus-namazu/message "No completions of %s" partial))
	 ((= 1 (length completions))
	  (goto-char (match-beginning 1))
	  (delete-region (match-beginning 1) (match-end 1))
	  (insert (car completions) ":")
	  (setq pos (point))
	  (gnus-namazu/message "Completed"))
	 (t
	  (let ((x (try-completion partial (mapcar 'list completions))))
	    (if (string= x partial)
		(if (and (eq last-command
			     'gnus-namazu/field-keyword-completion)
			 completion-auto-help)
		    (with-output-to-temp-buffer "*Completions*"
		      (display-completion-list completions))
		  (gnus-namazu/message "Sole completion"))
	      (goto-char (match-beginning 1))
	      (delete-region (match-beginning 1) (match-end 1))
	      (insert x)
	      (setq pos (point))))))))
     ((and (looking-at "\\+subject:")
	   (= pos (match-end 0)))
      (let ((s (gnus-namazu/get-current-subject)))
	(when s
	  (goto-char pos)
	  (insert "\"" s "\"")
	  (setq pos (point)))))
     ((and (looking-at "\\+from:")
	   (= pos (match-end 0)))
      (let ((f (gnus-namazu/get-current-from)))
	(when f
	  (goto-char pos)
	  (insert "\"" f "\"")
	  (setq pos (point))))))
    (goto-char pos)))

(defvar gnus-namazu/read-query-map
  (let ((keymap (copy-keymap minibuffer-local-map)))
    (define-key keymap "\t" 'gnus-namazu/complete-query)
    keymap))

(defun gnus-namazu/read-query (prompt &optional initial)
  (let ((gnus-namazu/read-query-original-buffer (current-buffer))
	(gnus-namazu/read-query-prompt prompt))
    (unless initial
      (when (setq initial (gnus-namazu/get-current-query))
	(setq initial (cons initial 0))))
    (read-from-minibuffer prompt initial gnus-namazu/read-query-map nil
			  'gnus-namazu/read-query-history)))

(defun gnus-namazu/truncate-article-list (articles)
  (let ((hit (length articles)))
    (when (> hit gnus-large-newsgroup)
      (let* ((cursor-in-echo-area nil)
	     (input
	      (when (> hit gnus-large-newsgroup)
		(read-from-minibuffer
		 (format
		  "Too many articles were retrieved.  How many articles (max %d): "
		  hit)
		 (cons (number-to-string gnus-large-newsgroup) 0)))))
	(unless (string-match "\\`[ \t]*\\'" input)
	  (setcdr (nthcdr (min (1- (string-to-number input)) hit) articles)
		  nil))))
    articles))

(defun gnus-namazu-search (groups query)
  "Search QUERY through GROUPS with Namazu,
and make a virtual group contains its results."
  (interactive
   (list
    (gnus-namazu/get-target-groups)
    (gnus-namazu/read-query "Enter Keyword: ")))
  (gnus-namazu/setup)
  (let ((articles (gnus-namazu/search groups query)))
    (if articles
	(let ((real-groups groups)
	      (vgroup
	       (apply 'format
		      "nnvirtual:namazu-search?query=%s&groups=%s&id=%d%d%d"
		      query
		      (if groups (mapconcat 'identity groups ",") "ALL")
		      (current-time))))
	  (gnus-namazu/truncate-article-list articles)
	  (unless real-groups
	    (dolist (a articles)
	      (add-to-list 'real-groups (gnus-namazu/article-group a))))
	  ;; Generate virtual group which includes all results.
	  (setq vgroup
		(gnus-group-read-ephemeral-group
		 vgroup
		 `(nnvirtual ,vgroup
			     (nnvirtual-component-groups ,real-groups)
			     (gnus-namazu-target-groups ,groups)
			     (gnus-namazu-current-query ,query))
		 t (cons (current-buffer) (current-window-configuration)) t))
	  ;; Generate new summary buffer which contains search results.
	  (gnus-group-read-group
	   t t vgroup
	   (sort (delq nil ;; Ad-hoc fix, to avoid wrong-type-argument error.
		       (mapcar
			(lambda (a)
			  (nnvirtual-reverse-map-article
			   (gnus-namazu/article-group a)
			   (gnus-namazu/article-number a)))
			articles))
		 '<)))
      (message "No entry."))))


(defun gnus-namazu-insinuate ()
  (add-hook
   'gnus-group-mode-hook
   (lambda ()
     (define-key gnus-group-mode-map "\C-c\C-n" 'gnus-namazu-search)))
  (add-hook
   'gnus-summary-mode-hook
   (lambda ()
     (define-key gnus-summary-mode-map "\C-c\C-n" 'gnus-namazu-search))))


(provide 'gnus-namazu)
;; gnus-namazu.el ends here.

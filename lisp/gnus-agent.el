;;; gnus-agent.el --- unplugged support for Semi-gnus
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	Tatsuya Ichikawa <t-ichi@po.shiojiri.ne.jp>
;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'gnus-clfns))

(require 'gnus)
(require 'gnus-cache)
(require 'nnvirtual)
(require 'gnus-sum)
(require 'gnus-score)
(require 'gnus-srvr)
(eval-when-compile
  (if (featurep 'xemacs)
      (require 'itimer)
    (require 'timer))
  (require 'gnus-group))

(eval-and-compile
  (autoload 'gnus-server-update-server "gnus-srvr")
  (autoload 'number-at-point "thingatpt"))

(defface gnus-agent-downloaded-article-face
  '((((class color)
      (background light))
     (:foreground "darkslategray" :bold nil))
    (((class color) (background dark))
     (:foreground "LightGray" :bold nil))
    (t (:inverse-video t :bold nil)))
  "Face used for displaying downloaded articles"
  :group 'gnus-agent)

(defcustom gnus-agent-directory (nnheader-concat gnus-directory "agent/")
  "Where the Gnus agent will store its files."
  :group 'gnus-agent
  :type 'directory)

(defcustom gnus-agent-plugged-hook nil
  "Hook run when plugging into the network."
  :group 'gnus-agent
  :type 'hook)

(defcustom gnus-agent-unplugged-hook nil
  "Hook run when unplugging from the network."
  :group 'gnus-agent
  :type 'hook)

(defcustom gnus-agent-handle-level gnus-level-subscribed
  "Groups on levels higher than this variable will be ignored by the Agent."
  :group 'gnus-agent
  :type 'integer)

(defcustom gnus-agent-expire-days 7
  "Read articles older than this will be expired.
This can also be a list of regexp/day pairs.  The regexps will
be matched against group names."
  :group 'gnus-agent
  :type 'integer)

(defcustom gnus-agent-expire-all nil
  "If non-nil, also expire unread, ticked and dormant articles.
If nil, only read articles will be expired."
  :group 'gnus-agent
  :type 'boolean)

(defcustom gnus-agent-group-mode-hook nil
  "Hook run in Agent group minor modes."
  :group 'gnus-agent
  :type 'hook)

;; Extracted from gnus-xmas-redefine in order to preserve user settings
(when (featurep 'xemacs)
  (add-hook 'gnus-agent-group-mode-hook 'gnus-xmas-agent-group-menu-add))

(defcustom gnus-agent-summary-mode-hook nil
  "Hook run in Agent summary minor modes."
  :group 'gnus-agent
  :type 'hook)

;; Extracted from gnus-xmas-redefine in order to preserve user settings
(when (featurep 'xemacs)
  (add-hook 'gnus-agent-summary-mode-hook 'gnus-xmas-agent-summary-menu-add))

(defcustom gnus-agent-server-mode-hook nil
  "Hook run in Agent summary minor modes."
  :group 'gnus-agent
  :type 'hook)

;; Extracted from gnus-xmas-redefine in order to preserve user settings
(when (featurep 'xemacs)
  (add-hook 'gnus-agent-server-mode-hook 'gnus-xmas-agent-server-menu-add))

(defcustom gnus-agent-confirmation-function 'y-or-n-p
  "Function to confirm when error happens."
  :version "21.1"
  :group 'gnus-agent
  :type 'function)

(defcustom gnus-agent-large-newsgroup nil
  "*The number of articles which indicates a large newsgroup.
If the number of unread articles exceeds it, The number of articles to be
fetched will be limited to it. If not a positive integer, never consider it."
  :group 'gnus-agent
  :type '(choice (const nil)
		 (integer :tag "Number")))

(defcustom gnus-agent-synchronize-flags 'ask
  "Indicate if flags are synchronized when you plug in.
If this is `ask' the hook will query the user."
  :version "21.1"
  :type '(choice (const :tag "Always" t)
		 (const :tag "Never" nil)
		 (const :tag "Ask" ask))
  :group 'gnus-agent)

(defcustom gnus-agent-go-online 'ask
  "Indicate if offline servers go online when you plug in.
If this is `ask' the hook will query the user."
  :version "21.1"
  :type '(choice (const :tag "Always" t)
		 (const :tag "Never" nil)
		 (const :tag "Ask" ask))
  :group 'gnus-agent)

(defcustom gnus-agent-mark-unread-after-downloaded t
  "Indicate whether to mark articles unread after downloaded."
  :version "21.1"
  :type 'boolean
  :group 'gnus-agent)

(defcustom gnus-agent-download-marks '(download)
  "Marks for downloading."
  :version "21.1"
  :type '(repeat (symbol :tag "Mark"))
  :group 'gnus-agent)

(defcustom gnus-agent-consider-all-articles nil
  "If non-nil, consider also the read articles for downloading."
  :version "21.4"
  :type 'boolean
  :group 'gnus-agent)

;;; Internal variables

(defvar gnus-agent-history-buffers nil)
(defvar gnus-agent-buffer-alist nil)
(defvar gnus-agent-article-alist nil
"An assoc list identifying the articles whose headers have been fetched.  
 If successfully fetched, these headers will be stored in the group's overview file.
 The key of each assoc pair is the article ID.
 The value of each assoc pair is a flag indicating 
 whether the identified article has been downloaded (gnus-agent-fetch-articles
 sets the value to the day of the download).
 NOTES:
 1) The last element of this list can not be expired as some 
    routines (for example, get-agent-fetch-headers) use the last
    value to track which articles have had their headers retrieved.
 2) The gnus-agent-regenerate may destructively modify the value.
")
(defvar gnus-agent-group-alist nil)
(defvar gnus-category-alist nil)
(defvar gnus-agent-current-history nil)
(defvar gnus-agent-overview-buffer nil)
(defvar gnus-category-predicate-cache nil)
(defvar gnus-category-group-cache nil)
(defvar gnus-agent-spam-hashtb nil)
(defvar gnus-agent-file-name nil)
(defvar gnus-agent-send-mail-function nil)
(defvar gnus-agent-file-coding-system 'raw-text)
(defvar gnus-agent-file-loading-cache nil)
(defvar gnus-agent-file-header-cache nil)

(defvar gnus-agent-auto-agentize-methods '(nntp nnimap)
  "Initially, all servers from these methods are agentized.
The user may remove or add servers using the Server buffer.  See Info
node `(gnus)Server Buffer'.")

;; Dynamic variables
(defvar gnus-headers)
(defvar gnus-score)

;;;
;;; Setup
;;;

(defun gnus-open-agent ()
  (setq gnus-agent t)
  (gnus-agent-read-servers)
  (gnus-category-read)
  (gnus-agent-create-buffer)
  (add-hook 'gnus-group-mode-hook 'gnus-agent-mode)
  (add-hook 'gnus-summary-mode-hook 'gnus-agent-mode)
  (add-hook 'gnus-server-mode-hook 'gnus-agent-mode))

(defun gnus-agent-create-buffer ()
  (if (gnus-buffer-live-p gnus-agent-overview-buffer)
      t
    (setq gnus-agent-overview-buffer
	  (gnus-get-buffer-create " *Gnus agent overview*"))
    (with-current-buffer gnus-agent-overview-buffer
      (set-buffer-multibyte t))
    nil))

(gnus-add-shutdown 'gnus-close-agent 'gnus)

(defun gnus-close-agent ()
  (setq gnus-agent-covered-methods nil
	gnus-category-predicate-cache nil
	gnus-category-group-cache nil
	gnus-agent-spam-hashtb nil)
  (gnus-kill-buffer gnus-agent-overview-buffer))

;;;
;;; Utility functions
;;;

(defun gnus-agent-read-file (file)
  "Load FILE and do a `read' there."
  (with-temp-buffer
    (ignore-errors
      (nnheader-insert-file-contents file)
      (goto-char (point-min))
      (read (current-buffer)))))

(defsubst gnus-agent-method ()
  (concat (symbol-name (car gnus-command-method)) "/"
	  (if (equal (cadr gnus-command-method) "")
	      "unnamed"
	    (cadr gnus-command-method))))

(defsubst gnus-agent-directory ()
  "Path of the Gnus agent directory."
  (nnheader-concat gnus-agent-directory
		   (nnheader-translate-file-chars (gnus-agent-method)) "/"))

(defun gnus-agent-lib-file (file)
  "The full path of the Gnus agent library FILE."
  (expand-file-name file
		    (file-name-as-directory
		     (expand-file-name "agent.lib" (gnus-agent-directory)))))

;;; Fetching setup functions.

(defun gnus-agent-start-fetch ()
  "Initialize data structures for efficient fetching."
  (gnus-agent-create-buffer))

(defun gnus-agent-stop-fetch ()
  "Save all data structures and clean up."
  (setq gnus-agent-spam-hashtb nil)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (widen)))

(defmacro gnus-agent-with-fetch (&rest forms)
  "Do FORMS safely."
  `(unwind-protect
       (let ((gnus-agent-fetching t))
	 (gnus-agent-start-fetch)
	 ,@forms)
     (gnus-agent-stop-fetch)))

(put 'gnus-agent-with-fetch 'lisp-indent-function 0)
(put 'gnus-agent-with-fetch 'edebug-form-spec '(body))

;;;
;;; Mode infestation
;;;

(defvar gnus-agent-mode-hook nil
  "Hook run when installing agent mode.")

(defvar gnus-agent-mode nil)
(defvar gnus-agent-mode-status '(gnus-agent-mode " Plugged"))

(defun gnus-agent-mode ()
  "Minor mode for providing a agent support in Gnus buffers."
  (let* ((buffer (progn (string-match "^gnus-\\(.*\\)-mode$"
				      (symbol-name major-mode))
			(match-string 1 (symbol-name major-mode))))
	 (mode (intern (format "gnus-agent-%s-mode" buffer))))
    (set (make-local-variable 'gnus-agent-mode) t)
    (set mode nil)
    (set (make-local-variable mode) t)
    ;; Set up the menu.
    (when (gnus-visual-p 'agent-menu 'menu)
      (funcall (intern (format "gnus-agent-%s-make-menu-bar" buffer))))
    (unless (assq 'gnus-agent-mode minor-mode-alist)
      (push gnus-agent-mode-status minor-mode-alist))
    (unless (assq mode minor-mode-map-alist)
      (push (cons mode (symbol-value (intern (format "gnus-agent-%s-mode-map"
						     buffer))))
	    minor-mode-map-alist))
    (when (eq major-mode 'gnus-group-mode)
      (gnus-agent-toggle-plugged gnus-plugged))
    (gnus-run-hooks 'gnus-agent-mode-hook
		    (intern (format "gnus-agent-%s-mode-hook" buffer)))))

(defvar gnus-agent-group-mode-map (make-sparse-keymap))
(gnus-define-keys gnus-agent-group-mode-map
  "Ju" gnus-agent-fetch-groups
  "Jc" gnus-enter-category-buffer
  "Jj" gnus-agent-toggle-plugged
  "Js" gnus-agent-fetch-session
  "JY" gnus-agent-synchronize-flags
  "JS" gnus-group-send-queue
  "Ja" gnus-agent-add-group
  "Jr" gnus-agent-remove-group
  "Jo" gnus-agent-toggle-group-plugged)

(defun gnus-agent-group-make-menu-bar ()
  (unless (boundp 'gnus-agent-group-menu)
    (easy-menu-define
     gnus-agent-group-menu gnus-agent-group-mode-map ""
     '("Agent"
       ["Toggle plugged" gnus-agent-toggle-plugged t]
       ["Toggle group plugged" gnus-agent-toggle-group-plugged t]
       ["List categories" gnus-enter-category-buffer t]
       ["Send queue" gnus-group-send-queue gnus-plugged]
       ("Fetch"
	["All" gnus-agent-fetch-session gnus-plugged]
	["Group" gnus-agent-fetch-group gnus-plugged])))))

(defvar gnus-agent-summary-mode-map (make-sparse-keymap))
(gnus-define-keys gnus-agent-summary-mode-map
  "Jj" gnus-agent-toggle-plugged
  "Ju" gnus-agent-summary-fetch-group
  "Js" gnus-agent-summary-fetch-series
  "J#" gnus-agent-mark-article
  "J\M-#" gnus-agent-unmark-article
  "@" gnus-agent-toggle-mark
  "Jc" gnus-agent-catchup)

(defun gnus-agent-summary-make-menu-bar ()
  (unless (boundp 'gnus-agent-summary-menu)
    (easy-menu-define
     gnus-agent-summary-menu gnus-agent-summary-mode-map ""
     '("Agent"
       ["Toggle plugged" gnus-agent-toggle-plugged t]
       ["Mark as downloadable" gnus-agent-mark-article t]
       ["Unmark as downloadable" gnus-agent-unmark-article t]
       ["Toggle mark" gnus-agent-toggle-mark t]
       ["Fetch downloadable" gnus-agent-summary-fetch-group t]
       ["Catchup undownloaded" gnus-agent-catchup t]))))

(defvar gnus-agent-server-mode-map (make-sparse-keymap))
(gnus-define-keys gnus-agent-server-mode-map
  "Jj" gnus-agent-toggle-plugged
  "Ja" gnus-agent-add-server
  "Jr" gnus-agent-remove-server)

(defun gnus-agent-server-make-menu-bar ()
  (unless (boundp 'gnus-agent-server-menu)
    (easy-menu-define
     gnus-agent-server-menu gnus-agent-server-mode-map ""
     '("Agent"
       ["Toggle plugged" gnus-agent-toggle-plugged t]
       ["Add" gnus-agent-add-server t]
       ["Remove" gnus-agent-remove-server t]))))

(defun gnus-agent-make-mode-line-string (string mouse-button mouse-func)
  (if (and (fboundp 'propertize)
	   (fboundp 'make-mode-line-mouse-map))
      (propertize string 'local-map
		  (make-mode-line-mouse-map mouse-button mouse-func))
    string))

(defun gnus-agent-toggle-plugged (plugged)
  "Toggle whether Gnus is unplugged or not."
  (interactive (list (not gnus-plugged)))
  (if plugged
      (progn
	(setq gnus-plugged plugged)
	(gnus-run-hooks 'gnus-agent-plugged-hook)
	(setcar (cdr gnus-agent-mode-status)
		(gnus-agent-make-mode-line-string " Plugged"
						  'mouse-2
						  'gnus-agent-toggle-plugged))
	(gnus-agent-go-online gnus-agent-go-online)
	(gnus-agent-possibly-synchronize-flags))
    (gnus-agent-close-connections)
    (setq gnus-plugged plugged)
    (gnus-run-hooks 'gnus-agent-unplugged-hook)
    (setcar (cdr gnus-agent-mode-status)
	    (gnus-agent-make-mode-line-string " Unplugged"
					      'mouse-2
					      'gnus-agent-toggle-plugged)))
  (force-mode-line-update)
  (set-buffer-modified-p t))

(defun gnus-agent-close-connections ()
  "Close all methods covered by the Gnus agent."
  (let ((methods gnus-agent-covered-methods))
    (while methods
      (gnus-close-server (pop methods)))))

;;;###autoload
(defun gnus-unplugged ()
  "Start Gnus unplugged."
  (interactive)
  (setq gnus-plugged nil)
  (gnus))

;;;###autoload
(defun gnus-plugged ()
  "Start Gnus plugged."
  (interactive)
  (setq gnus-plugged t)
  (gnus))

;;;###autoload
(defun gnus-slave-unplugged (&optional arg)
  "Read news as a slave unplugged."
  (interactive "P")
  (setq gnus-plugged nil)
  (gnus arg nil 'slave))

;;;###autoload
(defun gnus-agentize ()
  "Allow Gnus to be an offline newsreader.
The normal usage of this command is to put the following as the
last form in your `.gnus.el' file:

\(gnus-agentize)

This will modify the `gnus-setup-news-hook', and
`message-send-mail-real-function' variables, and install the Gnus agent
minor mode in all Gnus buffers."
  (interactive)
  (gnus-open-agent)
  (add-hook 'gnus-setup-news-hook 'gnus-agent-queue-setup)
  (unless gnus-agent-send-mail-function
    (setq gnus-agent-send-mail-function (or
					 message-send-mail-real-function
					 message-send-mail-function)
	  message-send-mail-real-function 'gnus-agent-send-mail))
  (unless gnus-agent-covered-methods
    (mapcar
     (lambda (server)
       (if (memq (car (gnus-server-to-method server)) 
		 gnus-agent-auto-agentize-methods)
	   (setq gnus-agent-covered-methods 
		 (cons (gnus-server-to-method server)
		       gnus-agent-covered-methods ))))
     (append (list gnus-select-method) gnus-secondary-select-methods))))

(defun gnus-agent-queue-setup ()
  "Make sure the queue group exists."
  (unless (gnus-gethash "nndraft:queue" gnus-newsrc-hashtb)
    (gnus-request-create-group "queue" '(nndraft ""))
    (let ((gnus-level-default-subscribed 1))
      (gnus-subscribe-group "nndraft:queue" nil '(nndraft "")))
    (gnus-group-set-parameter
     "nndraft:queue" 'gnus-dummy '((gnus-draft-mode)))))

(defun gnus-agent-send-mail ()
  (if gnus-plugged
      (funcall gnus-agent-send-mail-function)
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "\n"))
    (replace-match "\n")
    (gnus-agent-insert-meta-information 'mail)
    (gnus-request-accept-article "nndraft:queue" nil t t)))

(defun gnus-agent-insert-meta-information (type &optional method)
  "Insert meta-information into the message that says how it's to be posted.
TYPE can be either `mail' or `news'.  If the latter, then METHOD can
be a select method."
  (save-excursion
    (message-remove-header gnus-agent-meta-information-header)
    (goto-char (point-min))
    (insert gnus-agent-meta-information-header ": "
	    (symbol-name type) " " (format "%S" method)
	    "\n")
    (forward-char -1)
    (while (search-backward "\n" nil t)
      (replace-match "\\n" t t))))

(defun gnus-agent-restore-gcc ()
  "Restore GCC field from saved header."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (concat gnus-agent-gcc-header ":") nil t)
      (replace-match "Gcc:" 'fixedcase))))

(defun gnus-agent-any-covered-gcc ()
  (save-restriction
    (message-narrow-to-headers)
    (let* ((gcc (mail-fetch-field "gcc" nil t))
	   (methods (and gcc
			 (mapcar 'gnus-inews-group-method
				 (message-unquote-tokens
				  (message-tokenize-header
				   gcc " ,")))))
	   covered)
      (while (and (not covered) methods)
	(setq covered (gnus-agent-method-p (car methods))
	      methods (cdr methods)))
      covered)))

;;;###autoload
(defun gnus-agent-possibly-save-gcc ()
  "Save GCC if Gnus is unplugged."
  (when (and (not gnus-plugged) (gnus-agent-any-covered-gcc))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
	(while (re-search-forward "^gcc:" nil t)
	  (replace-match (concat gnus-agent-gcc-header ":") 'fixedcase))))))

(defun gnus-agent-possibly-do-gcc ()
  "Do GCC if Gnus is plugged."
  (when (or gnus-plugged (not (gnus-agent-any-covered-gcc)))
    (gnus-inews-do-gcc)))

;;;
;;; Group mode commands
;;;

(defun gnus-agent-fetch-groups (n)
  "Put all new articles in the current groups into the Agent."
  (interactive "P")
  (unless gnus-plugged
    (error "Groups can't be fetched when Gnus is unplugged"))
  (gnus-group-iterate n 'gnus-agent-fetch-group))

(defun gnus-agent-fetch-group (group)
  "Put all new articles in GROUP into the Agent."
  (interactive (list (gnus-group-group-name)))
  (let ((state gnus-plugged))
    (unwind-protect
	(progn
	  (unless group
	    (error "No group on the current line"))
	  (unless state
	    (gnus-agent-toggle-plugged gnus-plugged))
	  (let ((gnus-command-method (gnus-find-method-for-group group)))
	    (gnus-agent-with-fetch
	      (gnus-agent-fetch-group-1 group gnus-command-method)
	      (gnus-message 5 "Fetching %s...done" group))))
      (when (and (not state)
		 gnus-plugged)
	(gnus-agent-toggle-plugged gnus-plugged)))))

(defun gnus-agent-add-group (category arg)
  "Add the current group to an agent category."
  (interactive
   (list
    (intern
     (completing-read
      "Add to category: "
      (mapcar (lambda (cat) (list (symbol-name (car cat))))
	      gnus-category-alist)
      nil t))
    current-prefix-arg))
  (let ((cat (assq category gnus-category-alist))
	c groups)
    (gnus-group-iterate arg
      (lambda (group)
	(when (cadddr (setq c (gnus-group-category group)))
	  (setf (cadddr c) (delete group (cadddr c))))
	(push group groups)))
    (setf (cadddr cat) (nconc (cadddr cat) groups))
    (gnus-category-write)))

(defun gnus-agent-remove-group (arg)
  "Remove the current group from its agent category, if any."
  (interactive "P")
  (let (c)
    (gnus-group-iterate arg
      (lambda (group)
	(when (cadddr (setq c (gnus-group-category group)))
	  (setf (cadddr c) (delete group (cadddr c))))))
    (gnus-category-write)))

(defun gnus-agent-synchronize-flags ()
  "Synchronize unplugged flags with servers."
  (interactive)
  (save-excursion
    (dolist (gnus-command-method gnus-agent-covered-methods)
      (when (file-exists-p (gnus-agent-lib-file "flags"))
	(gnus-agent-synchronize-flags-server gnus-command-method)))))

(defun gnus-agent-possibly-synchronize-flags ()
  "Synchronize flags according to `gnus-agent-synchronize-flags'."
  (interactive)
  (save-excursion
    (dolist (gnus-command-method gnus-agent-covered-methods)
      (when (file-exists-p (gnus-agent-lib-file "flags"))
	(gnus-agent-possibly-synchronize-flags-server gnus-command-method)))))

(defun gnus-agent-synchronize-flags-server (method)
  "Synchronize flags set when unplugged for server."
  (let ((gnus-command-method method))
    (when (file-exists-p (gnus-agent-lib-file "flags"))
      (set-buffer (get-buffer-create " *Gnus Agent flag synchronize*"))
      (erase-buffer)
      (nnheader-insert-file-contents (gnus-agent-lib-file "flags"))
      (if (null (gnus-check-server gnus-command-method))
	  (gnus-message 1 "Couldn't open server %s" (nth 1 gnus-command-method))
	(while (not (eobp))
	  (if (null (eval (read (current-buffer))))
	      (progn (forward-line)
		     (kill-line -1))
	    (write-file (gnus-agent-lib-file "flags"))
	    (error "Couldn't set flags from file %s"
		   (gnus-agent-lib-file "flags"))))
	(delete-file (gnus-agent-lib-file "flags")))
      (kill-buffer nil))))

(defun gnus-agent-possibly-synchronize-flags-server (method)
  "Synchronize flags for server according to `gnus-agent-synchronize-flags'."
  (when (or (and gnus-agent-synchronize-flags
		 (not (eq gnus-agent-synchronize-flags 'ask)))
	    (and (eq gnus-agent-synchronize-flags 'ask)
		 (gnus-y-or-n-p (format "Synchronize flags on server `%s'? "
					(cadr method)))))
    (gnus-agent-synchronize-flags-server method)))

;;;
;;; Server mode commands
;;;

(defun gnus-agent-add-server (server)
  "Enroll SERVER in the agent program."
  (interactive (list (gnus-server-server-name)))
  (unless server
    (error "No server on the current line"))
  (let ((method (gnus-server-get-method nil (gnus-server-server-name))))
    (when (gnus-agent-method-p method)
      (error "Server already in the agent program"))
    (push method gnus-agent-covered-methods)
    (gnus-server-update-server server)
    (gnus-agent-write-servers)
    (gnus-message 1 "Entered %s into the Agent" server)))

(defun gnus-agent-remove-server (server)
  "Remove SERVER from the agent program."
  (interactive (list (gnus-server-server-name)))
  (unless server
    (error "No server on the current line"))
  (let ((method (gnus-server-get-method nil (gnus-server-server-name))))
    (unless (gnus-agent-method-p method)
      (error "Server not in the agent program"))
    (setq gnus-agent-covered-methods
	  (delete method gnus-agent-covered-methods))
    (gnus-server-update-server server)
    (gnus-agent-write-servers)
    (gnus-message 1 "Removed %s from the agent" server)))

(defun gnus-agent-read-servers ()
  "Read the alist of covered servers."
  (mapcar (lambda (m)
	    (let ((method (gnus-server-get-method
			   nil
			   (or m "native"))))
	      (if method
                  (unless (member method gnus-agent-covered-methods)
                    (push method gnus-agent-covered-methods))
		(gnus-message 1 "Ignoring disappeared server `%s'" m)
		(sit-for 1))))
	  (gnus-agent-read-file
	   (nnheader-concat gnus-agent-directory "lib/methods"))))

(defun gnus-agent-write-servers ()
  "Write the alist of covered servers."
  (gnus-make-directory (nnheader-concat gnus-agent-directory "lib"))
  (let ((coding-system-for-write nnheader-file-coding-system)
	(output-coding-system nnheader-file-coding-system)
	(file-name-coding-system nnmail-pathname-coding-system)
	(pathname-coding-system nnmail-pathname-coding-system))
    (with-temp-file (nnheader-concat gnus-agent-directory "lib/servers")
      (prin1 (mapcar 'gnus-method-simplify gnus-agent-covered-methods)
	     (current-buffer)))))

;;;
;;; Summary commands
;;;

(defun gnus-agent-mark-article (n &optional unmark)
  "Mark the next N articles as downloadable.
If N is negative, mark backward instead.  If UNMARK is non-nil, remove
the mark instead.  The difference between N and the actual number of
articles marked is returned."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and
	    (> n 0)
	    (progn
	      (gnus-summary-set-agent-mark
	       (gnus-summary-article-number) unmark)
	      (zerop (gnus-summary-next-subject (if backward -1 1) nil t))))
      (setq n (1- n)))
    (when (/= 0 n)
      (gnus-message 7 "No more articles"))
    (gnus-summary-recenter)
    (gnus-summary-position-point)
    n))

(defun gnus-agent-unmark-article (n)
  "Remove the downloadable mark from the next N articles.
If N is negative, unmark backward instead.  The difference between N and
the actual number of articles unmarked is returned."
  (interactive "p")
  (gnus-agent-mark-article n t))

(defun gnus-agent-toggle-mark (n)
  "Toggle the downloadable mark from the next N articles.
If N is negative, toggle backward instead.  The difference between N and
the actual number of articles toggled is returned."
  (interactive "p")
  (gnus-agent-mark-article n 'toggle))

(defun gnus-summary-set-agent-mark (article &optional unmark)
  "Mark ARTICLE as downloadable."
  (let ((unmark (if (and (not (null unmark)) (not (eq t unmark)))
		    (memq article gnus-newsgroup-downloadable)
		  unmark))
        (new-mark gnus-downloadable-mark))
    (if unmark
	(let ((agent-articles gnus-agent-article-alist))
	  (setq gnus-newsgroup-downloadable
		(delq article gnus-newsgroup-downloadable))
          (while (and agent-articles (< (caar agent-articles) article))
            (setq agent-articles (cdr agent-articles)))
          (if (and (eq (caar agent-articles) article)
                   (cdar agent-articles))
              (setq new-mark 32)
            (progn (setq new-mark gnus-undownloaded-mark)
                   (push article gnus-newsgroup-undownloaded))))
      (setq gnus-newsgroup-undownloaded
	    (delq article gnus-newsgroup-undownloaded))
      (setq gnus-newsgroup-downloadable
	    (gnus-add-to-sorted-list gnus-newsgroup-downloadable article)))
    (gnus-summary-update-mark
     new-mark
     'unread)))

;; Check history - this may make sense if the agent is configured to pre-fetch every article.
(defun gnus-agent-get-undownloaded-list ()
  "Mark all unfetched articles as read."
  (let ((gnus-command-method (gnus-find-method-for-group gnus-newsgroup-name)))
    (when (and 
           (not (gnus-online gnus-command-method))
           (gnus-agent-method-p gnus-command-method))
      (gnus-agent-load-alist gnus-newsgroup-name)
      ;; First mark all undownloaded articles as undownloaded.
      ;; CCC kaig: Maybe change here to consider all headers.
      (let ((articles (delq nil (mapcar (lambda (header) (if (equal (mail-header-from header) "Gnus Agent")
                                                             nil
                                                           (mail-header-number header)))
                                        gnus-newsgroup-headers)))
	    (agent-articles gnus-agent-article-alist)
	    candidates article)
	(while (setq article (pop articles))
	  (while (and agent-articles
		      (< (caar agent-articles) article))
	    (setq agent-articles (cdr agent-articles)))
	  (when (or (not (cdar agent-articles))
		    (not (= (caar agent-articles) article)))
	    (push article candidates)))
	(dolist (article candidates)
	  (unless (or (memq article gnus-newsgroup-downloadable)
		      (memq article gnus-newsgroup-cached))
	    (push article gnus-newsgroup-undownloaded))))
      ;; Then mark downloaded downloadable as not-downloadable,
      ;; if you get my drift.
      (dolist (article gnus-newsgroup-downloadable)
	(when (cdr (assq article gnus-agent-article-alist))
	  (setq gnus-newsgroup-downloadable
		(delq article gnus-newsgroup-downloadable)))))))

(defun gnus-agent-catchup ()
  "Mark all undownloaded articles as read."
  (interactive)
  (save-excursion
    (while gnus-newsgroup-undownloaded
      (gnus-summary-mark-article
       (pop gnus-newsgroup-undownloaded) gnus-catchup-mark)))
  (gnus-summary-position-point))

(defun gnus-agent-summary-fetch-series ()
  (interactive)
  (let ((dl gnus-newsgroup-downloadable))
    (while gnus-newsgroup-processable
      (let* ((art (car (last gnus-newsgroup-processable)))
             (gnus-newsgroup-downloadable (list art)))
        (gnus-summary-goto-subject art)
        (sit-for 0)
        (gnus-agent-summary-fetch-group)
        (setq dl (delq art dl))
        (gnus-summary-remove-process-mark art)
        (sit-for 0)))
    (setq gnus-newsgroup-downloadable dl)))

(defun gnus-agent-summary-fetch-group (&optional all)
  "Fetch the downloadable articles in the group.
Optional arg ALL, if non-nil, means to fetch all articles."
  (interactive "P")
  (let ((articles
	 (if all gnus-newsgroup-articles
	   gnus-newsgroup-downloadable))
	(gnus-command-method (gnus-find-method-for-group gnus-newsgroup-name))
	(state gnus-plugged))
    (unwind-protect
	(progn
	  (unless state
	    (gnus-agent-toggle-plugged t))
	  (unless articles
	    (error "No articles to download"))
	  (gnus-agent-with-fetch
	    (gnus-agent-fetch-articles gnus-newsgroup-name articles))
	  (save-excursion
	    (dolist (article articles)
	      (setq gnus-newsgroup-downloadable
		    (delq article gnus-newsgroup-downloadable))
	      (if gnus-agent-mark-unread-after-downloaded
		  (gnus-summary-mark-article article gnus-unread-mark)))))
      (when (and (not state)
		 gnus-plugged)
	(gnus-agent-toggle-plugged nil)))))

(defun gnus-agent-fetch-selected-article ()
  "Fetch the current article as it is selected.
This can be added to `gnus-select-article-hook' or
`gnus-mark-article-hook'."
  (let ((gnus-command-method gnus-current-select-method))
    (when (and gnus-plugged (gnus-agent-method-p gnus-command-method))
      (gnus-agent-fetch-articles
       gnus-newsgroup-name
       (list gnus-current-article)))))

;;;
;;; Internal functions
;;;

(defun gnus-agent-save-active (method)
  (gnus-agent-save-active-1 method 'gnus-active-to-gnus-format))

(defun gnus-agent-save-active-1 (method function)
  (when (gnus-agent-method-p method)
    (let* ((gnus-command-method method)
	   (new (gnus-make-hashtable (count-lines (point-min) (point-max))))
	   (file (gnus-agent-lib-file "active")))
      (funcall function nil new)
      (gnus-agent-write-active file new)
      (erase-buffer)
      (nnheader-insert-file-contents file))))

(defun gnus-agent-write-active (file new)
  (let ((orig (gnus-make-hashtable (count-lines (point-min) (point-max))))
	(file (gnus-agent-lib-file "active"))
	elem osym)
    (when (file-exists-p file)
      (with-temp-buffer
	(nnheader-insert-file-contents file)
	(gnus-active-to-gnus-format nil orig))
      (mapatoms
       (lambda (sym)
	 (when (and sym (boundp sym))
	   (if (and (boundp (setq osym (intern (symbol-name sym) orig)))
		    (setq elem (symbol-value osym)))
	       (progn
		 (if (and (integerp (car (symbol-value sym)))
			  (> (car elem) (car (symbol-value sym))))
		     (setcar elem (car (symbol-value sym))))
		 (if (integerp (cdr (symbol-value sym)))
		     (setcdr elem (cdr (symbol-value sym)))))
	     (set (intern (symbol-name sym) orig) (symbol-value sym)))))
       new))
    (gnus-make-directory (file-name-directory file))
    (let ((nnmail-active-file-coding-system gnus-agent-file-coding-system))
      ;; The hashtable contains real names of groups,  no more prefix
      ;; removing, so set `full' to `t'.
      (gnus-write-active-file file orig t))))

(defun gnus-agent-save-groups (method)
  (gnus-agent-save-active-1 method 'gnus-groups-to-gnus-format))

(defun gnus-agent-save-group-info (method group active)
  (when (gnus-agent-method-p method)
    (let* ((gnus-command-method method)
	   (coding-system-for-write nnheader-file-coding-system)
	   (output-coding-system nnheader-file-coding-system)
	   (file-name-coding-system nnmail-pathname-coding-system)
	   (pathname-coding-system nnmail-pathname-coding-system)
	   (file (gnus-agent-lib-file "active"))
	   oactive-min)
      (gnus-make-directory (file-name-directory file))
      (with-temp-file file
	;; Emacs got problem to match non-ASCII group in multibyte buffer.
	(set-buffer-multibyte nil)
	(when (file-exists-p file)
	  (nnheader-insert-file-contents file))
	(goto-char (point-min))
	(when (re-search-forward
	       (concat "^" (regexp-quote group) " ") nil t)
	  (save-excursion
	    (read (current-buffer))			 ;; max
	    (setq oactive-min (read (current-buffer))))  ;; min
	  (gnus-delete-line))
	(insert (format "%S %d %d y\n" (intern group)
			(cdr active)
			(or oactive-min (car active))))
	(goto-char (point-max))
	(while (search-backward "\\." nil t)
	  (delete-char 1))))))

(defun gnus-agent-group-path (group)
  "Translate GROUP into a path."
  (if nnmail-use-long-file-names
      (gnus-group-real-name group)
    (nnheader-translate-file-chars
     (nnheader-replace-chars-in-string
      (nnheader-replace-duplicate-chars-in-string
       (nnheader-replace-chars-in-string
	(gnus-group-real-name group)
	?/ ?_)
       ?. ?_)
      ?. ?/))))



(defun gnus-agent-get-function (method)
  (if (gnus-online method)
      (car method)
    (require 'nnagent)
    'nnagent))

;;; History functions

(defun gnus-agent-history-buffer ()
  (cdr (assoc (gnus-agent-method) gnus-agent-history-buffers)))

(defun gnus-agent-open-history ()
  (save-excursion
    (push (cons (gnus-agent-method)
		(set-buffer (gnus-get-buffer-create
			     (format " *Gnus agent %s history*"
				     (gnus-agent-method)))))
	  gnus-agent-history-buffers)
    (set-buffer-multibyte nil) ;; everything is binary
    (erase-buffer)
    (insert "\n")
    (let ((file (gnus-agent-lib-file "history")))
      (when (file-exists-p file)
	(nnheader-insert-file-contents file))
      (set (make-local-variable 'gnus-agent-file-name) file))))

(defun gnus-agent-close-history ()
  (when (gnus-buffer-live-p gnus-agent-current-history)
    (kill-buffer gnus-agent-current-history)
    (setq gnus-agent-history-buffers
	  (delq (assoc (gnus-agent-method) gnus-agent-history-buffers)
		gnus-agent-history-buffers))))

;;;
;;; Fetching
;;;

(defun gnus-agent-fetch-articles (group articles)
  "Fetch ARTICLES from GROUP and put them into the Agent."
  (gnus-agent-load-alist group)
  (when articles
    ;; Prune off articles that we have already fetched.
    (while (and articles
		(cdr (assq (car articles) gnus-agent-article-alist)))
      (pop articles))
    (let ((arts articles))
      (while (cdr arts)
	(if (cdr (assq (cadr arts) gnus-agent-article-alist))
	    (setcdr arts (cddr arts))
	  (setq arts (cdr arts)))))
    (when articles
      (let ((dir (concat
		  (gnus-agent-directory)
		  (gnus-agent-group-path group) "/"))
	    (date (time-to-days (current-time)))
	    (case-fold-search t)
	    pos crosses id elem)
	(gnus-make-directory dir)
	(gnus-message 7 "Fetching articles for %s..." group)
	;; Fetch the articles from the backend.
	(if (gnus-check-backend-function 'retrieve-articles group)
	    (setq pos (gnus-retrieve-articles articles group))
	  (with-temp-buffer
	    (let (article)
	      (while (setq article (pop articles))
		(gnus-message 10 "Fetching article %s for %s..."
			      article group)
		(when (or
		       (gnus-backlog-request-article group article
						     nntp-server-buffer)
		       (gnus-request-article article group))
		  (goto-char (point-max))
		  (push (cons article (point)) pos)
		  (insert-buffer-substring nntp-server-buffer)))
	      (copy-to-buffer nntp-server-buffer (point-min) (point-max))
	      (setq pos (nreverse pos)))))
	;; Then save these articles into the Agent.
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (while pos
	    (narrow-to-region (cdar pos) (or (cdadr pos) (point-max)))
	    (goto-char (point-min))
	    (unless (eobp)  ;; Don't save empty articles.
	      (when (search-forward "\n\n" nil t)
		(when (search-backward "\nXrefs: " nil t)
		  ;; Handle cross posting.
                  (goto-char (match-end 0)) ; move to end of header name
		  (skip-chars-forward "^ ") ; skip server name
		  (skip-chars-forward " ")
		  (setq crosses nil)
		  (while (looking-at "\\([^: \n]+\\):\\([0-9]+\\) *")
		    (push (cons (buffer-substring (match-beginning 1)
						  (match-end 1))
				(string-to-int (buffer-substring (match-beginning 2)
                                                                 (match-end 2))))
			  crosses)
		    (goto-char (match-end 0)))
		  (gnus-agent-crosspost crosses (caar pos) date)))
	      (goto-char (point-min))
	      (if (not (re-search-forward
			"^Message-ID: *<\\([^>\n]+\\)>" nil t))
		  (setq id "No-Message-ID-in-article")
		(setq id (buffer-substring (match-beginning 1) (match-end 1))))
	      (write-region-as-coding-system
	       gnus-agent-file-coding-system (point-min) (point-max)
	       (concat dir (number-to-string (caar pos))) nil 'silent)
	      (when (setq elem (assq (caar pos) gnus-agent-article-alist))
		(setcdr elem date)))
	    (widen)
	    (pop pos)))
	(gnus-agent-save-alist group)))))

(defun gnus-agent-crosspost (crosses article &optional date)
  (setq date (or date t))

  (let (gnus-agent-article-alist group alist beg end)
    (save-excursion
      (set-buffer gnus-agent-overview-buffer)
      (when (nnheader-find-nov-line article)
	(forward-word 1)
	(setq beg (point))
	(setq end (progn (forward-line 1) (point)))))
    (while crosses
      (setq group (caar crosses))
      (unless (setq alist (assoc group gnus-agent-group-alist))
	(push (setq alist (list group (gnus-agent-load-alist (caar crosses))))
	      gnus-agent-group-alist))
      (setcdr alist (cons (cons (cdar crosses) date) (cdr alist)))
      (save-excursion
	(set-buffer (gnus-get-buffer-create (format " *Gnus agent overview %s*"
						    group)))
	(when (= (point-max) (point-min))
	  (push (cons group (current-buffer)) gnus-agent-buffer-alist)
	  (ignore-errors
	    (nnheader-insert-file-contents
	     (gnus-agent-article-name ".overview" group))))
	(nnheader-find-nov-line (string-to-number (cdar crosses)))
	(insert (string-to-number (cdar crosses)))
	(insert-buffer-substring gnus-agent-overview-buffer beg end)
        (gnus-agent-check-overview-buffer))
      (pop crosses))))

(defun gnus-agent-check-overview-buffer (&optional buffer)
  "Check the overview file given for sanity.
In particular, checks that the file is sorted by article number
and that there are no duplicates."
  (let (prev-num)
    (save-excursion
      (when buffer (set-buffer buffer))
      (save-excursion
        (save-restriction
          (let ((deactivate-mark (if (boundp 'deactivate-mark)
				     (symbol-value 'deactivate-mark)
				   nil)))
            (widen)
            (goto-char (point-min))
            (setq prev-num (number-at-point))
            (while (and (zerop (forward-line 1))
                        (not (eobp)))
              (let ((cur (number-at-point)))
                (cond
		 ((= cur prev-num)
		  (gnus-message 10 "Duplicate overview line for %d" cur)
		  (delete-region (point) (progn (forward-line 1) (point))))
		 ((< cur prev-num)
		  (gnus-message 10 "Overview buffer not sorted!"))))
              (setq prev-num (number-at-point)))))))))


(defun gnus-agent-flush-cache ()
  (save-excursion
    (while gnus-agent-buffer-alist
      (set-buffer (cdar gnus-agent-buffer-alist))
      (write-region-as-coding-system
       gnus-agent-file-coding-system
       (point-min) (point-max)
       (gnus-agent-article-name ".overview"
				(caar gnus-agent-buffer-alist))
       nil 'silent)
      (pop gnus-agent-buffer-alist))
    (while gnus-agent-group-alist
      (with-temp-file (gnus-agent-article-name ".agentview" (caar gnus-agent-group-alist))
	(princ (cdar gnus-agent-group-alist))
	(insert "\n")
        (princ 1 (current-buffer))
	(insert "\n"))
      (pop gnus-agent-group-alist))))

(defun gnus-agent-fetch-headers (group &optional force)
  (let* ((fetch-all (and gnus-agent-consider-all-articles
                         ;; Do not fetch all headers if the predicate
                         ;; implies that we only consider unread articles.
                         (not (gnus-predicate-implies-unread
                               (or (gnus-group-find-parameter
                                    group 'agent-predicate t)
                                   (cadr (gnus-group-category group)))))))
         (articles (if fetch-all
                       (gnus-uncompress-range (gnus-active group))
                     (gnus-list-of-unread-articles group)))
         (gnus-decode-encoded-word-function 'identity)
         (file (gnus-agent-article-name ".overview" group))
         gnus-agent-cache)
    ;; Check whether the number of articles is not too large.
    (when (and (integerp gnus-agent-large-newsgroup)
	       (> gnus-agent-large-newsgroup 0))
      (setq articles (nthcdr (max (- (length articles)
				     gnus-agent-large-newsgroup)
				  0)
			     articles)))
    (unless fetch-all
      ;; Add articles with marks to the list of article headers we want to
      ;; fetch.  Don't fetch articles solely on the basis of a recent or seen
      ;; mark, but do fetch recent or seen articles if they have other, more
      ;; interesting marks.  (We have to fetch articles with boring marks
      ;; because otherwise the agent will remove their marks.)
      (dolist (arts (gnus-info-marks (gnus-get-info group)))
        (unless (memq (car arts) '(seen recent))
          (setq articles (gnus-range-add articles (cdr arts)))))
      (setq articles (sort (gnus-uncompress-sequence articles) '<)))

    ;; At this point, I have the list of articles to consider for fetching.  
    ;; This is the list that I'll return to my caller. Some of these articles may have already 
    ;; been fetched.  That's OK as the fetch article code will filter those out.
    ;; Internally, I'll filter this list to just those articles whose headers need to be fetched.
    (let ((articles articles))
      ;; Remove known articles.
      (when (gnus-agent-load-alist group)
        ;; Remove articles marked as downloaded.
        (if fetch-all
            ;; I want to fetch all headers in the active range.  
            ;; Therefore, exclude only those headers that are in the article alist.
            ;; NOTE: This is probably NOT what I want to do after agent expiration in this group.
            (setq articles (gnus-agent-uncached-articles articles group))

          ;; I want to only fetch those headers that have never been fetched. 
          ;; Therefore, exclude all headers that are, or WERE, in the article alist.
          (let ((low (1+ (caar (last gnus-agent-article-alist))))
                (high (cdr (gnus-active group))))
            ;; Low can be greater than High when the same group is fetched twice
            ;; in the same session {The first fetch will fill the article alist 
            ;; such that (last gnus-agent-article-alist) equals (cdr (gnus-active group))}.  
            ;; The addition of one(the 1+ above) then forces Low to be greater than High.  
            ;; When this happens, gnus-list-range-intersection returns nil which indicates 
            ;; that no headers need to be fetched. -- Kevin
            (setq articles (gnus-list-range-intersection
                            articles (list (cons low high)))))))
      (when articles
        (gnus-message 7 "Fetching headers for %s..." group)

        ;; Fetch them.
        (gnus-make-directory (nnheader-translate-file-chars
                              (file-name-directory file) t))

        (save-excursion
          (set-buffer nntp-server-buffer)
          (unless (eq 'nov (gnus-retrieve-headers articles group))
            (nnvirtual-convert-headers))
          (gnus-agent-check-overview-buffer)
          ;; Move these headers to the overview buffer so that gnus-agent-brand-nov can merge them
          ;; with the contents of FILE.
          (copy-to-buffer gnus-agent-overview-buffer (point-min) (point-max))
          (when (file-exists-p file)
            (gnus-agent-braid-nov group articles file))
          (gnus-agent-check-overview-buffer)
	  (write-region-as-coding-system
	   gnus-agent-file-coding-system
	   (1+ (point-min)) (point-max) file nil 'silent)
          (gnus-agent-save-alist group articles nil)
          articles)))
    articles))

(defsubst gnus-agent-copy-nov-line (article)
  (let (art b e)
    (set-buffer gnus-agent-overview-buffer)
    (while (and (not (eobp))
		(< (setq art (read (current-buffer))) article))
      (forward-line 1))
    (beginning-of-line)
    (if (or (eobp)
	    (not (eq article art)))
	(set-buffer nntp-server-buffer)
      (setq b (point))
      (setq e (progn (forward-line 1) (point)))
      (set-buffer nntp-server-buffer)
      (insert-buffer-substring gnus-agent-overview-buffer b e))))

(defun gnus-agent-braid-nov (group articles file)
  "Merges the article headers identified by ARTICLES from gnus-agent-overview-buffer with the contents
of FILE placing the combined headers in nntp-server-buffer."
  (let (start last)
    (set-buffer gnus-agent-overview-buffer)
    (goto-char (point-min))
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (nnheader-insert-file-contents file)
    (goto-char (point-max))
    (forward-line -1)
    (unless (looking-at "[0-9]+\t")
      ;; Remove corrupted lines
      (gnus-message 1 "Overview %s is corrupted. Removing corrupted lines..." file)
      (goto-char (point-min))
      (while (not (eobp))
	(if (looking-at "[0-9]+\t")
	    (forward-line 1)
	  (delete-region (point) (progn (forward-line 1) (point)))))
      (forward-line -1))
    (unless (or (= (point-min) (point-max))
		(< (setq last (read (current-buffer))) (car articles)))
      ;; We do it the hard way.
      (when (nnheader-find-nov-line (car articles))
        ;; Replacing existing NOV entry
        (delete-region (point) (progn (forward-line 1) (point))))
      (gnus-agent-copy-nov-line (pop articles))

      (ignore-errors
        (while articles
          (while (let ((art (read (current-buffer))))
                   (cond ((< art (car articles))
                          (forward-line 1)
                          t)
                         ((= art (car articles))
                          (beginning-of-line)
                          (delete-region (point) (progn (forward-line 1) (point)))
                          nil)
                         (t
                          (beginning-of-line)
                          nil))))
            
	  (gnus-agent-copy-nov-line (pop articles)))))

    ;; Copy the rest lines
    (set-buffer nntp-server-buffer)
    (goto-char (point-max))
    (when articles
      (when last
	(set-buffer gnus-agent-overview-buffer)
	(ignore-errors
          (while (<= (read (current-buffer)) last)
            (forward-line 1)))
	(beginning-of-line)
	(setq start (point))
	(set-buffer nntp-server-buffer))
      (insert-buffer-substring gnus-agent-overview-buffer start))))

(eval-when-compile ; Keeps the compiler from warning about the free variable in gnus-agent-read-agentview
  (defvar gnus-agent-read-agentview))

(defun gnus-agent-load-alist (group)
  (let ((gnus-agent-read-agentview group)) ; Binds free variable that's used in gnus-agent-read-agentview
    "Load the article-state alist for GROUP."
    (setq gnus-agent-article-alist
          (gnus-cache-file-contents
           (gnus-agent-article-name ".agentview" group)
           'gnus-agent-file-loading-cache
           'gnus-agent-read-agentview))))

;; Save format may be either 1 or 2.  Two is the new, compressed format that is still being tested.  Format 1 is uncompressed but known to be reliable.
(defconst gnus-agent-article-alist-save-format 2)

(defun gnus-agent-read-agentview (file)
  "Load FILE and do a `read' there."
  (with-temp-buffer
    (ignore-errors
      (nnheader-insert-file-contents file)
      (goto-char (point-min))
      (let ((alist (read (current-buffer)))
            (version (condition-case nil (read (current-buffer))
                       (end-of-file 0)))
            changed-version)

        (cond ((= version 0)
               (let ((inhibit-quit t)
                     entry)
                 (gnus-agent-open-history)
                 (set-buffer (gnus-agent-history-buffer))
                 (goto-char (point-min))
                 (while (not (eobp))
                   (if (and (looking-at
                             "[^\t\n]+\t\\([0-9]+\\)\t\\([^ \n]+\\) \\([0-9]+\\)")
                            (string= (match-string 2)
                                     gnus-agent-read-agentview)
                            (setq entry (assoc (string-to-number (match-string 3)) alist)))
                       (setcdr entry (string-to-number (match-string 1))))
                   (forward-line 1))
                 (gnus-agent-close-history)
                 (setq changed-version t)))
              ((= version 1)
               (setq changed-version (not (= 1 gnus-agent-article-alist-save-format))))
              ((= version 2)
               (let (uncomp)
                 (mapcar (lambda (comp-list)
                           (let ((state (car comp-list))
                                 (sequence (gnus-uncompress-sequence (cdr comp-list))))
                             (mapcar (lambda (article-id)
                                       (setq uncomp (cons (cons article-id state) uncomp))) sequence))) alist)
                 (setq alist (sort uncomp (lambda (first second) (< (car first) (car second)))))
                 )
               ))
        (when changed-version
          (let ((gnus-agent-article-alist alist))
            (gnus-agent-save-alist gnus-agent-read-agentview)))
        alist))))

(defun gnus-agent-save-alist (group &optional articles state dir)
  "Save the article-state alist for GROUP."
  (let* ((file-name-coding-system nnmail-pathname-coding-system)
	 (pathname-coding-system nnmail-pathname-coding-system)
	 (prev (cons nil gnus-agent-article-alist))
	 (all prev)
	 print-level print-length item article)
    (while (setq article (pop articles))
      (while (and (cdr prev)
                  (< (caadr prev) article))
	(setq prev (cdr prev)))
      (cond
       ((not (cdr prev))
	(setcdr prev (list (cons article state))))
       ((> (caadr prev) article)
	(setcdr prev (cons (cons article state) (cdr prev))))
       ((= (caadr prev) article)
	(setcdr (cadr prev) state)))
      (setq prev (cdr prev)))
    (setq gnus-agent-article-alist (cdr all))
    (with-temp-file (if dir
			(expand-file-name ".agentview" dir)
		      (gnus-agent-article-name ".agentview" group))
      (cond ((eq gnus-agent-article-alist-save-format 1)
             (princ gnus-agent-article-alist (current-buffer)))
            ((eq gnus-agent-article-alist-save-format 2)
             (let ((compressed nil))
               (mapcar (lambda (pair)
                         (let* ((article-id (car pair))
                                (day-of-download (cdr pair))
                                (comp-list (assq day-of-download compressed)))
                           (if comp-list
                               (setcdr comp-list (cons article-id (cdr comp-list)))
                             (setq compressed (cons (list day-of-download article-id) compressed)))
                           nil)) gnus-agent-article-alist)
               (mapcar (lambda (comp-list) (setcdr comp-list (gnus-compress-sequence (nreverse (cdr comp-list))))) compressed)
               (princ compressed (current-buffer))
               )
             )
            )
      (insert "\n")
      (princ gnus-agent-article-alist-save-format (current-buffer))
      (insert "\n"))))

(defun gnus-agent-article-name (article group)
  (expand-file-name (if (stringp article) article (string-to-number article))
		    (file-name-as-directory
		     (expand-file-name (gnus-agent-group-path group)
				       (gnus-agent-directory)))))

(defun gnus-agent-batch-confirmation (msg)
  "Show error message and return t."
  (gnus-message 1 msg)
  t)

;;;###autoload
(defun gnus-agent-batch-fetch ()
  "Start Gnus and fetch session."
  (interactive)
  (gnus)
  (let ((gnus-agent-confirmation-function 'gnus-agent-batch-confirmation))
    (gnus-agent-fetch-session))
  (gnus-group-exit))

(defun gnus-agent-fetch-session ()
  "Fetch all articles and headers that are eligible for fetching."
  (interactive)
  (unless gnus-agent-covered-methods
    (error "No servers are covered by the Gnus agent"))
  (unless gnus-plugged
    (error "Can't fetch articles while Gnus is unplugged"))
  (let ((methods gnus-agent-covered-methods)
	groups group gnus-command-method)
    (save-excursion
      (while methods
	(condition-case err
	    (progn
	      (setq gnus-command-method (car methods))
	      (when (and (or (gnus-server-opened gnus-command-method)
			     (gnus-open-server gnus-command-method))
			 (gnus-online gnus-command-method))
		(setq groups (gnus-groups-from-server (car methods)))
		(gnus-agent-with-fetch
		  (while (setq group (pop groups))
		    (when (<= (gnus-group-level group) gnus-agent-handle-level)
		      (gnus-agent-fetch-group-1 group gnus-command-method))))))
	  (error
           (unless (funcall gnus-agent-confirmation-function
			    (format "Error %s.  Continue? " (cdr err)))
             (error "Cannot fetch articles into the Gnus agent")))
	  (quit
	   (unless (funcall gnus-agent-confirmation-function
			    (format "Quit fetching session %s.  Continue? "
				    (cdr err)))
	     (signal 'quit "Cannot fetch articles into the Gnus agent"))))
	(pop methods))
      (run-hooks 'gnus-agent-fetch-hook)
      (gnus-message 6 "Finished fetching articles into the Gnus agent"))))

(defun gnus-agent-fetch-group-1 (group method)
  "Fetch GROUP."
  (let ((gnus-command-method method)
	(gnus-newsgroup-name group)
	gnus-newsgroup-dependencies gnus-newsgroup-headers
	gnus-newsgroup-scored gnus-headers gnus-score
	gnus-use-cache articles arts
	category predicate info marks score-param
	(gnus-summary-expunge-below gnus-summary-expunge-below)
	(gnus-summary-mark-below gnus-summary-mark-below)
	(gnus-orphan-score gnus-orphan-score)
	;; Maybe some other gnus-summary local variables should also
	;; be put here.
	)
    (unless (gnus-check-group group)
      (error "Can't open server for %s" group))
    ;; Fetch headers.
    (when (and (or (gnus-active group)
		   (gnus-activate-group group))
	       (setq articles (gnus-agent-fetch-headers group))
	       (let ((nntp-server-buffer gnus-agent-overview-buffer))
		 ;; Parse them and see which articles we want to fetch.
		 (setq gnus-newsgroup-dependencies
		       (make-vector (length articles) 0))
		 (setq gnus-newsgroup-headers
		       (gnus-get-newsgroup-headers-xover articles nil nil
							 group))
		 ;; Some articles may not exist, so update `articles'
		 ;; from what was actually found.  -- kai
		 (setq articles
		       (mapcar (lambda (x) (mail-header-number x))
			       gnus-newsgroup-headers))
		 ;; `gnus-agent-overview-buffer' may be killed for
		 ;; timeout reason.  If so, recreate it.
		 (gnus-agent-create-buffer)))
      (setq category (gnus-group-category group))
      (setq predicate
	    (gnus-get-predicate
	     (or (gnus-group-find-parameter group 'agent-predicate t)
		 (cadr category))))
      (if (memq predicate '(gnus-agent-true gnus-agent-false))
	  ;; Simple implementation
	  (setq arts (and (eq predicate 'gnus-agent-true) articles))
	(setq arts nil)
	(setq score-param
	      (or (gnus-group-get-parameter group 'agent-score t)
		  (caddr category)))
	;; Translate score-param into real one
	(cond
	 ((not score-param))
	 ((eq score-param 'file)
	  (setq score-param (gnus-all-score-files group)))
	 ((stringp (car score-param)))
	 (t
	  (setq score-param (list (list score-param)))))
	(when score-param
	  (gnus-score-headers score-param))
      
        ;; Construct arts list with same order as gnus-newsgroup-headers
        (let* ((a (list nil)) 
               (b a))
          (while (setq gnus-headers (pop gnus-newsgroup-headers))
            (setq gnus-score
                  (or (cdr (assq (mail-header-number gnus-headers)
                                 gnus-newsgroup-scored))
                      gnus-summary-default-score))
            (when (funcall predicate)
              (setq a (setcdr a (list (mail-header-number gnus-headers))))))
          (setq arts (cdr b))))

      ;; Fetch the articles.
      (when arts
	(gnus-agent-fetch-articles group arts)))
    ;; Perhaps we have some additional articles to fetch.
    (dolist (mark gnus-agent-download-marks)
      (setq arts (assq mark (gnus-info-marks
			     (setq info (gnus-get-info group)))))
      (when (cdr arts)
	(gnus-message 8 "Agent is downloading marked articles...")
	(gnus-agent-fetch-articles
	 group (gnus-uncompress-range (cdr arts)))
	(when (eq mark 'download)
	  (setq marks (delq arts (gnus-info-marks info)))
	  (gnus-info-set-marks info marks)
	  (gnus-dribble-enter
	   (concat "(gnus-group-set-info '"
		   (gnus-prin1-to-string info)
		   ")")))))))

;;;
;;; Agent Category Mode
;;;

(defvar gnus-category-mode-hook nil
  "Hook run in `gnus-category-mode' buffers.")

(defvar gnus-category-line-format "     %(%20c%): %g\n"
  "Format of category lines.

Valid specifiers include:
%c  Topic name (string)
%g  The number of groups in the topic (integer)

General format specifiers can also be used.  See Info node
`(gnus)Formatting Variables'.")

(defvar gnus-category-mode-line-format "Gnus: %%b"
  "The format specification for the category mode line.")

(defvar gnus-agent-short-article 100
  "Articles that have fewer lines than this are short.")

(defvar gnus-agent-long-article 200
  "Articles that have more lines than this are long.")

(defvar gnus-agent-low-score 0
  "Articles that have a score lower than this have a low score.")

(defvar gnus-agent-high-score 0
  "Articles that have a score higher than this have a high score.")


;;; Internal variables.

(defvar gnus-category-buffer "*Agent Category*")

(defvar gnus-category-line-format-alist
  `((?c gnus-tmp-name ?s)
    (?g gnus-tmp-groups ?d)))

(defvar gnus-category-mode-line-format-alist
  `((?u user-defined ?s)))

(defvar gnus-category-line-format-spec nil)
(defvar gnus-category-mode-line-format-spec nil)

(defvar gnus-category-mode-map nil)
(put 'gnus-category-mode 'mode-class 'special)

(unless gnus-category-mode-map
  (setq gnus-category-mode-map (make-sparse-keymap))
  (suppress-keymap gnus-category-mode-map)

  (gnus-define-keys gnus-category-mode-map
    "q" gnus-category-exit
    "k" gnus-category-kill
    "c" gnus-category-copy
    "a" gnus-category-add
    "p" gnus-category-edit-predicate
    "g" gnus-category-edit-groups
    "s" gnus-category-edit-score
    "l" gnus-category-list

    "\C-c\C-i" gnus-info-find-node
    "\C-c\C-b" gnus-bug))

(defvar gnus-category-menu-hook nil
  "*Hook run after the creation of the menu.")

(defun gnus-category-make-menu-bar ()
  (gnus-turn-off-edit-menu 'category)
  (unless (boundp 'gnus-category-menu)
    (easy-menu-define
     gnus-category-menu gnus-category-mode-map ""
     '("Categories"
       ["Add" gnus-category-add t]
       ["Kill" gnus-category-kill t]
       ["Copy" gnus-category-copy t]
       ["Edit predicate" gnus-category-edit-predicate t]
       ["Edit score" gnus-category-edit-score t]
       ["Edit groups" gnus-category-edit-groups t]
       ["Exit" gnus-category-exit t]))

    (gnus-run-hooks 'gnus-category-menu-hook)))

(defun gnus-category-mode ()
  "Major mode for listing and editing agent categories.

All normal editing commands are switched off.
\\<gnus-category-mode-map>
For more in-depth information on this mode, read the manual
\(`\\[gnus-info-find-node]').

The following commands are available:

\\{gnus-category-mode-map}"
  (interactive)
  (when (gnus-visual-p 'category-menu 'menu)
    (gnus-category-make-menu-bar))
  (kill-all-local-variables)
  (gnus-simplify-mode-line)
  (setq major-mode 'gnus-category-mode)
  (setq mode-name "Category")
  (gnus-set-default-directory)
  (setq mode-line-process nil)
  (use-local-map gnus-category-mode-map)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (gnus-run-hooks 'gnus-category-mode-hook))

(defalias 'gnus-category-position-point 'gnus-goto-colon)

(defun gnus-category-insert-line (category)
  (let* ((gnus-tmp-name (format "%s" (car category)))
	 (gnus-tmp-groups (length (cadddr category))))
    (beginning-of-line)
    (gnus-add-text-properties
     (point)
     (prog1 (1+ (point))
       ;; Insert the text.
       (eval gnus-category-line-format-spec))
     (list 'gnus-category gnus-tmp-name))))

(defun gnus-enter-category-buffer ()
  "Go to the Category buffer."
  (interactive)
  (gnus-category-setup-buffer)
  (gnus-configure-windows 'category)
  (gnus-category-prepare))

(defun gnus-category-setup-buffer ()
  (unless (get-buffer gnus-category-buffer)
    (save-excursion
      (set-buffer (gnus-get-buffer-create gnus-category-buffer))
      (gnus-category-mode))))

(defun gnus-category-prepare ()
  (gnus-set-format 'category-mode)
  (gnus-set-format 'category t)
  (let ((alist gnus-category-alist)
	(buffer-read-only nil))
    (erase-buffer)
    (while alist
      (gnus-category-insert-line (pop alist)))
    (goto-char (point-min))
    (gnus-category-position-point)))

(defun gnus-category-name ()
  (or (intern (get-text-property (gnus-point-at-bol) 'gnus-category))
      (error "No category on the current line")))

(defun gnus-category-read ()
  "Read the category alist."
  (setq gnus-category-alist
	(or (gnus-agent-read-file
	     (nnheader-concat gnus-agent-directory "lib/categories"))
	    (list (list 'default 'short nil nil)))))

(defun gnus-category-write ()
  "Write the category alist."
  (setq gnus-category-predicate-cache nil
	gnus-category-group-cache nil)
  (gnus-make-directory (nnheader-concat gnus-agent-directory "lib"))
  (with-temp-file (nnheader-concat gnus-agent-directory "lib/categories")
    (prin1 gnus-category-alist (current-buffer))))

(defun gnus-category-edit-predicate (category)
  "Edit the predicate for CATEGORY."
  (interactive (list (gnus-category-name)))
  (let ((info (assq category gnus-category-alist)))
    (gnus-edit-form
     (cadr info) (format "Editing the predicate for category %s" category)
     `(lambda (predicate)
	(setcar (cdr (assq ',category gnus-category-alist)) predicate)
	(gnus-category-write)
	(gnus-category-list)))))

(defun gnus-category-edit-score (category)
  "Edit the score expression for CATEGORY."
  (interactive (list (gnus-category-name)))
  (let ((info (assq category gnus-category-alist)))
    (gnus-edit-form
     (caddr info)
     (format "Editing the score expression for category %s" category)
     `(lambda (groups)
	(setcar (nthcdr 2 (assq ',category gnus-category-alist)) groups)
	(gnus-category-write)
	(gnus-category-list)))))

(defun gnus-category-edit-groups (category)
  "Edit the group list for CATEGORY."
  (interactive (list (gnus-category-name)))
  (let ((info (assq category gnus-category-alist)))
    (gnus-edit-form
     (cadddr info) (format "Editing the group list for category %s" category)
     `(lambda (groups)
	(setcar (nthcdr 3 (assq ',category gnus-category-alist)) groups)
	(gnus-category-write)
	(gnus-category-list)))))

(defun gnus-category-kill (category)
  "Kill the current category."
  (interactive (list (gnus-category-name)))
  (let ((info (assq category gnus-category-alist))
	(buffer-read-only nil))
    (gnus-delete-line)
    (setq gnus-category-alist (delq info gnus-category-alist))
    (gnus-category-write)))

(defun gnus-category-copy (category to)
  "Copy the current category."
  (interactive (list (gnus-category-name) (intern (read-string "New name: "))))
  (let ((info (assq category gnus-category-alist)))
    (push (list to (gnus-copy-sequence (cadr info))
		(gnus-copy-sequence (caddr info)) nil)
	  gnus-category-alist)
    (gnus-category-write)
    (gnus-category-list)))

(defun gnus-category-add (category)
  "Create a new category."
  (interactive "SCategory name: ")
  (when (assq category gnus-category-alist)
    (error "Category %s already exists" category))
  (push (list category 'false nil nil)
	gnus-category-alist)
  (gnus-category-write)
  (gnus-category-list))

(defun gnus-category-list ()
  "List all categories."
  (interactive)
  (gnus-category-prepare))

(defun gnus-category-exit ()
  "Return to the group buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (gnus-configure-windows 'group t))

;; To avoid having 8-bit characters in the source file.
(defvar gnus-category-not (list '! 'not (intern (format "%c" 172))))

(defvar gnus-category-predicate-alist
  '((spam . gnus-agent-spam-p)
    (short . gnus-agent-short-p)
    (long . gnus-agent-long-p)
    (low . gnus-agent-low-scored-p)
    (high . gnus-agent-high-scored-p)
    (read . gnus-agent-read-p)
    (true . gnus-agent-true)
    (false . gnus-agent-false))
  "Mapping from short score predicate symbols to predicate functions.")

(defun gnus-agent-spam-p ()
  "Say whether an article is spam or not."
  (unless gnus-agent-spam-hashtb
    (setq gnus-agent-spam-hashtb (gnus-make-hashtable 1000)))
  (if (not (equal (mail-header-references gnus-headers) ""))
      nil
    (let ((string (gnus-simplify-subject (mail-header-subject gnus-headers))))
      (prog1
	  (gnus-gethash string gnus-agent-spam-hashtb)
	(gnus-sethash string t gnus-agent-spam-hashtb)))))

(defun gnus-agent-short-p ()
  "Say whether an article is short or not."
  (< (mail-header-lines gnus-headers) gnus-agent-short-article))

(defun gnus-agent-long-p ()
  "Say whether an article is long or not."
  (> (mail-header-lines gnus-headers) gnus-agent-long-article))

(defun gnus-agent-low-scored-p ()
  "Say whether an article has a low score or not."
  (< gnus-score gnus-agent-low-score))

(defun gnus-agent-high-scored-p ()
  "Say whether an article has a high score or not."
  (> gnus-score gnus-agent-high-score))

(defun gnus-agent-read-p ()
  "Say whether an article is read or not."
  (gnus-member-of-range (mail-header-number gnus-headers)
			(gnus-info-read (gnus-get-info gnus-newsgroup-name))))

(defun gnus-category-make-function (cat)
  "Make a function from category CAT."
  (let ((func (gnus-category-make-function-1 cat)))
    (if (and (= (length func) 1)
	     (symbolp (car func)))
	(car func)
      (gnus-byte-compile `(lambda () ,func)))))

(defun gnus-agent-true ()
  "Return t."
  t)

(defun gnus-agent-false ()
  "Return nil."
  nil)

(defun gnus-category-make-function-1 (cat)
  "Make a function from category CAT."
  (cond
   ;; Functions are just returned as is.
   ((or (symbolp cat)
	(gnus-functionp cat))
    `(,(or (cdr (assq cat gnus-category-predicate-alist))
	   cat)))
   ;; More complex category.
   ((consp cat)
    `(,(cond
	((memq (car cat) '(& and))
	 'and)
	((memq (car cat) '(| or))
	 'or)
	((memq (car cat) gnus-category-not)
	 'not))
      ,@(mapcar 'gnus-category-make-function-1 (cdr cat))))
   (t
    (error "Unknown category type: %s" cat))))

(defun gnus-get-predicate (predicate)
  "Return the predicate for CATEGORY."
  (or (cdr (assoc predicate gnus-category-predicate-cache))
      (let ((func (gnus-category-make-function predicate)))
	(setq gnus-category-predicate-cache
	      (nconc gnus-category-predicate-cache
		     (list (cons predicate func))))
	func)))

(defun gnus-predicate-implies-unread (predicate)
  "Say whether PREDICATE implies unread articles only.
It is okay to miss some cases, but there must be no false positives.
That is, if this function returns true, then indeed the predicate must
return only unread articles."
  ;; Todo: make this work in more cases.
  (equal predicate '(not read)))

(defun gnus-group-category (group)
  "Return the category GROUP belongs to."
  (unless gnus-category-group-cache
    (setq gnus-category-group-cache (gnus-make-hashtable 1000))
    (let ((cs gnus-category-alist)
	  groups cat)
      (while (setq cat (pop cs))
	(setq groups (cadddr cat))
	(while groups
	  (gnus-sethash (pop groups) cat gnus-category-group-cache)))))
  (or (gnus-gethash group gnus-category-group-cache)
      (assq 'default gnus-category-alist)))

(defun gnus-agent-expire (&optional articles group force)
  "Expire all old articles.
If you want to force expiring of certain articles, this function can
take ARTICLES, GROUP and FORCE parameters as well.

The articles on which the expiration process runs are selected as follows:
  if ARTICLES is null, all read and unmarked articles.
  if ARTICLES is t, all articles.
  if ARTICLES is a list, just those articles.
Setting GROUP will limit expiration to that group.
FORCE is equivalent to setting gnus-agent-expire-days to zero(0)."
  (interactive)

  (if force (setq force 'forced))

  (if (or (not (eq articles t))
          (yes-or-no-p (concat "Are you sure that you want to expire all articles in " (if group group "every agentized group") ".")))
      (let ((methods (if group
                         (list (gnus-find-method-for-group group))
                       gnus-agent-covered-methods))
            (day (if (numberp gnus-agent-expire-days)
                     (- (time-to-days (current-time)) gnus-agent-expire-days)
                   nil))
            gnus-command-method sym arts pos
            history overview file histories elem art nov-file low info
            unreads marked article orig lowest highest found days)
        (save-excursion
          (setq overview (gnus-get-buffer-create " *expire overview*"))
          (while (setq gnus-command-method (pop methods))
            (when (file-exists-p (gnus-agent-lib-file "active"))
              (with-temp-buffer
                (nnheader-insert-file-contents (gnus-agent-lib-file "active"))
                (gnus-active-to-gnus-format
                 gnus-command-method
                 (setq orig (gnus-make-hashtable
                             (count-lines (point-min) (point-max))))))
              (dolist (expiring-group (gnus-groups-from-server gnus-command-method))
                (if (or (not group)
                        (equal group expiring-group))
                    (let* ((dir (concat
                                 (gnus-agent-directory)
                                 (gnus-agent-group-path expiring-group) "/")))
                      (cond ((gnus-gethash-safe expiring-group; KJG (gnus-group-real-name expiring-group)
                                                orig)
                             (gnus-agent-load-alist expiring-group)
                             (gnus-message 5 "Expiring articles in %s" expiring-group)
                             (let* ((info (gnus-get-info expiring-group))
                                    (alist gnus-agent-article-alist)
                                    changed-alist
                                    (specials (if alist
                                                  (list (caar (last alist)))))
                                    (unreads;; Articles that are excluded from the expiration process
                                     (cond (gnus-agent-expire-all
                                            ;; All articles are marked read by global decree
                                            nil)
                                           ((eq articles t)
                                            ;; All articles are marked read by function parameter
                                            nil)
                                           ((not articles)
                                            ;; Unread articles are marked protected from expiration
                                            (ignore-errors (gnus-list-of-unread-articles expiring-group)))
                                           (t
                                            ;; All articles EXCEPT those named by the caller are protected from expiration
                                            (gnus-sorted-difference (gnus-uncompress-range (cons (caar alist) (caar (last alist)))) (sort articles '<)))))
                                    (marked;; More articles that are exluded from the expiration process
                                     (cond (gnus-agent-expire-all
                                            ;; All articles are unmarked by global decree
                                            nil)
                                           ((eq articles t)
                                            ;; All articles are unmarked by function parameter
                                            nil)
                                           (articles
                                            ;; All articles may as well be unmarked as the unreads list already names the articles we are going to keep
                                            nil)
                                           (t
                                            ;; Ticked and/or dormant articles are excluded from expiration
                                            (nconc
                                             (gnus-uncompress-range
                                              (cdr (assq 'tick (gnus-info-marks info))))
                                             (gnus-uncompress-range
                                              (cdr (assq 'dormant
                                                         (gnus-info-marks info))))))
                                           ))
                                    (keep (sort (nconc specials unreads marked) '<))
                                    (nov-file (concat dir ".overview"))
                                    (len (length alist))
                                    (cnt 0)
                                    type)
                               (when (file-exists-p nov-file)
                                 (set-buffer overview)
                                 (erase-buffer)
                                 (nnheader-insert-file-contents nov-file)
                                 (goto-char (point-min))
                                 (set-buffer-modified-p nil))
                               (while alist
                                 (let ((art (caar alist)))
                                   (gnus-message 9 "Processing %d of %d" (setq cnt (1+ cnt)) len)
                                   (while (< (or (car keep) (1+ art)) art)
                                     (ignore-errors
                                       (while (let ((nov-art (read (current-buffer))))
                                                (cond ((< nov-art (car keep))
                                                       (gnus-delete-line)
                                                       t)
                                                      ((= nov-art (car keep))
                                                       (forward-line 1)
                                                       nil)
                                                      (t
                                                       (beginning-of-line)
                                                       nil)))))
                                     (setq keep (cdr keep)))
                         
                                   (cond ((eq art (car keep))
                                          (if (and (cdar alist)
                                                   (not (file-exists-p (concat dir (number-to-string art)))))
                                              (progn (setcdr (car alist) nil)
                                                     (gnus-message 7 "Article %d: cleared download flag as local file missing" (caar alist))
                                                     (setq changed-alist t)))
                                          (setq alist (cdr alist)
                                                keep (cdr keep))
                                          (condition-case nil
                                              (while (let ((nov-art (read (current-buffer))))
                                                       (cond ((< nov-art art)
                                                              (gnus-message 7 "Article %d: NOV line removed" nov-art)
                                                              (gnus-delete-line)
                                                              t)
                                                             ((= nov-art art)
                                                              (forward-line 1)
                                                              nil)
                                                             (t
                                                              (beginning-of-line)
                                                              nil))))
                                            (error (forward-line 1))))
                                         ((setq type (let ((fetch-date (cdar alist)))
                                                       (or
                                                        ;; if read but not downloaded
                                                        (if (and (numberp fetch-date)
                                                                 (file-exists-p (concat dir (number-to-string art))))
                                                            nil
                                                          'read)
                                                        ;; We now have the arrival day, so we see
                                                        ;; whether it's old enough to be expired.
                                                        (if (< fetch-date
                                                               (if (numberp day)
                                                                   day
                                                                 (let (found
                                                                       (days gnus-agent-expire-days))
                                                                   (while (and (not found)
                                                                               days)
                                                                     (when (eq 0 (string-match (caar days) expiring-group))
                                                                       (setq found (cadar days)))
                                                                     (pop days))
                                                                   found)))
                                                            'expired)
                                                        force)))
                                          
                                          (if gnus-agent-consider-all-articles
                                              (setq alist (cdr alist)) ;; Iterate forward
                                            (gnus-message 7 "Article %d: Removed %s article from alist" art type)
                                            (setcar alist (cadr alist))
                                            (setcdr alist (cddr alist))
                                            (setq changed-alist t))

                                          (if (memq type '(forced expired))
                                              (ignore-errors
                                                (delete-file (concat dir (number-to-string art)))
                                                (gnus-message 7 "Article %d: Expired local copy" art)))
                                          (ignore-errors
                                            (let (nov-art)
                                              (while (<= (setq nov-art (read (current-buffer))) art)
                                                (gnus-message 7 "Article %d: NOV line removed" nov-art)
                                                (gnus-delete-line)))
                                            (beginning-of-line))
                                          )
                                         (t
                                          (setq alist (cdr alist)))
                                         )
                                   )
                                 )

                               (let ((inhibit-quit t))
                                 (if changed-alist
                                     (gnus-agent-save-alist expiring-group))
                                 (if (buffer-modified-p)
				     (progn
				       (gnus-make-directory dir)
				       (write-region-as-coding-system
					gnus-agent-file-coding-system
					(point-min) (point-max) nov-file
					nil 'silent)
                                       ;; clear the modified flag as that I'm not confused by its status on the next pass through this routine.
                                       (set-buffer-modified-p nil))
                                   )
                                 (if (eq articles t)
                                     (gnus-summary-update-info))
                                 ))))))))))))
  (gnus-message 4 "Expiry...done"))

;;;###autoload
(defun gnus-agent-batch ()
  "Start Gnus, send queue and fetch session."
  (interactive)
  (let ((init-file-user "")
	(gnus-always-read-dribble-file t))
    (gnus))
  (let ((gnus-agent-confirmation-function 'gnus-agent-batch-confirmation))
    (gnus-group-send-queue)
    (gnus-agent-fetch-session)))

(defun gnus-agent-uncached-articles (articles group &optional cached-header)
  "Constructs sublist of ARTICLES that excludes those articles ids in GROUP that have already been fetched.
 If CACHED-HEADER is nil, articles are only excluded if the article itself has been fetched."

;; Logically equivalent to: (gnus-sorted-difference articles (mapcar 'car gnus-agent-article-alist))
;; Functionally, I don't need to construct a temp list using mapcar.

  (if (gnus-agent-load-alist group)
    (let* ((ref gnus-agent-article-alist)
           (arts articles)
           (uncached (list nil))
           (tail uncached))
      (while (and ref arts)
        (let ((v1 (car arts))
              (v2 (caar ref)))
          (cond ((< v1 v2) ; the article (v1) does not appear in the reference list
                 (setq tail (setcdr tail (list v1)))
                 (pop arts))
                ((= v1 v2)
                 (unless (or cached-header (cdar ref)) ; the article (v1) is already cached
                   (setq tail (setcdr tail (list v1))))
                 (pop arts)
                 (pop ref))
                (t ; the reference article (v2) preceeds the list being filtered
                 (pop ref)))))
      (while arts
        (setq tail (setcdr tail (list (pop arts)))))
      (cdr uncached))
    ;; if gnus-agent-load-alist fails, no articles are cached.
    articles))

(defun gnus-agent-retrieve-headers (articles group &optional fetch-old)
  (save-excursion
    (gnus-agent-create-buffer)
    (let ((gnus-decode-encoded-word-function 'identity)
	  (file (gnus-agent-article-name ".overview" group))
	  cached-articles uncached-articles)
      (gnus-make-directory (nnheader-translate-file-chars
			    (file-name-directory file) t))

      ;; Populate temp buffer with known headers
      (when (file-exists-p file)
	(with-current-buffer gnus-agent-overview-buffer
	  (erase-buffer)
	  (let ((nnheader-file-coding-system
		 gnus-agent-file-coding-system))
	    (nnheader-insert-nov-file file (car articles)))))

      (if (setq uncached-articles (gnus-agent-uncached-articles articles group t))
	  (progn
            ;; Populate nntp-server-buffer with uncached headers
	    (set-buffer nntp-server-buffer)
	    (erase-buffer)
	    (let (gnus-agent-cache)     ; Turn off agent cache
	      (cond ((not (eq 'nov (gnus-retrieve-headers
                                    uncached-articles group fetch-old)))
                     (nnvirtual-convert-headers))
                    ((eq 'nntp (car gnus-current-select-method))
                     ;; The author of gnus-get-newsgroup-headers-xover reports that the XOVER command
                     ;; is commonly unreliable. The problem is that recently posted articles may not 
                     ;; be entered into the NOV database in time to respond to my XOVER query.
                     ;;
                     ;; I'm going to use his assumption that the NOV database is updated in order 
                     ;; of ascending article ID.  Therefore, a response containing article ID N 
                     ;; implies that all articles from 1 to N-1 are up-to-date.  Therefore, 
                     ;; missing articles in that range have expired.
                     
                     (set-buffer nntp-server-buffer)
                     (let* ((fetched-articles (list nil))
                            (tail fetched-articles)
                            (min (cond ((numberp fetch-old)
                                        (max 1 (- (car articles) fetch-old)))
                                       (fetch-old
                                        1)
                                       (t
                                        (car articles))))
                            (max (car (last articles))))
                       
                       ;; Get the list of articles that were fetched
                       (goto-char (point-min))
                       (ignore-errors 
                         (while t
                           (setq tail (setcdr tail (cons (read (current-buffer)) nil)))
                           (forward-line 1)))
                       
                       ;; Clip this list to the headers that will actually be returned
                       (setq fetched-articles (gnus-list-range-intersection
                                               (cdr fetched-articles)
                                               (cons min max)))

                       ;; Clip the uncached articles list to exclude IDs after the last FETCHED header.  
                       ;; The excluded IDs may be fetchable using HEAD.
                       (if (car tail)
                           (setq uncached-articles (gnus-list-range-intersection 
                                                    uncached-articles 
                                                    (cons (car uncached-articles) (car tail)))))

                       ;; Create the list of articles that were "successfully" fetched.  Success, in
                       ;; this case, means that the ID should not be fetched again.  In the case of 
                       ;; an expired article, the header will not be fetched.
                       (setq uncached-articles (gnus-sorted-nunion fetched-articles uncached-articles))
                       ))))

            ;; Erase the temp buffer
	    (set-buffer gnus-agent-overview-buffer)
	    (erase-buffer)

            ;; Copy the nntp-server-buffer to the temp buffer
	    (set-buffer nntp-server-buffer)
	    (copy-to-buffer gnus-agent-overview-buffer (point-min) (point-max))

            ;; Merge the temp buffer with the known headers (found on disk in FILE) into the nntp-server-buffer
	    (when (and uncached-articles (file-exists-p file))
	      (gnus-agent-braid-nov group uncached-articles file))

            ;; Save the new set of known headers to FILE
	    (set-buffer nntp-server-buffer)
	    (gnus-agent-check-overview-buffer)
	    (write-region-as-coding-system
	     gnus-agent-file-coding-system
	     (point-min) (point-max) file nil 'silent)
            
            ;; Update the group's article alist to include the newly fetched articles.
	    (gnus-agent-load-alist group)
	    (gnus-agent-save-alist group uncached-articles nil)
            )
        
        ;; Copy the temp buffer to the nntp-server-buffer
        (set-buffer nntp-server-buffer)
	(erase-buffer)
	(insert-buffer-substring gnus-agent-overview-buffer)))

    (if (and fetch-old
	     (not (numberp fetch-old)))
	t				; Don't remove anything.
      (nnheader-nov-delete-outside-range
       (if fetch-old (max 1 (- (car articles) fetch-old))
	 (car articles))
       (car (last articles)))
      t)

    'nov))

(defun gnus-agent-request-article (article group)
  "Retrieve ARTICLE in GROUP from the agent cache."
  (let* ((gnus-command-method (gnus-find-method-for-group group))
	 (file (concat
		  (gnus-agent-directory)
		  (gnus-agent-group-path group) "/"
		  (number-to-string article)))
	 (buffer-read-only nil))
    (when (and (file-exists-p file)
	       (> (nth 7 (file-attributes file)) 0))
      (erase-buffer)
      (gnus-kill-all-overlays)
      (insert-file-contents-as-coding-system gnus-cache-coding-system file)
      t)))

(defun gnus-agent-regenerate-group (group &optional reread)
  "Regenerate GROUP.  If REREAD is t, all articles in the .overview are marked as unread.  If REREAD is not nil, downloaded articles are marked as unread."
  (gnus-message 5 "Regenerating in %s" group)
  (let* ((gnus-command-method (or gnus-command-method
                                  (gnus-find-method-for-group group)))
         (file (gnus-agent-article-name ".overview" group))
         (dir (file-name-directory file))
         point
	 (gnus-tmp-downloaded
	  (if (file-exists-p dir)
	      (sort (mapcar (lambda (name) (string-to-int name))
			    (directory-files dir nil "^[0-9]+$" t))
		    '>)
	    (progn (gnus-make-directory dir) nil)))
         dl nov-arts
         alist header
         regenerated)

    (mm-with-unibyte-buffer
     (if (file-exists-p file)
         (let ((nnheader-file-coding-system
                gnus-agent-file-coding-system))
           (nnheader-insert-file-contents file)))
     (set-buffer-modified-p nil)

     ;; Load the article IDs found in the overview file.  As a side-effect, validate the file contents.
     (let ((load t))
       (while load
         (setq load nil)
         (goto-char (point-min))
         (while (< (point) (point-max))
           (cond ((looking-at "[0-9]+\\b")
                  (push (read (current-buffer)) nov-arts)
                  (forward-line 1)
                  (let ((l1 (car nov-arts))
                        (l2 (cadr nov-arts)))
                    (cond ((not l2)
                           nil)
                          ((< l1 l2)
                           ;; Don't sort now as I haven't verified that every line begins with a number
                           (setq load t))
                          ((= l1 l2)
                           (forward-line -1)
                           (gnus-delete-line)
                           (pop nov-arts)))))
                 (t
                  (gnus-delete-line))))
         (if load
             (progn (sort-numeric-fields 1 (point-min) (point-max))
                    (setq nov-arts nil)))))
     (gnus-agent-check-overview-buffer)

     ;; Construct a new article alist whose nodes match every header in the .overview file.  
     ;; As a side-effect, missing headers are reconstructed from the downloaded article file.
     (while (or downloaded nov-arts)
       (cond ((and downloaded 
                   (or (not nov-arts)
                       (> (car downloaded) (car nov-arts))))
              ;; This entry is missing from the overview file
              (gnus-message 6 "Regenerating NOV %s %d..." group (car downloaded))
              (let ((file (concat dir (number-to-string (car downloaded)))))
                (mm-with-unibyte-buffer
                 (nnheader-insert-file-contents file)
                 (nnheader-remove-body)
                 (setq header (nnheader-parse-naked-head)))
                (mail-header-set-number header (car downloaded))
                (if nov-arts
                    (let ((key (concat "^" (int-to-string (car nov-arts)) "\t")))
                      (or (re-search-backward key nil t)
                          (re-search-forward key))
                      (forward-line 1))
                  (goto-char (point-min)))
                (nnheader-insert-nov header))
              (setq nov-arts (cons (car downloaded) nov-arts)))
             ((eq (car downloaded) (car nov-arts))
              ;; This entry in the overview has been downloaded
              (push (cons (car downloaded) (time-to-days (nth 5 (file-attributes (concat dir (number-to-string (car downloaded))))))) alist)
              (pop downloaded)
              (pop nov-arts))
             (t
              ;; This entry in the overview has not been downloaded
              (push (cons (car nov-arts) nil) alist)
              (pop nov-arts))))

     ;; When gnus-agent-consider-all-articles is set, gnus-agent-regenerate-group should NOT remove article IDs 
     ;; from the alist.  Those IDs serve as markers to indicate that an attempt has been made to fetch that 
     ;; article's header.

     ;; When gnus-agent-consider-all-articles is NOT set, gnus-agent-regenerate-group can remove the article
     ;; ID of every article (with the exception of the last ID in the list - it's special) that no longer appears in the overview.
     ;; In this situtation, the last article ID in the list implies that it, and every article ID preceeding it, 
     ;; have been fetched from the server.
     (if gnus-agent-consider-all-articles
         ;; Restore all article IDs that were not found in the overview file.
         (let* ((n (cons nil alist))
                (merged n)
                (o (gnus-agent-load-alist group)))
           (while o
             (let ((nID (caadr n))
                   (oID (caar o)))
               (cond ((not nID)
                      (setq n (setcdr n (list (list oID))))
                      (pop o))
                     ((< oID nID)
                      (setcdr n (cons (list oID) (cdr n)))
                      (pop o))
                     ((= oID nID)
                      (pop o)
                      (pop n))
                     (t
                      (pop n)))))
           (setq alist (cdr merged)))
       ;; Restore the last article ID if it is not already in the new alist
       (let ((n (last alist))
             (o (last (gnus-agent-load-alist group))))
         (cond ((not n)
                (when o
                  (push (cons (caar o) nil) alist)))
               ((< (caar n) (caar o))
                (setcdr n (list (car o)))))))
                     
     (if (setq regenerated (buffer-modified-p))
	 (write-region-as-coding-system
	  gnus-agent-file-coding-system
	  (point-min) (point-max) file nil 'silent))
     )

    (setq regenerated (or regenerated
                          (and reread gnus-agent-article-alist)
                          (not (equal alist gnus-agent-article-alist)))
          )

    (setq gnus-agent-article-alist alist)
 
    (when regenerated
      (gnus-agent-save-alist group))

    (when (and reread gnus-agent-article-alist)
      (gnus-make-ascending-articles-unread
       group
       (delq nil (mapcar (function (lambda (c)
                                     (cond ((eq reread t)
                                            (car c))
                                           ((cdr c)
                                            (car c)))))
                         gnus-agent-article-alist)))

      (when (gnus-buffer-live-p gnus-group-buffer)
        (gnus-group-update-group group t)
        (sit-for 0))
      )

    regenerated))

;;;###autoload
(defun gnus-agent-regenerate (&optional clean reread)
  "Regenerate all agent covered files.
If CLEAN, don't read existing active files."
  (interactive "P")
  (let (regenerated)
    (gnus-message 4 "Regenerating Gnus agent files...")
    (dolist (gnus-command-method gnus-agent-covered-methods)
      (let ((active-file (gnus-agent-lib-file "active"))
            active-hashtb active-changed
            point)
        (gnus-make-directory (file-name-directory active-file))
        (if clean
            (setq active-hashtb (gnus-make-hashtable 1000))
          (mm-with-unibyte-buffer
           (if (file-exists-p active-file)
               (let ((nnheader-file-coding-system
                      gnus-agent-file-coding-system))
                 (nnheader-insert-file-contents active-file))
             (setq active-changed t))
           (gnus-active-to-gnus-format
            nil (setq active-hashtb
                      (gnus-make-hashtable
                       (count-lines (point-min) (point-max)))))))
        (dolist (group (gnus-groups-from-server gnus-command-method))
          (setq regenerated (or (gnus-agent-regenerate-group group reread)
                                regenerated))
          (let ((min (or (caar gnus-agent-article-alist) 1))
                (max (or (caar (last gnus-agent-article-alist)) 0))
                (active (gnus-gethash-safe (gnus-group-real-name group)
                                           active-hashtb))
                (read (gnus-info-read (gnus-get-info group))))
            (if (not active)
                (progn
                  (setq active (cons min max)
                        active-changed t)
                  (gnus-sethash group active active-hashtb))
              (when (> (car active) min)
                (setcar active min)
                (setq active-changed t))
              (when (< (cdr active) max)
                (setcdr active max)
                (setq active-changed t)))))
        (when active-changed
          (setq regenerated t)
          (gnus-message 4 "Regenerate %s" active-file)
          (let ((nnmail-active-file-coding-system gnus-agent-file-coding-system))
            (gnus-write-active-file active-file active-hashtb)))))
    (gnus-message 4 "Regenerating Gnus agent files...done")
    regenerated))

(defun gnus-agent-go-online (&optional force)
  "Switch servers into online status."
  (interactive (list t))
  (dolist (server gnus-opened-servers)
    (when (eq (nth 1 server) 'offline)
      (if (if (eq force 'ask)
	      (gnus-y-or-n-p
	       (format "Switch %s:%s into online status? "
		       (caar server) (cadar server)))
	    force)
	  (setcar (nthcdr 1 server) 'close)))))

(defun gnus-agent-toggle-group-plugged (group)
  "Toggle the status of the server of the current group."
  (interactive (list (gnus-group-group-name)))
  (let* ((method (gnus-find-method-for-group group))
	 (status (cadr (assoc method gnus-opened-servers))))
    (if (eq status 'offline)
	(gnus-server-set-status method 'closed)
      (gnus-close-server method)
      (gnus-server-set-status method 'offline))
    (message "Turn %s:%s from %s to %s." (car method) (cadr method)
	     (if (eq status 'offline) 'offline 'online)
	     (if (eq status 'offline) 'online 'offline))))

(defun gnus-agent-group-covered-p (group)
  (member (gnus-group-method group)
	  gnus-agent-covered-methods))

(provide 'gnus-agent)

;;; gnus-agent.el ends here

;; pop3-fma.el.el --- POP3 for Multiple Account for Gnus.
;; Copyright (C) 1996,97,98 Free Software Foundation, Inc. , Tatsuya Ichikawa
;; Author: Tatsuya Ichikawa <t-ichi@po.shiojiri.ne.jp>
;; Version: 0.12
;; Keywords: mail , gnus , pop3
;;
;; SPECIAL THANKS
;;    Keiichi Suzuki <kei-suzu@mail.wbs.or.jp>
;;    Katsumi Yamaoka <yamaoka@jpl.org>
;;
;; This file is not part of GNU Emacs.

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
;;
;;
;; How to use.
;;
;; add your .emacs following codes.
;;
;;  (autoload 'pop3-fma-set-pop3-password "pop3-fma")
;;  (add-hook 'gnus-load-hook 'pop3-fma-set-pop3-password)
;;  (setq pop3-fma-spool-file-alist
;;        '(
;;  	    "po:username0@mailhost0.your.domain0"
;;	    "po:username1@mailhost1.your.domain1"
;;                         :
;;                         :
;;	   ))
;;
;; Variables
;;
;;  pop3-fma-spool-file-alist      ... Spool file alist of POP3 protocol
;;  pop3-fma-movemail-type         ... Type of movemail program.
;;                                         'lisp or 'exe
;;                                         'lisp use pop3.el
;;                                         'exe use movemail
;;  pop3-fma-movemail-arguments    ... List of options of movemail program.
;;
;;; Code:

(require 'cl)
(require 'custom)

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

(defgroup pop3-fma nil
  "Multile POP3 account utility for Gnus."
  :prefix "pop3-fma-"
  :group 'mail
  :group 'news)

(defconst pop3-fma-version-number "0.12")
(defconst pop3-fma-codename
;;;  "Feel the wind"		; 0.10
;;  "My home town"  		; 0.11
  "On the road"			; 0.12
;;  "Before generation of Love"	; 0.xx
;;  "Lonely Christmas eve"	; 0.xx
;;  "Rock'n Roll city"		; 0.xx
;;  "Money"			; 0.xx
;;  "Midnight blue train" 	; 0.xx
;;  "Still 19"       		; 0.xx
;;  "J boy"          		; 0.xx
;;  "Blood line"		; 0.xx
;;  "Star ring"			; 0.xx
;;  "Goodbye Game"		; 0.xx
  )
(defconst pop3-fma-version (format "Multiple POP3 account utiliy for Gnus v%s - \"%s\""
				       pop3-fma-version-number
				       pop3-fma-codename))

(defcustom pop3-fma-spool-file-alist nil
  "*Spoolfile to get mail using pop3 protocol.
You should specify this variable like
 '(
   \"po:user1@mailhost1\"
   \"po:user2@mailhost2\"
  )"
  :group 'pop3-fma
  :type 'alist)

(defcustom pop3-fma-movemail-type 'lisp
  "*Type of movemail program.
Lisp means `nnmail-movemail-program' is lisp function.
 Exe means `nnmail-movemail-program' is external program.
 Please do not use exe if you do not use Meadow."
  :group 'pop3-fma
  :type '(choice (const lisp)
		 (const exe)))

(defcustom pop3-fma-movemail-arguments '("-pf")
  "*Options for movemail."
  :group 'pop3-fma
  :type '(repeat (string :tag "Argument")))

;;; Internal variables.
(defvar pop3-fma-password nil
  "*POP3 password , user , mailhost information for Gnus.")

(defvar pop3-fma-movemail-program "movemail.exe"
  "*External program name your movemail.
Please do not set this valiable non-nil if you do not use Meadow.")

;; Temporary variable
(defvar hdr nil)
(defvar passwd nil)
(defvar str nil)
(defvar pop3-fma-movemail-options pop3-fma-movemail-arguments)

(defun pop3-fma-init-hooks ()
  (add-hook 'message-send-hook 'pop3-fma-message-add-header))

(eval-after-load "message"
  '(pop3-fma-init-hooks))
;;
;;
;; Gnus POP3 additional utility...
;;
(defun pop3-fma-movemail (inbox crashbox)
  "Function to move mail from INBOX on a pop3 server to file CRASHBOX."
  (let ((pop3-maildrop
	 (substring inbox (match-end (string-match "^po:" inbox))
		    (- (match-end (string-match "^.*@" inbox)) 1)))
	(pop3-mailhost
	 (substring inbox (match-end (string-match "^.*@" inbox)))))
    (let ((pop3-password
	   (pop3-fma-read-passwd pop3-mailhost)))
      (message "Checking new mail user %s at %s..." pop3-maildrop pop3-mailhost)
      (if (and (eq system-type 'windows-nt)
	       (eq pop3-fma-movemail-type 'exe))
	  (progn
	    (setenv "MAILHOST" pop3-mailhost)
	    (if (and (not (memq pop3-password pop3-fma-movemail-arguments))
		     (not (memq (concat "po:" pop3-maildrop) pop3-fma-movemail-arguments)))
		(progn
		  (setq pop3-fma-movemail-arguments nil)
		  (setq pop3-fma-movemail-arguments
		      (append pop3-fma-movemail-options
			      (list
			       (concat "po:" pop3-maildrop)
			       crashbox
			       pop3-password)))))
	    (apply 'call-process (concat
				  exec-directory
				  pop3-fma-movemail-program)
		   nil nil nil
		   pop3-fma-movemail-arguments))
	(pop3-movemail crashbox)))))
;;
;;
(defun pop3-fma-read-passwd (mailhost)
  (setq passwd (nth 2 (assoc mailhost pop3-fma-password)))
  passwd)

(setq pop3-read-passwd 'pop3-fma-read-passwd)
;;
;; Set multiple pop3 server's password
(defun pop3-fma-store-password (passwd)
  (interactive
   (list (pop3-fma-read-noecho
	  (format "POP Password for %s at %s: " pop3-maildrop pop3-mailhost) t)))
  (if (not (assoc pop3-mailhost pop3-fma-password))
      (setq pop3-fma-password
	    (append pop3-fma-password
		    (list
		     (list
		      pop3-mailhost
		      pop3-maildrop
		      passwd))))
    (setcar (cdr (cdr (assoc pop3-mailhost pop3-fma-password)))
	    passwd))
  (message "POP password registered.")
  passwd)
;;
;;;###autoload
(defun pop3-fma-set-pop3-password()
  (interactive)
  (mapcar
   (lambda (x)
     (let ((pop3-maildrop
	    (substring x (match-end (string-match "^po:" x))
		       (- (match-end (string-match "^.*@" x)) 1)))
	   (pop3-mailhost
	    (substring x (match-end (string-match "^.*@" x)))))
       (call-interactively 'pop3-fma-store-password)))
   pop3-fma-spool-file-alist)
  (setq nnmail-movemail-program 'pop3-fma-movemail)
  (setq nnmail-spool-file pop3-fma-spool-file-alist))
;;
(defun pop3-fma-read-noecho (prompt &optional stars)
  "Read a single line of text from user without echoing, and return it.
Argument PROMPT ."
  (let ((ans "")
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t)
	(log-message-max-size 0)
	message-log-max done msg truncate)
    (while (not done)
      (if (or (not stars) (string-equal "" ans))
	  (setq msg prompt)
	(setq msg (concat prompt (make-string (length ans) ?*)))
	(setq truncate
	      (1+ (- (length msg) (window-width (minibuffer-window)))))
	(and (> truncate 0)
	     (setq msg (concat "$" (substring msg (1+ truncate))))))
      (message msg)
      (setq c (read-char-exclusive))
      (cond ((= c ?\C-g)
	     (setq quit-flag t
		   done t))
	    ((or (= c ?\r) (= c ?\n) (= c ?\e))
	     (setq done t))
	    ((= c ?\C-u)
	     (setq ans ""))
	    ((and (/= c ?\b) (/= c ?\177))
	     (setq ans (concat ans (char-to-string c))))
	    ((> (length ans) 0)
	     (setq ans (substring ans 0 -1)))))
    (if quit-flag
	(prog1
	    (setq quit-flag nil)
	  (message "Quit")
	  (beep t))
      (message "")
      ans)))
;;
;;
(defun pop3-fma-message-add-header ()
  (pop3-fma-add-custom-header "X-Ya-Pop3:" pop3-fma-version))
  
;;
;; Add your custom header.
;;
(defun pop3-fma-add-custom-header (header string)
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
(provide 'pop3-fma)
;;
;; pop3-fma.el ends here.

;; pop3-fma.el.el --- POP3 for Multiple Account for Gnus.
;; Copyright (C) 1996,97,98 Free Software Foundation, Inc. , Tatsuya Ichikawa
;;                                                           Yasuo Okabe
;; Author: Tatsuya Ichikawa <t-ichi@po.shiojiri.ne.jp>
;;         Yasuo OKABE <okabe@kuis.kyoto-u.ac.jp>
;; Version: 1.17
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
;; Note.
;;
;; This file store pop3 password in variable "pop3-fma-password".
;; Please take care by yourself to treat pop3 password.
;;
;; How to use.
;;
;; add your .emacs following codes.
;;
;;  (require 'pop3-fma)
;;  (setq pop3-fma-spool-file-alist
;;        '(
;;  	    ("po:username0@mailhost0.your.domain0" pass)
;;	    ("po:username1@mailhost1.your.domain1" apop)
;;                         :
;;                         :
;;	   ))
;;
;;	pass means normal authentication USER/PASS.
;;	apop means authentication using APOP.
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

(defconst pop3-fma-version-number "1.16")
(defconst pop3-fma-codename
;;  "J boy"          		; 1.00
;;  "Blood line"		; 1.10
;;  "Star ring"			; 1.11
;;  "Goodbye Game"		; 1.12
;;  "Love is Gamble"		; 1.13
;;  "Lonely"			; 1.14
;;  "Feel the wind"		; 1.16
  "Sadness like snow"		; 1.17
  )
(defconst pop3-fma-version (format "Multiple POP3 account utiliy for Gnus v%s - \"%s\""
				       pop3-fma-version-number
				       pop3-fma-codename))

(defcustom pop3-fma-spool-file-alist nil
  "*Spool file to get mail using pop3 protocol.
You should specify this variable like
 '(
   (\"po:user1@mailhost1\" type)
   (\"po:user2@mailhost2\" type)
  )
Type must be pass or apop."
  :group 'pop3-fma
  :type 'alist)

(defcustom pop3-fma-local-spool-file-alist nil
  "*List of Local spool file to get mail."
  :group 'pop3-fma
  :type 'alist)

(defcustom pop3-fma-movemail-type 'lisp
  "*Type of movemail program.
Lisp means `nnmail-movemail-program' is lisp function.
 Exe means `nnmail-movemail-program' is external program."
  :group 'pop3-fma
  :type '(choice (const lisp)
		 (const exe)))

(defcustom pop3-fma-movemail-arguments '("-pf")
  "*Options for movemail."
  :group 'pop3-fma
  :type '(repeat (string :tag "Argument")))

(defcustom pop3-fma-save-password-information nil
  "*If non nil , save POP Server's password information.
============== Important notice =====================
Please take care of your password information.
If set to t , your pop3 password is saved in pop3-fma-password in raw text.
So , Anybody can see this information by describe-variable.
If there is any problem , please set this variable to nil(default).
============== Important notice ====================="
  :group 'pop3-fma
  :type 'boolean)

;;; Internal variables.
(defvar pop3-fma-password nil
  "*POP3 password , user , mailhost information for Gnus.")

(defvar pop3-fma-movemail-program
  (if (eq system-type 'windows-nt)
      "movemail.exe"
    "movemail")
  "*External program name your movemail.")


;; Temporary variable
(defvar hdr nil)
(defvar passwd nil)
(defvar str nil)
(defvar spool nil)
(defvar movemail-output-buffer " *movemail-out*")
(defvar pop3-fma-commandline-arguments nil)

;;; To silence byte compiler
(and
 (fboundp 'eval-when-compile)
 (eval-when-compile
   (save-excursion
     (beginning-of-defun)
     (eval-region (point-min) (point)))
   (let (case-fold-search)
     (mapcar
      (function
       (lambda (symbol)
	 (unless (boundp symbol)
	   (make-local-variable symbol)
	   (eval (list 'setq symbol nil)))))
      '(:group
	:prefix :type
	pop3-maildrop
	pop3-mailhost
	))
     (make-local-variable 'byte-compile-warnings)
     (setq byte-compile-warnings nil))))

(defun pop3-fma-init-message-hook ()
  (add-hook 'message-send-hook 'pop3-fma-message-add-header))

(eval-after-load "message"
  '(pop3-fma-init-message-hook))

(add-hook 'gnus-after-exiting-gnus-hook
	  '(lambda () (setq pop3-fma-password nil)))
(add-hook 'gnus-before-startup-hook 'pop3-fma-set-pop3-password)

;;
;;
;; Gnus POP3 additional utility...
;;
(defun pop3-fma-movemail (inbox crashbox)
  "Function to move mail from INBOX on a pop3 server to file CRASHBOX."
  (if (string-match "^po:" inbox)
      (progn
	(let ((pop3-maildrop
	       (substring inbox (match-end (string-match "^po:" inbox))
			  (- (match-end (string-match "^.*@" inbox)) 1)))
	      (pop3-mailhost
	       (substring inbox (match-end (string-match "^.*@" inbox))))
	      (pop3-password
	       (if pop3-fma-save-password-information
		   (pop3-fma-read-passwd (substring inbox (match-end (string-match "^.*@" inbox))))
		 (pop3-fma-input-password
		  (substring inbox (match-end (string-match "^.*@" inbox)))
		  (substring inbox (match-end (string-match "^po:" inbox))
			     (- (match-end (string-match "^.*@" inbox)) 1)))))
	      (pop3-authentication-scheme
	       (nth 1 (assoc inbox pop3-fma-spool-file-alist))))
;;	      (pop3-fma-movemail-type (pop3-fma-get-movemail-type inbox)))
	  (if (eq pop3-authentication-scheme 'pass)
	      (message "Checking new mail user %s at %s using USER/PASS ..." pop3-maildrop pop3-mailhost)
	    (message "Checking new mail user %s at %s using APOP ..." pop3-maildrop pop3-mailhost))
	  (if (eq pop3-fma-movemail-type 'exe)
	      (progn
		(setenv "MAILHOST" pop3-mailhost)
		(if (and (not (memq pop3-password pop3-fma-commandline-arguments))
			 (not (memq (concat "po:" pop3-maildrop) pop3-fma-commandline-arguments)))
		    (progn
		      (if (eq pop3-authentication-scheme 'apop)
			  (setq pop3-fma-commandline-arguments
				(append
				 pop3-fma-movemail-arguments
				 (list
				  "-A"
				  (concat "po:" pop3-maildrop)
				  crashbox
				  pop3-password)))
			(setq pop3-fma-commandline-arguments
			      (append
			       pop3-fma-movemail-arguments
			       (list
				(concat "po:" pop3-maildrop)
				crashbox
				pop3-password))))))
		(if (not (get-buffer movemail-output-buffer))
		    (get-buffer-create movemail-output-buffer))
		(set-buffer movemail-output-buffer)
		(erase-buffer)
		(apply 'call-process (concat
				      exec-directory
				      pop3-fma-movemail-program)
		       nil movemail-output-buffer nil
		       pop3-fma-commandline-arguments)
		(let ((string (buffer-string)))
		  (if (> (length string) 0)
		      (progn
			(if (y-or-n-p
			     (concat (substring string 0
						(- (length string) 1))
						" continue ??"))
			    nil
			  nil)))))
	    (pop3-movemail crashbox))))
    (message "Checking new mail at %s ... " inbox)
    (call-process (concat exec-directory pop3-fma-movemail-program)
		  nil
		  nil
		  nil
		  inbox
		  crashbox)
    (message "Checking new mail at %s ... done." inbox)))
;;
;;
(defun pop3-fma-read-passwd (mailhost)
  (setq passwd (nth 2 (assoc mailhost pop3-fma-password)))
  passwd)

(defun pop3-fma-input-password (mailhost maildrop)
  (pop3-fma-read-noecho
   (format "POP Password for %s at %s: " maildrop mailhost) t))

(setq pop3-read-passwd 'pop3-fma-read-passwd
      nnmail-read-passwd 'pop3-fma-read-passwd)
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
		      passwd)))))
    (setcar (cdr (cdr (assoc pop3-mailhost pop3-fma-password)))
	    passwd)
    (message "POP password registered.")
    passwd)
;;
;;;###autoload
(defun pop3-fma-set-pop3-password()
  (interactive)
  (if pop3-fma-save-password-information
      (progn
	(mapcar
	 (lambda (x)
	   (let ((pop3-maildrop
		  (substring (car x) (match-end (string-match "^po:" (car x)))
			     (- (match-end (string-match "^.*@" (car x))) 1)))
		 (pop3-mailhost
		  (substring (car x) (match-end (string-match "^.*@" (car x))))))
	     (call-interactively 'pop3-fma-store-password)))
	 pop3-fma-spool-file-alist)))
  (setq nnmail-movemail-program 'pop3-fma-movemail)
;;  (setq nnmail-spool-file pop3-fma-spool-file-alist))
  (setq nnmail-spool-file (append
			   pop3-fma-local-spool-file-alist
			   (mapcar
			    (lambda (spool)
			      (car spool))
			    pop3-fma-spool-file-alist))))
;;
(defmacro pop3-fma-read-char-exclusive ()
  (cond ((featurep 'xemacs)
	 '(let ((table (quote ((backspace . ?\C-h) (delete . ?\C-?)
			       (left . ?\C-h))))
		event key)
	    (while (not
		    (and
		     (key-press-event-p (setq event (next-command-event)))
		     (setq key (or (event-to-character event)
				   (cdr (assq (event-key event) table)))))))
	    key))
	((fboundp 'read-char-exclusive)
	 '(read-char-exclusive))
	(t
	 '(read-char))))
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
      (setq c (pop3-fma-read-char-exclusive))
      (cond ((eq ?\C-g c)
	     (setq quit-flag t
		   done t))
	    ((memq c '(?\r ?\n ?\e))
	     (setq done t))
	    ((eq ?\C-u c)
	     (setq ans ""))
	    ((and (/= ?\b c) (/= ?\177 c))
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
  (if (message-mail-p)
      (pop3-fma-add-custom-header "X-Ya-Pop3:" pop3-fma-version)))
  
;;
;; Add your custom header.
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
;;
(defun pop3-fma-get-movemail-type (inbox)
  (if (eq (nth 1 (assoc inbox pop3-fma-spool-file-alist)) 'apop)
      'lisp
    pop3-fma-movemail-type))
;;
(provide 'pop3-fma)
;;
;; pop3-fma.el ends here.



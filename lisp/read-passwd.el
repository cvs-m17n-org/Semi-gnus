;; read-passwd.el.el --- Read password function for Pterodactyl Gnus.
;; Copyright (C) 1996,97,98 Free Software Foundation, Inc. , Tatsuya Ichikawa
;; Author: Tatsuya Ichikawa <t-ichi@po.shiojiri.ne.jp>
;; Version: 0.01
;; Keywords: mail , gnus , pop3 , password
;;
;; SPECIAL THANKS
;;    Katsumi Yamaoka <yamaoka@jpl.org>
;;
;; This file is part of Semi-gnus.

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
;;
;;
(require 'mail-source)

(defun read-pw-read-passwd (prompt)
  (read-pw-read-noecho prompt t))
;;
(defmacro read-pw-read-char-exclusive ()
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
(defun read-pw-read-noecho (prompt &optional stars)
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
      (setq c (read-pw-read-char-exclusive))
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
(defvar pw nil)
(defun read-pw-set-mail-source-passwd-cache ()
  (car (mapcar
	(lambda (x)
	  (mail-source-bind (pop x)
	    (let ((from (format "%s:%s:%s" server user port))
		  (mail-source-string (format "pop:%s@%s" user server)))
	      (setq pw (read-pw-return-passwd-string user server))
	      (unless (assoc user mail-source-password-cache)
		(set-alist 'mail-source-password-cache
			   (format "%s:%s:%s" server user port)
			   pw))
	      (cdr (assoc from mail-source-password-cache)))))
;;	gnus-offline-mail-source)))
	nnmail-spool-file)))
;;
;;
(defvar passwd nil)
(defun read-pw-return-passwd-string (user server)
  (setq passwd (read-pw-read-passwd
		(message "POP Password for %s at %s : " user server)))
  passwd)
;;
(provide 'read-passwd)

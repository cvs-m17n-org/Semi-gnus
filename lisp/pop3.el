;;; pop3.el --- Post Office Protocol (RFC 1460) interface

;; Copyright (C) 1996,1997,1998 Free Software Foundation, Inc.

;; Author: Richard L. Pieri <ratinox@peorth.gweep.net>
;; Keywords: mail, pop3
;; Version: 1.3m

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Most of the standard Post Office Protocol version 3 (RFC 1460) commands
;; are implemented.  The LIST command has not been implemented due to lack
;; of actual usefulness.
;; The optional POP3 command TOP has not been implemented.

;; This program was inspired by Kyle E. Jones's vm-pop program.

;;; Code:

(require 'mail-utils)
(provide 'pop3)

(defconst pop3-version "1.3m")

(defvar pop3-maildrop (or (user-login-name) (getenv "LOGNAME") (getenv "USER") nil)
  "*POP3 maildrop.")
(defvar pop3-mailhost (or (getenv "MAILHOST") nil)
  "*POP3 mailhost.")
(defvar pop3-port 110
  "*POP3 port.")

(defvar pop3-password-required t
  "*Non-nil if a password is required when connecting to POP server.")
(defvar pop3-password nil
  "*Password to use when connecting to POP server.")

(defvar pop3-authentication-scheme 'pass
  "*POP3 authentication scheme.
Defaults to 'pass, for the standard USER/PASS authentication.  Other valid
values are 'apop.")

(defvar pop3-timestamp nil
  "Timestamp returned when initially connected to the POP server.
Used for APOP authentication.")

(defvar pop3-read-point nil)
(defvar pop3-debug nil)

(defun pop3-movemail (&optional crashbox)
  "Transfer contents of a maildrop to the specified CRASHBOX."
  (or crashbox (setq crashbox (expand-file-name "~/.crashbox")))
  (let* ((process (pop3-open-server pop3-mailhost pop3-port))
	 (crashbuf (get-buffer-create " *pop3-retr*"))
	 (n 1)
	 message-count
	 (pop3-password pop3-password)
	 )
    ;; for debugging only
    (if pop3-debug (switch-to-buffer (process-buffer process)))
    ;; query for password
    (if (and pop3-password-required (not pop3-password))
	(setq pop3-password
	      (pop3-read-passwd (format "Password for %s: " pop3-maildrop))))
    (cond ((equal 'apop pop3-authentication-scheme)
	   (pop3-apop process pop3-maildrop))
	  ((equal 'pass pop3-authentication-scheme)
	   (pop3-user process pop3-maildrop)
	   (pop3-pass process))
	  (t (error "Invalid POP3 authentication scheme.")))
    (setq message-count (car (pop3-stat process)))
    (while (<= n message-count)
      (message (format "Retrieving message %d of %d from %s..."
		       n message-count pop3-mailhost))
      (pop3-retr process n crashbuf)
      (save-excursion
	(set-buffer crashbuf)
	(write-region-as-binary (point-min) (point-max) crashbox 'append)
	(set-buffer (process-buffer process))
	(while (> (buffer-size) 5000)
	  (goto-char (point-min))
	  (forward-line 50)
	  (delete-region (point-min) (point))))
      (pop3-dele process n)
      (setq n (+ 1 n))
      (if pop3-debug (sit-for 1) (sit-for 0.1))
      )
    (pop3-quit process)
    (kill-buffer crashbuf)
    )
  )

(defun pop3-open-server (mailhost port)
  "Open TCP connection to MAILHOST.
Returns the process associated with the connection."
  (let ((process-buffer
	 (get-buffer-create (format "trace of POP session to %s" mailhost)))
	(process))
    (save-excursion
      (set-buffer process-buffer)
      (erase-buffer)
      (setq pop3-read-point (point-min))
      )
    (setq process
	  (open-network-stream-as-binary "POP" process-buffer mailhost port))
    (let ((response (pop3-read-response process t)))
      (setq pop3-timestamp
	    (substring response (or (string-match "<" response) 0)
		       (+ 1 (or (string-match ">" response) -1)))))
    process))

;; Support functions

(defun pop3-process-filter (process output)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)))

(defun pop3-send-command (process command)
    (set-buffer (process-buffer process))
    (goto-char (point-max))
;;    (if (= (aref command 0) ?P)
;;	(insert "PASS <omitted>\r\n")
;;      (insert command "\r\n"))
    (setq pop3-read-point (point))
    (goto-char (point-max))
    (process-send-string process command)
    (process-send-string process "\r\n")
    )

(defun pop3-read-response (process &optional return)
  "Read the response from the server.
Return the response string if optional second argument is non-nil."
  (let ((case-fold-search nil)
	match-end)
    (save-excursion
      (set-buffer (process-buffer process))
      (goto-char pop3-read-point)
      (while (not (search-forward "\r\n" nil t))
	(accept-process-output process 3)
	(goto-char pop3-read-point))
      (setq match-end (point))
      (goto-char pop3-read-point)
      (if (looking-at "-ERR")
	  (signal 'error (list (buffer-substring (point) (- match-end 2))))
	(if (not (looking-at "+OK"))
	    (progn (setq pop3-read-point match-end) nil)
	  (setq pop3-read-point match-end)
	  (if return
	      (buffer-substring (point) match-end)
	    t)
	  )))))

(defvar pop3-read-passwd nil)
(defun pop3-read-passwd (prompt)
  (if (not pop3-read-passwd)
      (if (functionp 'read-passwd)
	  (setq pop3-read-passwd 'read-passwd)
	(if (load "passwd" t)
	    (setq pop3-read-passwd 'read-passwd)
	  (autoload 'ange-ftp-read-passwd "ange-ftp")
	  (setq pop3-read-passwd 'ange-ftp-read-passwd))))
  (funcall pop3-read-passwd prompt))

(defun pop3-clean-region (start end)
  (setq end (set-marker (make-marker) end))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end) (search-forward "\r\n" end t))
      (replace-match "\n" t t))
    (goto-char start)
    (while (and (< (point) end) (re-search-forward "^\\." end t))
      (replace-match "" t t)
      (forward-char)))
  (set-marker end nil))

(defun pop3-munge-message-separator (start end)
  "Check to see if a message separator exists.  If not, generate one."
  (if (not (fboundp 'parse-time-string))
      (autoload 'parse-time-string "parse-time"))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (not (or (looking-at "From .?") ; Unix mail
		   (looking-at "\001\001\001\001\n") ; MMDF
		   (looking-at "BABYL OPTIONS:") ; Babyl
		   ))
	  (let ((from (mail-strip-quoted-names (mail-fetch-field "From")))
		(date (mail-fetch-field "Date"))
		(From_))
	    ;; sample date formats I have seen
	    ;; Date: Tue, 9 Jul 1996 09:04:21 -0400 (EDT)
	    ;; Date: 08 Jul 1996 23:22:24 -0400
	    ;; should be
	    ;; Tue Jul 9 09:04:21 1996
	    (setq date (format-time-string
			"%a %b %e %T %Y"
			(if date
			    (apply 'encode-time (parse-time-string date))
			  (current-time))))
	    (setq From_ (format "\nFrom %s  %s\n" from date))
	    (while (string-match "," From_)
	      (setq From_ (concat (substring From_ 0 (match-beginning 0))
				  (substring From_ (match-end 0)))))
	    (goto-char (point-min))
	    (insert From_))))))

;; The Command Set

;; AUTHORIZATION STATE

(defun pop3-user (process user)
  "Send USER information to POP3 server."
  (pop3-send-command process (format "USER %s" user))
  (let ((response (pop3-read-response process t)))
    (if (not (and response (string-match "+OK" response)))
	(error (format "USER %s not valid." user)))))

(defun pop3-pass (process)
  "Send authentication information to the server."
  (pop3-send-command process (format "PASS %s" pop3-password))
  (let ((response (pop3-read-response process t)))
    (if (not (and response (string-match "+OK" response)))
	(pop3-quit process))))

(defun pop3-apop (process user)
  "Send alternate authentication information to the server."
  (if (not (fboundp 'md5)) (autoload 'md5 "md5"))
  (let ((hash (md5 (concat pop3-timestamp pop3-password))))
    (pop3-send-command process (format "APOP %s %s" user hash))
    (let ((response (pop3-read-response process t)))
      (if (not (and response (string-match "+OK" response)))
	  (pop3-quit process)))))

;; TRANSACTION STATE

(defun pop3-stat (process)
  "Return the number of messages in the maildrop and the maildrop's size."
  (pop3-send-command process "STAT")
  (let ((response (pop3-read-response process t)))
    (list (string-to-int (nth 1 (split-string response)))
	  (string-to-int (nth 2 (split-string response))))
    ))

(defun pop3-list (process &optional msg)
  "Scan listing of available messages.
This function currently does nothing.")

(defun pop3-retr (process msg crashbuf)
  "Retrieve message-id MSG to buffer CRASHBUF."
  (pop3-send-command process (format "RETR %s" msg))
  (pop3-read-response process)
  (let ((start pop3-read-point) end)
    (save-excursion
      (set-buffer (process-buffer process))
      (while (not (re-search-forward "^\\.\r\n" nil t))
	(accept-process-output process 3)
	;; bill@att.com ... to save wear and tear on the heap
	;; uncommented because the condensed version below is a problem for
	;; some.
	(if (> (buffer-size)  20000) (sleep-for 1))
	(if (> (buffer-size)  50000) (sleep-for 1))
	(if (> (buffer-size) 100000) (sleep-for 1))
	(if (> (buffer-size) 200000) (sleep-for 1))
	(if (> (buffer-size) 500000) (sleep-for 1))
	;; bill@att.com
	;; condensed into:
	;; (sometimes causes problems for really large messages.)
;	(if (> (buffer-size) 20000) (sleep-for (/ (buffer-size) 20000)))
	(goto-char start))
      (setq pop3-read-point (point-marker))
;; this code does not seem to work for some POP servers...
;; and I cannot figure out why not.
;      (goto-char (match-beginning 0))
;      (backward-char 2)
;      (if (not (looking-at "\r\n"))
;	  (insert "\r\n"))
;      (re-search-forward "\\.\r\n")
      (goto-char (match-beginning 0))
      (setq end (point-marker))
      (pop3-clean-region start end)
      (pop3-munge-message-separator start end)
      (save-excursion
	(set-buffer crashbuf)
	(erase-buffer))
      (copy-to-buffer crashbuf start end)
      (delete-region start end)
      )))

(defun pop3-dele (process msg)
  "Mark message-id MSG as deleted."
  (pop3-send-command process (format "DELE %s" msg))
  (pop3-read-response process))

(defun pop3-noop (process msg)
  "No-operation."
  (pop3-send-command process "NOOP")
  (pop3-read-response process))

(defun pop3-last (process)
  "Return highest accessed message-id number for the session."
  (pop3-send-command process "LAST")
  (let ((response (pop3-read-response process t)))
    (string-to-int (nth 1 (split-string response)))
    ))

(defun pop3-rset (process)
  "Remove all delete marks from current maildrop."
  (pop3-send-command process "RSET")
  (pop3-read-response process))

;; UPDATE

(defun pop3-quit (process)
  "Close connection to POP3 server.
Tell server to remove all messages marked as deleted, unlock the maildrop,
and close the connection."
  (pop3-send-command process "QUIT")
  (pop3-read-response process t)
  (if process
      (save-excursion
	(set-buffer (process-buffer process))
	(goto-char (point-max))
	(delete-process process))))

;; Summary of POP3 (Post Office Protocol version 3) commands and responses

;;; AUTHORIZATION STATE

;; Initial TCP connection
;; Arguments: none
;; Restrictions: none
;; Possible responses:
;;  +OK [POP3 server ready]

;; USER name
;; Arguments: a server specific user-id (required)
;; Restrictions: authorization state [after unsuccessful USER or PASS
;; Possible responses:
;;  +OK [valid user-id]
;;  -ERR [invalid user-id]

;; PASS string
;; Arguments: a server/user-id specific password (required)
;; Restrictions: authorization state, after successful USER
;; Possible responses:
;;  +OK [maildrop locked and ready]
;;  -ERR [invalid password]
;;  -ERR [unable to lock maildrop]

;;; TRANSACTION STATE

;; STAT
;; Arguments: none
;; Restrictions: transaction state
;; Possible responses:
;;  +OK nn mm [# of messages, size of maildrop]

;; LIST [msg]
;; Arguments: a message-id (optional)
;; Restrictions: transaction state; msg must not be deleted
;; Possible responses:
;;  +OK [scan listing follows]
;;  -ERR [no such message]

;; RETR msg
;; Arguments: a message-id (required)
;; Restrictions: transaction state; msg must not be deleted
;; Possible responses:
;;  +OK [message contents follow]
;;  -ERR [no such message]

;; DELE msg
;; Arguments: a message-id (required)
;; Restrictions: transaction state; msg must not be deleted
;; Possible responses:
;;  +OK [message deleted]
;;  -ERR [no such message]

;; NOOP
;; Arguments: none
;; Restrictions: transaction state
;; Possible responses:
;;  +OK

;; LAST
;; Arguments: none
;; Restrictions: transaction state
;; Possible responses:
;;  +OK nn [highest numbered message accessed]

;; RSET
;; Arguments: none
;; Restrictions: transaction state
;; Possible responses:
;;  +OK [all delete marks removed]

;;; UPDATE STATE

;; QUIT
;; Arguments: none
;; Restrictions: none
;; Possible responses:
;;  +OK [TCP connection closed]

;;; pop3.el --- Post Office Protocol (RFC 1460) interface

;; Copyright (C) 1996-1999 Free Software Foundation, Inc.

;; Author: Richard L. Pieri <ratinox@peorth.gweep.net>
;;      Daiki Ueno  <ueno@ueda.info.waseda.ac.jp>
;; Keywords: mail, pop3
;; Version: 1.3s

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

(defconst pop3-version "1.3s")

(defvar pop3-maildrop (or (user-login-name) (getenv "LOGNAME") (getenv "USER") nil)
  "*POP3 maildrop.")
(defvar pop3-mailhost (or (getenv "MAILHOST") nil)
  "*POP3 mailhost.")
(defvar pop3-port 110
  "*POP3 port.")
(defvar pop3-connection-type nil
  "*POP3 connection type.")

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

(defvar pop3-leave-mail-on-server nil
  "Non-nil if mail is to be left on the server and UIDL used for 
message retrieval.")

(defvar pop3-maximum-message-size nil
  "If non-nil only download messages smaller than this.")

(defvar pop3-uidl-file-name "~/.uidls"
  "File in which to store the UIDL of processed messages.")

(defvar pop3-uidl-support 'dont-know
  "Whether the server supports UIDL.
Nil means no, t means yes, not-nil-or-t means yet to be determined.")

(defvar pop3-uidl-obarray (make-vector 31 0)
  "Uidl hash table.")

(defvar pop3-read-point nil)
(defvar pop3-debug nil)

(eval-and-compile
  (autoload 'open-ssl-stream "ssl"))

(defvar pop3-ssl-program-arguments
  '("-quiet")
  "Arguments to be passed to the program `pop3-ssl-program-name'.")

(defun pop3-movemail (&optional crashbox)
  "Transfer contents of a maildrop to the specified CRASHBOX."
  (or crashbox (setq crashbox (expand-file-name "~/.crashbox")))
  (let* ((process (pop3-open-server pop3-mailhost pop3-port))
	 (crashbuf (get-buffer-create " *pop3-retr*"))
	 (n 1)
	 (msgid 1)
	 (msglen 0)
	 message-count
	 (pop3-password pop3-password)
	 (retrieved-messages nil)
	 messages)
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
    ;; get messages that are suitable for download
    (message "Retrieving message list...")
    (setq messages (pop3-get-message-numbers process)
	  message-count (length messages))
    (message (format "Retrieving message list...%d unread" message-count))
    (unwind-protect
	(progn
	  (while (<= n message-count)
	    (setq msgid (caar messages)
		  msglen (cdar messages)
		  messages (cdr messages))
	    ;; only retrieve messages matching our regexp or in the uidl list
	    (unless (or (not msgid)
			;; don't download messages that are too large
			(and pop3-maximum-message-size 
			     (> msglen pop3-maximum-message-size)))
	      (message (format "Retrieving message %d of %d from %s..."
			       n message-count pop3-mailhost))
	      (pop3-retr process msgid crashbuf)
	      (setq retrieved-messages (cons msgid retrieved-messages)))
	    (setq n (1+ n)))
	  (with-current-buffer crashbuf
	    (write-region-as-binary (point-min) (point-max)
				    crashbox 'append 'nomesg)
	    )
	  ;; mark messages as read
	  (when pop3-leave-mail-on-server
	    (pop3-save-uidls))
	  ;; now delete the messages we have retrieved
	  (unless pop3-leave-mail-on-server
	    (dolist (n retrieved-messages)
	      (message (format "Deleting message %d of %d from %s..."
			       n message-count pop3-mailhost))
	      (pop3-dele process n)))
	  )
      (pop3-quit process))
    (kill-buffer crashbuf)
    t))

(defun pop3-open-server (mailhost port)
  "Open TCP connection to MAILHOST.
Returns the process associated with the connection."
  (let ((process-buffer
	 (get-buffer-create (format "trace of POP session to %s" mailhost)))
	(process))
    (save-excursion
      (set-buffer process-buffer)
      (erase-buffer))
    (setq
     process
     (cond
      ((eq pop3-connection-type 'ssl)
       (pop3-open-ssl-stream "POP" process-buffer mailhost port))
      (t
       (open-network-stream-as-binary "POP" process-buffer mailhost port))))
    (setq pop3-read-point (point-min))
    (let ((response (pop3-read-response process t)))
      (setq pop3-timestamp
	    (substring response (or (string-match "<" response) 0)
		       (+ 1 (or (string-match ">" response) -1)))))
    process))

(defun pop3-open-ssl-stream-1 (name buffer host service extra-arg)
  (let* ((ssl-program-arguments 
	  `(,@pop3-ssl-program-arguments ,extra-arg
	    "-connect" ,(format "%s:%d" host service)))
         (process (open-ssl-stream name buffer host service)))
    (when process
      (with-current-buffer buffer
	(goto-char (point-min))
	(while (and (memq (process-status process) '(open run))
                    (goto-char (point-max))
                    (forward-line -1)
                    (not (looking-at "+OK")))
          (accept-process-output process 1)
          (sit-for 1))
	(delete-region (point-min) (point)))
      (and process (memq (process-status process) '(open run))
	   process))))

(defun pop3-open-ssl-stream (name buffer host service)
  "Open a SSL connection for a service to a host."
  (as-binary-process
   (or (pop3-open-ssl-stream-1 name buffer host service "-ssl3")
       (pop3-open-ssl-stream-1 name buffer host service "-ssl2"))))

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
    (process-send-string process (concat command "\r\n"))
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
    (while (re-search-forward "\n\n\\(From \\)" end t)
      (replace-match "\n\n>\\1" t nil))
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
			    (condition-case nil
				(apply 'encode-time (parse-time-string date))
			      (error (current-time)))
			  (current-time))))
	    (setq From_ (format "\nFrom %s  %s\n" from date))
	    (while (string-match "," From_)
	      (setq From_ (concat (substring From_ 0 (match-beginning 0))
				  (substring From_ (match-end 0)))))
	    (goto-char (point-min))
	    (insert From_))))))

;; UIDL support

(defun pop3-get-message-numbers (process)
  "Get the list of message numbers and lengths to retrieve via PROCESS."
  ;; we use the LIST comand first anyway to get the message lengths.
  ;; then if we're leaving mail on the server, see if the UIDL command
  ;; is implemented. if so, we use it to get the message number list.
  (let ((messages (pop3-list process))
	(uidl (if pop3-leave-mail-on-server
		  (pop3-get-uidl process))))
    (when messages
      (pop messages)
      (cond
       ((eq pop3-uidl-support t)
	;; remove elements not in the uidl, this assumes the uidl is short
	(remove-if-not
	 (lambda (message) (memq (car message) uidl))
	 (reverse messages)))
       (t messages)))))

(defun pop3-get-list (process)
  "Use PROCESS to get a list of message numbers."
  (let ((messages (pop3-list process)))
    (when messages
      (reverse messages))))

(defun pop3-get-uidl (process)
  "Use PROCESS to get a list of unread message numbers."
  (let ((messages (pop3-uidl process)) uidl)
    (if (or (null messages) (null pop3-uidl-support))
	(setq pop3-uidl-support nil)
      (setq pop3-uidl-support t)
      (save-excursion
	(with-temp-buffer
	  (when (file-readable-p pop3-uidl-file-name)
	    (insert-file-contents pop3-uidl-file-name))
	  (goto-char (point-min))
	  (while (looking-at "\\([^ \n\t]+\\)")
	    (set (intern (match-string 1) pop3-uidl-obarray)
		 (cons nil t))
	    (forward-line 1))))
      (dolist (message (cdr messages))
	(if (setq uidl (intern-soft (cdr message) pop3-uidl-obarray))
	    (setcar (symbol-value uidl) (car message))
	  (set (intern (cdr message) pop3-uidl-obarray)
	       (cons (car message) nil))))
      (pop3-get-unread-message-numbers))))
	    
(defun pop3-get-unread-message-numbers ()
  "Return a sorted list of unread msg numbers to retrieve."
  (let (nums)
    (mapatoms (lambda (atom)
		(if (not (cdr (symbol-value atom)))
		    (push (car (symbol-value atom)) nums)))
	      pop3-uidl-obarray)
    (sort nums '<)))

(defun pop3-save-uidls ()
  "Save the updated UIDLs to disk for use next time."
  (when (and pop3-leave-mail-on-server 
	     pop3-uidl-obarray
	     (catch 'found
	       (dotimes (i (length pop3-uidl-obarray))
		 (if (symbolp (aref pop3-uidl-obarray i))
		     (throw 'found t)))))
    (save-excursion
      (with-temp-buffer
        (when (file-readable-p pop3-uidl-file-name)
          (copy-file pop3-uidl-file-name
                     (concat pop3-uidl-file-name ".old")
                     'overwrite 'keeptime))
        (mapatoms 
	 (lambda (atom)
	   (when (car (symbol-value atom))
	     (insert (format "%s\n" atom))))
	 pop3-uidl-obarray)
        (write-file pop3-uidl-file-name)))))

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

(defun pop3-retr (process msg crashbuf)
  "Retrieve message-id MSG to buffer CRASHBUF."
  (pop3-send-command process (format "RETR %s" msg))
  (pop3-read-response process)
  (save-excursion
    (save-restriction
      (apply 'narrow-to-region (pop3-get-extended-response process))
      (pop3-munge-message-separator (point-min) (point-max))
      (append-to-buffer crashbuf (point-min) (point-max))
      (delete-region (point-min) (point-max)))))

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

(defun pop3-uidl (process &optional msgno)
  "Return the results of a UIDL command in PROCESS for optional MSGNO.
If UIDL is unsupported on this mail server or if msgno is invalid, return nil.
Otherwise, return a list in the form

   (N (1 UIDL-1) (2 UIDL-2) ... (N UIDL-N))

where

   N is an integer for the number of UIDLs returned (could be 0)
   UIDL-n is a string."

  (if msgno
      (pop3-send-command process (format "UIDL %d" msgno))
    (pop3-send-command process "UIDL"))
  
  (let ((uidl-not-supported 
	 (condition-case nil
	     (progn (pop3-read-response process t) nil)
	   (error t))))
    (unless uidl-not-supported
      (let (pairs uidl)
        (save-excursion
	  (save-restriction
	    (apply 'narrow-to-region (pop3-get-extended-response process))
	    (goto-char (point-min))
	    (while (looking-at "\\([^ \n\t]*\\) \\([^ \n\t]*\\)")
	      (setq msgno (string-to-int (match-string 1))
		    uidl (match-string 2))
	      (push (cons msgno uidl) pairs)
	      (beginning-of-line 2))
	    (cons (length pairs) (nreverse pairs))))))))

(defun pop3-list (process &optional msgno)
  "Return the results of a LIST command for PROCESS and optional MSGNO.
If (optional) msgno is invalid, return nil.  Otherwise, return a list
in the form

   (N (1 LEN-1) (2 LEN-2) ... (N LEN-N))

where

   N is an integer for the number of msg/len pairs (could be 0)
   LEN-n is an integer."
  (if msgno
      (pop3-send-command process (format "LIST %d" msgno))
    (pop3-send-command process "LIST"))

  (let ((bad-msgno 
	 (condition-case nil
	     (progn (pop3-read-response process t) nil)
	   (error t))))
    (unless bad-msgno
      (let (pairs len)
	(save-excursion
	  (save-restriction
	    (apply 'narrow-to-region (pop3-get-extended-response process))
	    (goto-char (point-min))
	    (while (looking-at "\\([^ \n\t]*\\) \\([^ \n\t]*\\)")
	      (setq msgno (string-to-int (match-string 1))
		    len (string-to-int (match-string 2)))
	      (push (cons msgno len) pairs)
	      (beginning-of-line 2))
	    (cons (length pairs) (nreverse pairs))))))))

;;; Utility code

(defun pop3-get-extended-response (process)
  "Get the extended pop3 response in the PROCESS buffer."
  (let ((start pop3-read-point) end)
    (set-buffer (process-buffer process))
    (while (not (re-search-forward "^\\.\r\n" nil t))
      (accept-process-output process)
      (goto-char start))
    (setq pop3-read-point (point-marker))
    (goto-char (match-beginning 0))
    (setq end (point-marker))
    (pop3-clean-region start end)
    (list start end)))


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

;; UIDL [msg]
;; Arguments: a message-id (optional)
;; Restrictions: transaction state; msg must not be deleted
;; Possible responses:
;;  +OK [uidl listing follows]
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

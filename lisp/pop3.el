;;; pop3.el --- Post Office Protocol (RFC 1460) interface

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003
;;        Free Software Foundation, Inc.

;; Author: Richard L. Pieri <ratinox@peorth.gweep.net>
;;      Daiki Ueno  <ueno@ueda.info.waseda.ac.jp>
;;      Katsumi Yamaoka <yamaoka@jpl.org>
;; Maintainer: Volunteers
;; Keywords: mail

;; This file is part of T-gnus.

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

;; You have to set the variable `pop3-connection-type' to `ssl' or
;; `tls' expressly, if you would like to use this module with Gnus
;; (not T-gnus) for those connection types.  For examples:
;;
;;(setq mail-sources '((pop :server "POPSERVER" :port 995 :connection ssl
;;                          :authentication apop)))
;;(setq pop3-connection-type 'ssl)

;;; Code:

(eval-when-compile
  (require 'cl)
  ;; For compiling this module in Gnus with XEmacs -no-autoloads.
  (require 'advice))

;; as-binary-process, open-network-stream-as-binary, write-region-as-binary
(require 'pces)
;; exec-installed-p
(require 'path-util)

(require 'mail-utils)

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
  "Non-nil if mail is to be left on the server and UIDL used for message retrieval.")

(defvar pop3-maximum-message-size nil
  "If non-nil only download messages smaller than this.")

(defvar pop3-except-header-regexp nil
  "If non-nil we do not retrieve messages whose headers are matching this regexp.")

(defvar pop3-uidl-file-name "~/.uidls"
  "File in which to store the UIDL of processed messages.")

(defvar pop3-uidl-support nil
  "Alist of servers and flags of whether they support UIDLs.
Users don't have to set this value.")

(defvar pop3-uidl-obarray (make-vector 31 0)
  "Uidl hash table.")

(defvar pop3-read-point nil)
(defvar pop3-debug nil)

(eval-and-compile
  (autoload 'starttls-open-stream "starttls")
  (autoload 'starttls-negotiate "starttls"))

(defvar pop3-ssl-program-name
  (if (exec-installed-p "openssl")
      "openssl"
    "ssleay")
  "The program to run in a subprocess to open an SSL connection.")

(defvar pop3-ssl-program-arguments
  '("s_client" "-quiet")
  "Arguments to be passed to the program `pop3-ssl-program-name'.")

(defun pop3-progress-message (format percent &rest args)
  (apply (function message) format args))

(defun pop3-movemail (&optional crashbox)
  "Transfer contents of a maildrop to the specified CRASHBOX."
  (or crashbox (setq crashbox (expand-file-name "~/.crashbox")))
  (let* ((process (pop3-open-server pop3-mailhost pop3-port))
	 (crashbuf (get-buffer-create " *pop3-retr*"))
	 (n 1)
	 message-count
	 (pop3-password pop3-password)
	 (pop3-uidl-file-name (convert-standard-filename
			       (concat pop3-uidl-file-name "-"
				       pop3-mailhost)))
	 retrieved-messages messages)
    ;; for debugging only
    (if pop3-debug (switch-to-buffer (process-buffer process)))
    ;; query for password
    (if (and pop3-password-required (not pop3-password))
	(setq pop3-password
	      (read-passwd (format "Password for %s: " pop3-maildrop))))
    (cond ((equal 'apop pop3-authentication-scheme)
	   (pop3-apop process pop3-maildrop))
	  ((equal 'pass pop3-authentication-scheme)
	   (pop3-user process pop3-maildrop)
	   (pop3-pass process))
	  (t (error "Invalid POP3 authentication scheme")))
    ;; get messages that are suitable for download
    (message "Retrieving message list...")
    (setq messages (pop3-get-message-numbers process)
	  message-count (length (cdr messages)))
    (message "Retrieving message list...%d of %d unread"
	     message-count (pop messages))
    (unwind-protect
	(unless (not (stringp crashbox))
	  (while messages
	    (pop3-progress-message
	     "Retrieving message %d of %d (%d octets) from %s..."
	     (floor (* (/ (float n) message-count) 100))
	     n message-count (cdar messages) pop3-mailhost)
	    (pop3-retr process (caar messages) crashbuf)
	    (push (caar messages) retrieved-messages)
	    (setq messages (cdr messages)
		  n (1+ n)))
	  (with-current-buffer crashbuf
	    (write-region-as-binary (point-min) (point-max)
				    crashbox 'append 'nomesg))
	  ;; mark messages as read
	  (when pop3-leave-mail-on-server
	    (pop3-save-uidls))
	  ;; now delete the messages we have retrieved
	  (unless pop3-leave-mail-on-server
	    (dolist (n retrieved-messages)
	      (message "Deleting message %d of %d from %s..."
		       n message-count pop3-mailhost)
	      (pop3-dele process n)))
	  )
      (pop3-quit process))
    (kill-buffer crashbuf)
    message-count))

(defun pop3-get-message-count ()
  "Return the number of messages in the maildrop."
  (let* ((process (pop3-open-server pop3-mailhost pop3-port))
	 message-count
	 (pop3-password pop3-password)
	 )
    ;; for debugging only
    (if pop3-debug (switch-to-buffer (process-buffer process)))
    ;; query for password
    (if (and pop3-password-required (not pop3-password))
	(setq pop3-password
	      (read-passwd (format "Password for %s: " pop3-maildrop))))
    (cond ((equal 'apop pop3-authentication-scheme)
	   (pop3-apop process pop3-maildrop))
	  ((equal 'pass pop3-authentication-scheme)
	   (pop3-user process pop3-maildrop)
	   (pop3-pass process))
	  (t (error "Invalid POP3 authentication scheme")))
    (setq message-count (car (pop3-stat process)))
    (pop3-quit process)
    message-count))

(defun pop3-open-server (mailhost port)
  "Open TCP connection to MAILHOST on PORT.
Returns the process associated with the connection.
Argument PORT specifies connecting port."
  (let (process)
    (save-excursion
      (set-buffer (get-buffer-create (concat " trace of POP session to "
					     mailhost)))
      (erase-buffer)
      (setq pop3-read-point (point-min))
      (setq
       process
       (cond
	((eq pop3-connection-type 'ssl)
	 (pop3-open-ssl-stream "POP" (current-buffer) mailhost port))
	((eq pop3-connection-type 'tls)
	 (pop3-open-tls-stream "POP" (current-buffer) mailhost port))
	(t
	 (open-network-stream-as-binary "POP" (current-buffer)
					mailhost port))))
      (let ((response (pop3-read-response process t)))
	(setq pop3-timestamp
	      (substring response (or (string-match "<" response) 0)
			 (+ 1 (or (string-match ">" response) -1)))))
      process)))

(defun pop3-open-ssl-stream-1 (name buffer host service extra-arg)
  (require 'ssl)
  (let* ((ssl-program-name
	  pop3-ssl-program-name)
	 (ssl-program-arguments
	  `(,@pop3-ssl-program-arguments
	    ,extra-arg
	    "-connect" ,(format "%s:%d" host service)))
	 (process (open-ssl-stream name buffer host service)))
    (when process
      (with-current-buffer buffer
	(goto-char (point-min))
	(while (and (memq (process-status process) '(open run))
		    (goto-char (point-max))
		    (forward-line -1)
		    (not (looking-at "+OK")))
	  (nnheader-accept-process-output process)
	  (sit-for 1))
	(delete-region (point-min) (point)))
      (and process (memq (process-status process) '(open run))
	   process))))

(defun pop3-open-ssl-stream (name buffer host service)
  "Open a SSL connection for a service to a host.
Returns a subprocess-object to represent the connection.
Args are NAME BUFFER HOST SERVICE."
  (as-binary-process
   (or (pop3-open-ssl-stream-1 name buffer host service "-ssl3")
       (pop3-open-ssl-stream-1 name buffer host service "-ssl2"))))

(defun pop3-open-tls-stream (name buffer host service)
  "Open a TLSv1 connection for a service to a host.
Returns a subprocess-object to represent the connection.
Args are NAME BUFFER HOST SERVICE."
  (let ((process
	 (as-binary-process (starttls-open-stream
			     name buffer host service))))
    (pop3-stls process)
    (starttls-negotiate process)
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
;;  (if (= (aref command 0) ?P)
;;      (insert "PASS <omitted>\r\n")
;;    (insert command "\r\n"))
  (setq pop3-read-point (point))
  (goto-char (point-max))
  (process-send-string process (concat command "\r\n"))
  )

(defun pop3-read-response (process &optional return)
  "Read the response from the server PROCESS.
Return the response string if optional second argument RETURN is non-nil."
  (let ((case-fold-search nil)
	match-end)
    (save-excursion
      (set-buffer (process-buffer process))
      (goto-char pop3-read-point)
      (while (not (search-forward "\r\n" nil t))
	(nnheader-accept-process-output process)
	(goto-char pop3-read-point))
      (setq match-end (point))
      (goto-char pop3-read-point)
      (if (looking-at "-ERR")
	  (error (buffer-substring (point) (- match-end 2)))
	(if (not (looking-at "+OK"))
	    (progn (setq pop3-read-point match-end) nil)
	  (setq pop3-read-point match-end)
	  (if return
	      (buffer-substring (point) match-end)
	    t)
	  )))))

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

(eval-when-compile (defvar parse-time-months))

;; Copied from message-make-date.
(defun pop3-make-date (&optional now)
  "Make a valid date header.
If NOW, use that time instead."
  (require 'parse-time)
  (let* ((now (or now (current-time)))
	 (zone (nth 8 (decode-time now)))
	 (sign "+"))
    (when (< zone 0)
      (setq sign "-")
      (setq zone (- zone)))
    (concat
     (format-time-string "%d" now)
     ;; The month name of the %b spec is locale-specific.  Pfff.
     (format " %s "
	     (capitalize (car (rassoc (nth 4 (decode-time now))
				      parse-time-months))))
     (format-time-string "%Y %H:%M:%S " now)
     ;; We do all of this because XEmacs doesn't have the %z spec.
     (format "%s%02d%02d" sign (/ zone 3600) (/ (% zone 3600) 60)))))

(defun pop3-munge-message-separator (start end)
  "Check to see if a message separator exists.  If not, generate one."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (not (or (looking-at "From .?") ; Unix mail
		   (looking-at "\001\001\001\001\n") ; MMDF
		   (looking-at "BABYL OPTIONS:") ; Babyl
		   ))
	  (let* ((from (mail-strip-quoted-names (mail-fetch-field "From")))
		 (tdate (mail-fetch-field "Date"))
		 (date (split-string (or (and tdate
					      (not (string= "" tdate))
					      tdate)
					 (pop3-make-date))
				     " "))
		 (From_))
	    ;; sample date formats I have seen
	    ;; Date: Tue, 9 Jul 1996 09:04:21 -0400 (EDT)
	    ;; Date: 08 Jul 1996 23:22:24 -0400
	    ;; should be
	    ;; Tue Jul 9 09:04:21 1996
	    (setq date
		  (cond ((not date)
			 "Tue Jan 1 00:00:0 1900")
			((string-match "[A-Z]" (nth 0 date))
			 (format "%s %s %s %s %s"
				 (nth 0 date) (nth 2 date) (nth 1 date)
				 (nth 4 date) (nth 3 date)))
			(t
			 ;; this really needs to be better but I don't feel
			 ;; like writing a date to day converter.
			 (format "Sun %s %s %s %s"
				 (nth 1 date) (nth 0 date)
				 (nth 3 date) (nth 2 date)))
			))
	    (setq From_ (format "\nFrom %s  %s\n" from date))
	    (while (string-match "," From_)
	      (setq From_ (concat (substring From_ 0 (match-beginning 0))
				  (substring From_ (match-end 0)))))
	    (goto-char (point-min))
	    (insert From_)
	    (if (search-forward "\n\n" nil t)
		nil
	      (goto-char (point-max))
	      (insert "\n"))
	    (narrow-to-region (point) (point-max))
	    (let ((size (- (point-max) (point-min))))
	      (goto-char (point-min))
	      (widen)
	      (forward-line -1)
	      (insert (format "Content-Length: %s\n" size)))
	    )))))

;; UIDL support

(defun pop3-get-message-numbers (process)
  "Get the list of message numbers and lengths to retrieve via PROCESS."
  ;; we use the LIST comand first anyway to get the message lengths.
  ;; then if we're leaving mail on the server, see if the UIDL command
  ;; is implemented. if so, we use it to get the message number list.
  (let* ((messages (pop3-list process))
	 (total (or (pop messages) 0))
	 (uidl (if pop3-leave-mail-on-server
		   (pop3-get-uidl process)))
	 out)
    (while messages
      ;; only retrieve messages matching our regexp or in the uidl list
      (when (and
	     ;; remove elements not in the uidl, this assumes the uidl is short
	     (or (not (and pop3-leave-mail-on-server
			   (cdr (assoc pop3-mailhost pop3-uidl-support))))
		 (memq (caar messages) uidl))
	     (caar messages)
	     ;; don't download messages that are too large
	     (not (and pop3-maximum-message-size
		       (> (cdar messages) pop3-maximum-message-size)))
	     (not (and pop3-except-header-regexp
		       (string-match pop3-except-header-regexp
				     (pop3-top process (caar messages) 0)))))
	(push (car messages) out))
      (setq messages (cdr messages)))
    (cons total (nreverse out))))

(defun pop3-get-uidl (process)
  "Use PROCESS to get a list of unread message numbers."
  (let ((messages (pop3-uidl process))
	(support (assoc pop3-mailhost pop3-uidl-support))
	uidl)
    (if support
	(setcdr support (and messages t))
      (push (cons pop3-mailhost (and messages t))
	    pop3-uidl-support))
    (when messages
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
	     ;; UIDL hash table is non-empty
	     (let ((len (length pop3-uidl-obarray)))
	       (while (< 0 len)
		 (setq len (if (symbolp (aref pop3-uidl-obarray (1- len)))
			       -1 (1- len))))
	       (minusp len)))
    (when (file-readable-p pop3-uidl-file-name)
      (copy-file pop3-uidl-file-name
		 (concat pop3-uidl-file-name ".old")
		 'overwrite 'keeptime))
    (save-excursion
      (with-temp-file pop3-uidl-file-name
	(mapatoms
	 (lambda (atom)
	   (when (car (symbol-value atom))
	     (insert (format "%s\n" atom))))
	 pop3-uidl-obarray)))
    (fillarray pop3-uidl-obarray 0)))


;; The Command Set

;; AUTHORIZATION STATE

(defun pop3-user (process user)
  "Send USER information to POP3 server."
  (pop3-send-command process (format "USER %s" user))
  (let ((response (pop3-read-response process t)))
    (if (not (and response (string-match "+OK" response)))
	(error (format "USER %s not valid" user)))))

(defun pop3-pass (process)
  "Send authentication information to the server."
  (pop3-send-command process (format "PASS %s" pop3-password))
  (let ((response (pop3-read-response process t)))
    (if (not (and response (string-match "+OK" response)))
	(pop3-quit process))))

;; When this file is being compiled in the Gnus (not T-gnus) source
;; tree, `md5' might have been defined in w3/md5.el, ./lpath.el or one
;; of some other libraries and `md5' will accept only 3 arguments.  We
;; will deceive the byte-compiler not to say warnings.
(eval-and-compile
  (if (fboundp 'eval-when)
      ;; `eval-when' might not be provided when loading .el file.
      (eval-when 'compile
	(let ((def (assq 'md5 byte-compile-function-environment)))
	  (if def
	      (setcdr def '(lambda (object &optional start end
					   coding-system noerror)))
	    (setq byte-compile-function-environment
		  (cons '(md5 . (lambda (object &optional start end
						coding-system noerror)))
			byte-compile-function-environment)))))))

;; Note that `pop3-md5' should never encode a given string to use for
;; the apop authentication.
(eval-and-compile
  (if (fboundp 'md5)
      (if (condition-case nil
	      (md5 "\
Check whether the 4th argument CODING-SYSTEM is allowed"
		   nil nil 'binary)
	    (error nil))
	  ;; Emacs 21 or XEmacs 21
	  ;; (md5 OBJECT &optional START END CODING-SYSTEM NOERROR)
	  (defun pop3-md5 (string)
	    (md5 string nil nil 'binary))
	;; The reason why the program reaches here:
	;; 1. XEmacs 20 is running and the built-in `md5' doesn't
	;;    allow the 4th argument.
	;; 2. `md5' has been defined by one of some lisp libraries.
	;; 3. This file is being compiled in the Gnus source tree,
	;;    and `md5' has been defined in lpath.el.
	(defalias 'pop3-md5 'md5))
    ;; The lisp function will be provided by FLIM or other libraries.
    (autoload 'md5 "md5")
    (defalias 'pop3-md5 'md5)))

(defun pop3-apop (process user)
  "Send alternate authentication information to the server."
  (let ((pass pop3-password))
    (if (and pop3-password-required (not pass))
	(setq pass
	      (read-passwd (format "Password for %s: " pop3-maildrop))))
    (if pass
	(let ((hash (pop3-md5 (concat pop3-timestamp pass))))
	  (pop3-send-command process (format "APOP %s %s" user hash))
	  (let ((response (pop3-read-response process t)))
	    (if (not (and response (string-match "+OK" response)))
		(pop3-quit process)))))
    ))

(defun pop3-stls (process)
  "Query whether TLS extension is supported"
  (pop3-send-command process "STLS")
  (let ((response (pop3-read-response process t)))
    (if (not (and response (string-match "+OK" response)))
	(pop3-quit process))))

;; TRANSACTION STATE

(defun pop3-stat (process)
  "Return the number of messages in the maildrop and the maildrop's size."
  (pop3-send-command process "STAT")
  (let ((response (pop3-read-response process t)))
    (list (string-to-int (nth 1 (split-string response " ")))
	  (string-to-int (nth 2 (split-string response " "))))
    ))

(defun pop3-retr (process msg crashbuf)
  "Retrieve message-id MSG to buffer CRASHBUF."
  (pop3-send-command process (format "RETR %s" msg))
  (pop3-read-response process)
  (save-excursion
    (let ((region (pop3-get-extended-response process)))
      (pop3-munge-message-separator (car region) (cadr region))
      (append-to-buffer crashbuf (car region) (cadr region))
      (delete-region (car region) (cadr region))
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
    (string-to-int (nth 1 (split-string response " ")))
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
  (when process
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

  (if (null (pop3-read-response process t))
      nil ;; UIDL is not supported on this server
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
	  (cons (length pairs) (nreverse pairs))
	  )))))

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

  (if (null (pop3-read-response process t))
      nil ;; MSGNO is not valid number
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
	  (cons (length pairs) (nreverse pairs))
	  )))))

(defun pop3-top (process msgno &optional lines)
  "Return the top LINES of messages for PROCESS and MSGNO.
If msgno is invalid, return nil.  Otherwise, return a string."
  (pop3-send-command process (format "TOP %d %d" msgno (or lines 1)))
  (if (pop3-read-response process t)
      nil ;; MSGNO is not valid number
    (save-excursion
      (apply 'buffer-substring (pop3-get-extended-response process)))
    ))

;;; Utility code

(defun pop3-get-extended-response (process)
  "Get the extended pop3 response in the PROCESS buffer."
  (let ((start pop3-read-point) end)
    (set-buffer (process-buffer process))
    (goto-char start)
    (while (not (re-search-forward "^\\.\r\n" nil t))
      (nnheader-accept-process-output process)
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

;; STLS
;; Arguments: none
;; Restrictions: authorization state
;; Possible responses:
;;  +OK [negotiation is ready]
;;  -ERR [security layer is already active]

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

;; TOP msg [lines]
;; Arguments: a message-id (required), number of lines (optional)
;; Restrictions: transaction state; msg must not be deleted
;; Possible responses:
;;  +OK [partial message listing follows]
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

(provide 'pop3)

;;; pop3.el ends here

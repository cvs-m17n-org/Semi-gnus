;;; smtpmail.el --- simple SMTP protocol (RFC 821) for sending mail

;; Copyright (C) 1995, 1996 Free Software Foundation, Inc.

;; Author: Tomoji Kagatani <kagatani@rbc.ncl.omron.co.jp>
;; Maintainer: Brian D. Carlstrom <bdc@ai.mit.edu>
;; ESMTP support: Simon Leinen <simon@switch.ch>
;; Keywords: mail

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

;; Send Mail to smtp host from smtpmail temp buffer.

;; Please add these lines in your .emacs(_emacs).
;;
;;(setq send-mail-function 'smtpmail-send-it)
;;(setq smtp-default-server "YOUR SMTP HOST")
;;(setq smtp-service "smtp")
;;(setq smtp-local-domain "YOUR DOMAIN NAME")
;;(setq smtp-debug-info t)
;;(autoload 'smtpmail-send-it "smtpmail")
;;(setq user-full-name "YOUR NAME HERE")

;; To queue mail, set smtpmail-queue-mail to t and use 
;; smtpmail-send-queued-mail to send.


;;; Code:

(require 'smtp)
(require 'sendmail)
(require 'time-stamp)

;;;

(defcustom smtpmail-queue-mail nil 
  "*Specify if mail is queued (if t) or sent immediately (if nil).
If queued, it is stored in the directory `smtpmail-queue-dir'
and sent with `smtpmail-send-queued-mail'."
  :type 'boolean
  :group 'smtp)

(defcustom smtpmail-queue-dir "~/Mail/queued-mail/"
  "*Directory where `smtpmail.el' stores queued mail."
  :type 'directory
  :group 'smtp)

(defvar smtpmail-queue-index-file "index"
  "File name of queued mail index,
This is relative to `smtpmail-queue-dir'.")

(defvar smtpmail-queue-index (concat smtpmail-queue-dir
				     smtpmail-queue-index-file))

(defvar smtpmail-recipient-address-list nil)


;;;
;;;
;;;

(defun smtpmail-send-it ()
  (require 'mail-utils)
  (let ((errbuf (if mail-interactive
		    (generate-new-buffer " smtpmail errors")
		  0))
	(tembuf (generate-new-buffer " smtpmail temp"))
	(case-fold-search nil)
	resend-to-addresses
	delimline
	(mailbuf (current-buffer)))
    (unwind-protect
	(save-excursion
	  (set-buffer tembuf)
	  (erase-buffer)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;; Change header-delimiter to be what sendmail expects.
	  (goto-char (point-min))
	  (re-search-forward
	    (concat "^" (regexp-quote mail-header-separator) "\n"))
	  (replace-match "\n")
	  (backward-char 1)
	  (setq delimline (point-marker))
;;	  (sendmail-synch-aliases)
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (let ((case-fold-search t))
	    (goto-char (point-min))
	    (goto-char (point-min))
	    (while (re-search-forward "^Resent-to:" delimline t)
	      (setq resend-to-addresses
		    (save-restriction
		      (narrow-to-region (point)
					(save-excursion
					  (end-of-line)
					  (point)))
		      (append (mail-parse-comma-list)
			      resend-to-addresses))))
;;; Apparently this causes a duplicate Sender.
;;;	    ;; If the From is different than current user, insert Sender.
;;;	    (goto-char (point-min))
;;;	    (and (re-search-forward "^From:"  delimline t)
;;;		 (progn
;;;		   (require 'mail-utils)
;;;		   (not (string-equal
;;;			 (mail-strip-quoted-names
;;;			  (save-restriction
;;;			    (narrow-to-region (point-min) delimline)
;;;			    (mail-fetch-field "From")))
;;;			 (user-login-name))))
;;;		 (progn
;;;		   (forward-line 1)
;;;		   (insert "Sender: " (user-login-name) "\n")))
	    ;; Don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:[ \t]*\n" delimline t)
		(replace-match ""))
	    ;; Put the "From:" field in unless for some odd reason
	    ;; they put one in themselves.
	    (goto-char (point-min))
	    (if (not (re-search-forward "^From:" delimline t))
		(let* ((login user-mail-address)
		       (fullname (user-full-name)))
		  (cond ((eq mail-from-style 'angles)
			 (insert "From: " fullname)
			 (let ((fullname-start (+ (point-min) 6))
			       (fullname-end (point-marker)))
			   (goto-char fullname-start)
			   ;; Look for a character that cannot appear unquoted
			   ;; according to RFC 822.
			   (if (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
						  fullname-end 1)
			       (progn
				 ;; Quote fullname, escaping specials.
				 (goto-char fullname-start)
				 (insert "\"")
				 (while (re-search-forward "[\"\\]"
							   fullname-end 1)
				   (replace-match "\\\\\\&" t))
				 (insert "\""))))
			 (insert " <" login ">\n"))
			((eq mail-from-style 'parens)
			 (insert "From: " login " (")
			 (let ((fullname-start (point)))
			   (insert fullname)
			   (let ((fullname-end (point-marker)))
			     (goto-char fullname-start)
			     ;; RFC 822 says \ and nonmatching parentheses
			     ;; must be escaped in comments.
			     ;; Escape every instance of ()\ ...
			     (while (re-search-forward "[()\\]" fullname-end 1)
			       (replace-match "\\\\\\&" t))
			     ;; ... then undo escaping of matching parentheses,
			     ;; including matching nested parentheses.
			     (goto-char fullname-start)
			     (while (re-search-forward 
				     "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
				     fullname-end 1)
			       (replace-match "\\1(\\3)" t)
			       (goto-char fullname-start))))
			 (insert ")\n"))
			((null mail-from-style)
			 (insert "From: " login "\n")))))
	    ;; Insert an extra newline if we need it to work around
	    ;; Sun's bug that swallows newlines.
	    (goto-char (1+ delimline))
	    (if (eval mail-mailer-swallows-blank-line)
		(newline))
	    ;; Find and handle any FCC fields.
	    (goto-char (point-min))
	    (if (re-search-forward "^FCC:" delimline t)
		(mail-do-fcc delimline))
	    (if mail-interactive
		(save-excursion
		  (set-buffer errbuf)
		  (erase-buffer))))
	  ;;
	  ;;
	  ;;
	  (setq smtpmail-recipient-address-list
		(or resend-to-addresses
		    (smtp-deduce-address-list tembuf (point-min) delimline)))

	  (smtp-do-bcc delimline)
	  ; Send or queue
	  (if (not smtpmail-queue-mail)
	      (if smtpmail-recipient-address-list
		  (if (not (smtp-via-smtp
			    smtpmail-recipient-address-list tembuf))
		      (error "Sending failed; SMTP protocol error"))
		(error "Sending failed; no recipients"))
	    (let* ((file-data (concat 
			       smtpmail-queue-dir
			       (time-stamp-strftime 
				"%02y%02m%02d-%02H%02M%02S")))
		   (file-elisp (concat file-data ".el"))
		   (buffer-data (create-file-buffer file-data))
		   (buffer-elisp (create-file-buffer file-elisp))
		   (buffer-scratch "*queue-mail*"))
	      (save-excursion
		(set-buffer buffer-data)
		(erase-buffer)
		(insert-buffer tembuf)
		(write-file file-data)
		(set-buffer buffer-elisp)
		(erase-buffer)
		(insert (concat
			 "(setq smtpmail-recipient-address-list '"
			 (prin1-to-string smtpmail-recipient-address-list)
			 ")\n"))	    	    
		(write-file file-elisp)
		(set-buffer (generate-new-buffer buffer-scratch))
		(insert (concat file-data "\n"))
		(append-to-file (point-min) 
				(point-max) 
				smtpmail-queue-index)
		)
	      (kill-buffer buffer-scratch)
	      (kill-buffer buffer-data)
	      (kill-buffer buffer-elisp))))
      (kill-buffer tembuf)
      (if (bufferp errbuf)
	  (kill-buffer errbuf)))))

(defun smtpmail-send-queued-mail ()
  "Send mail that was queued as a result of setting `smtpmail-queue-mail'."
  (interactive)
  ;;; Get index, get first mail, send it, get second mail, etc...
  (let ((buffer-index (find-file-noselect smtpmail-queue-index))
	(file-msg "")
	(tembuf nil))
    (save-excursion
      (set-buffer buffer-index)
      (beginning-of-buffer)
      (while (not (eobp))
	(setq file-msg (buffer-substring (point) (save-excursion
						   (end-of-line)
						   (point))))
	(load file-msg)
	(setq tembuf (find-file-noselect file-msg))
	(if smtpmail-recipient-address-list
	    (if (not (smtp-via-smtp smtpmail-recipient-address-list tembuf))
		(error "Sending failed; SMTP protocol error"))
	  (error "Sending failed; no recipients"))  
	(delete-file file-msg)
	(delete-file (concat file-msg ".el"))
	(kill-buffer tembuf)
	(kill-line 1))      
      (set-buffer buffer-index)
      (save-buffer smtpmail-queue-index)
      (kill-buffer buffer-index)
      )))


;;;

(provide 'smtpmail)

;;; smtpmail.el ends here

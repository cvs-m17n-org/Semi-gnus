;;; gnus-delay.el --- Delayed posting of articles -*- coding: latin-1; -*-

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: Kai Gro�johann <Kai.Grossjohann@CS.Uni-Dortmund.DE>
;; Keywords: mail, news, extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provide delayed posting of articles.

;;; Code:

(require 'nndraft)
(require 'gnus-draft)

(defvar gnus-delay-group "delayed"
  "Group name for storing delayed articles.")

(defvar gnus-delay-header "X-Gnus-Delayed"
  "Header name for storing info about delayed articles.")

(defvar gnus-delay-default-delay "3d"
  "*Default length of delay.")

(defvar gnus-delay-default-hour 8
  "*If deadline is given as date, then assume this time of day.")

(defun gnus-delay-article (delay)
  "Delay this article by some time.
DELAY is a string, giving the length of the time.  Possible values are:

* <digits><units> for <units> in minutes (`m'), hours (`h'), days (`d'),
  weeks (`w'), months (`M'), or years (`Y');

* YYYY-MM-DD for a specific date.  The time of day is given by the
  variable `gnus-delay-default-hour', minute and second are zero.

* hh:mm for a specific time.  Use 24h format.  If it is later than this
  time, then the deadline is tomorrow, else today."
  (interactive
   (list (read-string
	  "Target date (YYYY-MM-DD) or length of delay (units in [mhdwMY]): "
	  gnus-delay-default-delay)))
  (let (num unit days year month day hour minute deadline)
    (cond ((string-match
	    "\\([0-9][0-9][0-9]?[0-9]?\\)-\\([0-9]+\\)-\\([0-9]+\\)"
	    delay)
	   (setq year  (string-to-number (match-string 1 delay))
		 month (string-to-number (match-string 2 delay))
		 day   (string-to-number (match-string 3 delay)))
	   (setq deadline
		 (message-make-date
		  (encode-time 0 0      ; second and minute
			       gnus-delay-default-hour
			       day month year))))
	  ((string-match "\\([0-9]+\\):\\([0-9]+\\)" delay)
	   (setq hour   (string-to-number (match-string 1 delay))
		 minute (string-to-number (match-string 2 delay)))
	   ;; Use current time, except...
	   (setq deadline (apply 'vector (decode-time (current-time))))
	   ;; ... for minute and hour.
	   (aset deadline 1 minute)
	   (aset deadline 2 hour)
	   ;; Convert to seconds.
	   (setq deadline (time-to-seconds (apply 'encode-time
						  (append deadline nil))))
	   ;; If this time has passed already, add a day.
	   (when (< deadline (time-to-seconds (current-time)))
	     (setq deadline (+ 3600 deadline))) ;3600 secs/day
	   ;; Convert seconds to date header.
	   (setq deadline (message-make-date
			   (seconds-to-time deadline))))
	  ((string-match "\\([0-9]+\\)\\s-*\\([mhdwMY]\\)" delay)
	   (setq num (match-string 1 delay))
	   (setq unit (match-string 2 delay))
	   ;; Start from seconds, then multiply into needed units.
	   (setq num (string-to-number num))
	   (cond ((string= unit "Y")
		  (setq delay (* num 60 60 24 365)))
		 ((string= unit "M")
		  (setq delay (* num 60 60 24 30)))
		 ((string= unit "w")
		  (setq delay (* num 60 60 24 7)))
		 ((string= unit "d")
		  (setq delay (* num 60 60 24)))
		 ((string= unit "h")
		  (setq delay (* num 60 60)))
		 (t
		  (setq delay (* num 60))))
	   (setq deadline (message-make-date
			   (seconds-to-time (+ (time-to-seconds (current-time))
					       delay)))))
	  (t (error "Malformed delay `%s'" delay)))
    (message-add-header (format "%s: %s" gnus-delay-header deadline)))
  (set-buffer-modified-p t)
  (nndraft-request-create-group gnus-delay-group)
  (message-disassociate-draft)
  (nndraft-request-associate-buffer gnus-delay-group)
  (save-buffer 0)
  (kill-buffer (current-buffer))
  (message-do-actions message-postpone-actions))

(defun gnus-delay-send-drafts ()
  "Send all the delayed messages that are due now."
  (interactive)
  (save-excursion
    (let* ((group (format "nndraft:%s" gnus-delay-group))
	   articles
	   article deadline)
      (gnus-activate-group group)
      (setq articles (nndraft-articles))
      (while (setq article (pop articles))
	(gnus-request-head article group)
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(if (re-search-forward
	     (concat "^" (regexp-quote gnus-delay-header) ":\\s-+")
	     nil t)
	    (progn
	      (setq deadline (nnheader-header-value))
	      (setq deadline (apply 'encode-time (parse-time-string deadline)))
	      (setq deadline (time-since deadline))
	      (when (and (>= (nth 0 deadline) 0)
			 (>= (nth 1 deadline) 0))
		(message "Sending delayed article %d" article)
		(gnus-draft-send article group)
		(message "Sending delayed article %d...done" article)))
	  (message "Delay header missing for article %d" article))))))

;;;###autoload
(defun gnus-delay-initialize (&optional no-keymap no-check)
  "Initialize the gnus-delay package.
This sets up a key binding in `message-mode' to delay a message.
This tells Gnus to look for delayed messages after getting new news.

Key binding is skipped if optional arg NO-KEYMAP is non-nil.
Checking delayed messages is skipped if optional arg NO-CHECK is non-nil."
  (unless no-keymap
    (require 'message)
    (define-key message-mode-map "\C-c\n" 'gnus-delay-article))
  (unless no-check
    (add-hook 'gnus-get-new-news-hook 'gnus-delay-send-drafts)))

(provide 'gnus-delay)
;;; gnus-delay.el ends here
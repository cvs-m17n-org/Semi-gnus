;;; spam-report.el --- Reporting spam
;; Copyright (C) 2002, 2003 Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: network

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

;;; This module addresses a few aspects of spam reporting under Gnus.  Page
;;; breaks are used for grouping declarations and documentation relating to
;;; each particular aspect.

;;; Code:
(require 'gnus)
(require 'gnus-sum)

(defgroup spam-report nil
  "Spam reporting configuration.")

(defcustom spam-report-gmane-regex nil
  "String matching Gmane newsgroups if wanted, e.g. \"^nntp+.*:gmane.\"
This is probably handled better with group/topic parameters."
  :type 'regexp
  :group 'spam-report)

(defcustom spam-report-gmane-spam-header 
  "^X-Report-Spam: http://\\([^/]+\\)\\(.*\\)$"
  "String matching Gmane spam-reporting header.  Two match groups are needed."
  :type 'regexp
  :group 'spam-report)

(defcustom spam-report-gmane-use-article-number t
  "Whether the article number (faster!) or the header should be used."
  :type 'boolean
  :group 'spam-report)

(defun spam-report-gmane (article)
  "Report an article as spam through Gmane"
  (interactive "nEnter the article number: ")
  (when (and gnus-newsgroup-name
	     (or (null spam-report-gmane-regex)
		 (string-match spam-report-gmane-regex gnus-newsgroup-name)))
    (gnus-message 6 "Reporting spam article %d to spam.gmane.org..." article)
      (if spam-report-gmane-use-article-number
	  (spam-report-url-ping "spam.gmane.org" 
		    (format "/%s:%d"
			    (gnus-group-real-name gnus-newsgroup-name)
			    (gnus-summary-article-number)))
	(with-current-buffer nntp-server-buffer
	  (gnus-request-head article gnus-newsgroup-name)
	  (goto-char (point-min))
	  (if (re-search-forward spam-report-gmane-spam-header nil t)
	      (let* ((host (match-string 1))
		     (report (match-string 2))
		     (url (format "http://%s%s" host report)))
		(gnus-message 10 "Reporting spam through URL %s..." url)
		(spam-report-url-ping host report))
	    (gnus-message 10 "Could not find X-Report-Spam in article %d..."
			  article))))))


(defun spam-report-url-ping (host report)
  "Ping a host through HTTP, addressing a specific GET resource"
  (let ((tcp-connection))
    (with-temp-buffer
      (or (setq tcp-connection
		(open-network-stream 
		 "URL ping"
		 (buffer-name)
		 host
		 80))
	  (error "Could not open connection to %s" host))
      (set-marker (process-mark tcp-connection) (point-min))
      (process-send-string tcp-connection
			   (format "GET %s HTTP/1.1\nHost: %s\n\n"
				   report host)))))

(provide 'spam-report)

;;; spam-report.el ends here.
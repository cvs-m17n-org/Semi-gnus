;;; rfc2231.el --- Functions for decoding rfc2231 headers
;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

(require 'ietf-drums)

(defun rfc2231-get-value (ct attribute)
  "Return the value of ATTRIBUTE from CT."
  (cdr (assq attribute (cdr ct))))

(defun rfc2231-parse-string (string)
  "Parse STRING and return a list.
The list will be on the form
 `(name (attribute . value) (attribute . value)...)"
  (with-temp-buffer
    (let ((ttoken (ietf-drums-token-to-list ietf-drums-text-token))
	  (stoken (ietf-drums-token-to-list ietf-drums-tspecials))
	  (ntoken (ietf-drums-token-to-list "0-9"))
	  (prev-value "")
	  display-name mailbox c display-string parameters
	  attribute value type subtype number encoded
	  prev-attribute)
      (ietf-drums-init (mail-header-remove-whitespace
		   (mail-header-remove-comments string)))
      (let ((table (copy-syntax-table ietf-drums-syntax-table)))
	(modify-syntax-entry ?\' "w" table)
	(set-syntax-table table))
      (setq c (following-char))
      (when (and (memq c ttoken)
		 (not (memq c stoken)))
	(setq type (downcase (buffer-substring
			      (point) (progn (forward-sexp 1) (point)))))
	;; Do the params
	(while (not (eobp))
	  (setq c (following-char))
	  (unless (eq c ?\;)
	    (error "Invalid header: %s" string))
	  (forward-char 1)
	  (setq c (following-char))
	  (if (and (memq c ttoken)
		   (not (memq c stoken)))
	      (setq attribute
		    (intern
		     (downcase
		      (buffer-substring
		       (point) (progn (forward-sexp 1) (point))))))
	    (error "Invalid header: %s" string))
	  (setq c (following-char))
	  (setq encoded nil)
	  (when (eq c ?*)
	    (forward-char 1)
	    (setq c (following-char))
	    (when (memq c ntoken)
	      (setq number
		    (string-to-number
		     (buffer-substring
		      (point) (progn (forward-sexp 1) (point)))))
	      (setq c (following-char))
	      (when (eq c ?*)
		(setq encoded t)
		(forward-char 1)
		(setq c (following-char)))))
	  ;; See if we have any previous continuations.
	  (when (and prev-attribute
		     (not (eq prev-attribute attribute)))
	    (push (cons prev-attribute prev-value) parameters)
	    (setq prev-attribute nil
		  prev-value ""))
	  (unless (eq c ?=)
	    (error "Invalid header: %s" string))
	  (forward-char 1)
	  (setq c (following-char))
	  (cond
	   ((eq c ?\")
	    (setq value
		  (buffer-substring (1+ (point))
				    (progn (forward-sexp 1) (1- (point))))))
	   ((and (memq c ttoken)
		 (not (memq c stoken)))
	    (setq value (buffer-substring
			 (point) (progn (forward-sexp 1) (point)))))
	   (t
	    (error "Invalid header: %s" string)))
	  (when encoded
	    (setq value (rfc2231-decode-encoded-string value)))
	  (if number
	      (setq prev-attribute attribute
		    prev-value (concat prev-value value))
	    (push (cons attribute value) parameters)))

	;; Take care of any final continuations.
	(when prev-attribute
	  (push (cons prev-attribute prev-value) parameters))

	`(,type ,@(nreverse parameters))))))

(defun rfc2231-decode-encoded-string (string)
  "Decode an RFC2231-encoded string.
These look like \"us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A\"."
  (with-temp-buffer
    (let ((elems (split-string string "'")))
      ;; The encoded string may contain zero to two single-quote
      ;; marks.  This should give us the encoded word stripped
      ;; of any preceding values.
      (insert (car (last elems)))
      (goto-char (point-min))
      (while (search-forward "%" nil t)
	(insert
	 (prog1
	     (string-to-number (buffer-substring (point) (+ (point) 2)) 16)
	   (delete-region (1- (point)) (+ (point) 2)))))
      ;; Encode using the charset, if any.
      (when (and (< (length elems) 1)
		 (not (equal (intern (car elems)) 'us-ascii)))
	(mm-decode-coding-region (point-min) (point-max)
				 (intern (car elems))))
      (buffer-string))))

(provide 'rfc2231)

;;; rfc2231.el ends here

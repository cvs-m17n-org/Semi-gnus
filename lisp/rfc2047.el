;;; rfc2047.el --- functions for encoding and decoding rfc2047 messages

;; Copyright (C) 1998, 1999, 2000, 2002, 2003, 2004
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
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

;; RFC 2047 is "MIME (Multipurpose Internet Mail Extensions) Part
;; Three:  Message Header Extensions for Non-ASCII Text".

;;; Code:

(eval-when-compile
  (require 'cl)
  (defvar message-posting-charset))

(require 'qp)
(require 'mm-util)
;; Fixme: Avoid this (used for mail-parse-charset) mm dependence on gnus.
(require 'mail-prsvr)
(require 'base64)
(autoload 'mm-body-7-or-8 "mm-bodies")

(defvar rfc2047-header-encoding-alist
  '(("Newsgroups" . nil)
    ("Followup-To" . nil)
    ("Message-ID" . nil)
    ("\\(Resent-\\)?\\(From\\|Cc\\|To\\|Bcc\\|Reply-To\\|Sender\
\\|Mail-Followup-To\\|Mail-Copies-To\\|Approved\\)" . address-mime)
    (t . mime))
  "*Header/encoding method alist.
The list is traversed sequentially.  The keys can either be
header regexps or t.

The values can be:

1) nil, in which case no encoding is done;
2) `mime', in which case the header will be encoded according to RFC2047;
3) `address-mime', like `mime', but takes account of the rules for address
   fields (where quoted strings and comments must be treated separately);
4) a charset, in which case it will be encoded as that charset;
5) `default', in which case the field will be encoded as the rest
   of the article.")

(defvar rfc2047-charset-encoding-alist
  '((us-ascii . nil)
    (iso-8859-1 . Q)
    (iso-8859-2 . Q)
    (iso-8859-3 . Q)
    (iso-8859-4 . Q)
    (iso-8859-5 . B)
    (koi8-r . B)
    (iso-8859-7 . B)
    (iso-8859-8 . B)
    (iso-8859-9 . Q)
    (iso-8859-14 . Q)
    (iso-8859-15 . Q)
    (iso-2022-jp . B)
    (iso-2022-kr . B)
    (gb2312 . B)
    (big5 . B)
    (cn-big5 . B)
    (cn-gb . B)
    (cn-gb-2312 . B)
    (euc-kr . B)
    (iso-2022-jp-2 . B)
    (iso-2022-int-1 . B)
    (viscii . Q))
  "Alist of MIME charsets to RFC2047 encodings.
Valid encodings are nil, `Q' and `B'.  These indicate binary (no) encoding,
quoted-printable and base64 respectively.")

(defvar rfc2047-encode-function-alist
  '((Q . rfc2047-q-encode-string)
    (B . rfc2047-b-encode-string)
    (nil . identity))
  "Alist of RFC2047 encodings to encoding functions.")

(defvar rfc2047-encode-encoded-words t
  "Whether encoded words should be encoded again.")

;;;
;;; Functions for encoding RFC2047 messages
;;;

(defun rfc2047-qp-or-base64 ()
  "Return the type with which to encode the buffer.
This is either `base64' or `quoted-printable'."
  (save-excursion
    (let ((limit (min (point-max) (+ 2000 (point-min))))
	  (n8bit 0))
      (goto-char (point-min))
      (skip-chars-forward "\x20-\x7f\r\n\t" limit)
      (while (< (point) limit)
	(incf n8bit)
	(forward-char 1)
	(skip-chars-forward "\x20-\x7f\r\n\t" limit))
      (if (or (< (* 6 n8bit) (- limit (point-min)))
	      ;; Don't base64, say, a short line with a single
	      ;; non-ASCII char when splitting parts by charset.
	      (= n8bit 1))
	  'quoted-printable
	'base64))))

(defun rfc2047-narrow-to-field ()
  "Narrow the buffer to the header on the current line."
  (beginning-of-line)
  (narrow-to-region
   (point)
   (progn
     (forward-line 1)
     (if (re-search-forward "^[^ \n\t]" nil t)
	 (point-at-bol)
       (point-max))))
  (goto-char (point-min)))

(defun rfc2047-field-value ()
  "Return the value of the field at point."
  (save-excursion
    (save-restriction
      (rfc2047-narrow-to-field)
      (re-search-forward ":[ \t\n]*" nil t)
      (buffer-substring-no-properties (point) (point-max)))))

(defvar rfc2047-encoding-type 'address-mime
  "The type of encoding done by `rfc2047-encode-region'.
This should be dynamically bound around calls to
`rfc2047-encode-region' to either `mime' or `address-mime'.  See
`rfc2047-header-encoding-alist', for definitions.")

(defun rfc2047-encode-message-header ()
  "Encode the message header according to `rfc2047-header-encoding-alist'.
Should be called narrowed to the head of the message."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (let (alist elem method)
      (while (not (eobp))
	(save-restriction
	  (rfc2047-narrow-to-field)
	  (if (not (rfc2047-encodable-p))
	      (prog1
		  (if (and (eq (mm-body-7-or-8) '8bit)
			   (mm-multibyte-p)
			   (mm-coding-system-p
			    (car message-posting-charset)))
		      ;; 8 bit must be decoded.
		      (mm-encode-coding-region
		       (point-min) (point-max)
		       (mm-charset-to-coding-system
			(car message-posting-charset))))
		;; No encoding necessary, but folding is nice
		(rfc2047-fold-region
		 (save-excursion
		   (goto-char (point-min))
		   (skip-chars-forward "^:")
		   (when (looking-at ": ")
		     (forward-char 2))
		   (point))
		 (point-max)))
	    ;; We found something that may perhaps be encoded.
	    (setq method nil
		  alist rfc2047-header-encoding-alist)
	    (while (setq elem (pop alist))
	      (when (or (and (stringp (car elem))
			     (looking-at (car elem)))
			(eq (car elem) t))
		(setq alist nil
		      method (cdr elem))))
	    (goto-char (point-min))
	    (re-search-forward "^[^:]+: *" nil t)
	    (cond
	     ((eq method 'address-mime)
	      (rfc2047-encode-region (point) (point-max)))
	     ((eq method 'mime)
	      (let ((rfc2047-encoding-type 'mime))
		(rfc2047-encode-region (point) (point-max))))
	     ((eq method 'default)
	      (if (and (featurep 'mule)
		       (if (boundp 'default-enable-multibyte-characters)
			   default-enable-multibyte-characters)
		       mail-parse-charset)
		  (mm-encode-coding-region (point) (point-max)
					   mail-parse-charset)))
	     ;; We get this when CC'ing messsages to newsgroups with
	     ;; 8-bit names.  The group name mail copy just got
	     ;; unconditionally encoded.  Previously, it would ask
	     ;; whether to encode, which was quite confusing for the
	     ;; user.  If the new behaviour is wrong, tell me. I have
	     ;; left the old code commented out below.
	     ;; -- Per Abrahamsen <abraham@dina.kvl.dk> Date: 2001-10-07.
	     ;; Modified by Dave Love, with the commented-out code changed
	     ;; in accordance with changes elsewhere.
	     ((null method)
	      (rfc2047-encode-region (point) (point-max)))
;;; 	     ((null method)
;;; 	      (if (or (message-options-get
;;; 		       'rfc2047-encode-message-header-encode-any)
;;; 		      (message-options-set
;;; 		       'rfc2047-encode-message-header-encode-any
;;; 		       (y-or-n-p
;;; 			"Some texts are not encoded. Encode anyway?")))
;;; 		  (rfc2047-encode-region (point-min) (point-max))
;;; 		(error "Cannot send unencoded text")))
	     ((mm-coding-system-p method)
	      (if (and (featurep 'mule)
		       (if (boundp 'default-enable-multibyte-characters)
			   default-enable-multibyte-characters))
		  (mm-encode-coding-region (point) (point-max) method)))
	     ;; Hm.
	     (t)))
	  (goto-char (point-max)))))))

;; Fixme: This, and the require below may not be the Right Thing, but
;; should be safe just before release.  -- fx 2001-02-08
(eval-when-compile (defvar message-posting-charset))

(defun rfc2047-encodable-p ()
  "Return non-nil if any characters in current buffer need encoding in headers.
The buffer may be narrowed."
  (require 'message)			; for message-posting-charset
  (let ((charsets
	 (mm-find-mime-charset-region (point-min) (point-max))))
    (goto-char (point-min))
    (or (and rfc2047-encode-encoded-words
	     (prog1
		 (search-forward "=?" nil t)
	       (goto-char (point-min))))
	(and charsets
	     (not (equal charsets (list (car message-posting-charset))))))))

;; Use this syntax table when parsing into regions that may need
;; encoding.  Double quotes are string delimiters, backslash is
;; character quoting, and all other RFC 2822 special characters are
;; treated as punctuation so we can use forward-sexp/forward-word to
;; skip to the end of regions appropriately.  Nb. ietf-drums does
;; things differently.
(defconst rfc2047-syntax-table
  ;; (make-char-table 'syntax-table '(2)) only works in Emacs.
  (let ((table (make-syntax-table)))
    ;; The following is done to work for setting all elements of the table
    ;; in Emacs 21 and 22 and XEmacs; it appears to be the cleanest way.
    ;; Play safe and don't assume the form of the word syntax entry --
    ;; copy it from ?a.
    (if (fboundp 'set-char-table-range)	; Emacs
	(funcall (intern "set-char-table-range")
		 table t (aref (standard-syntax-table) ?a))
      (if (fboundp 'put-char-table)
	  (if (fboundp 'get-char-table)	; warning avoidance
	      (put-char-table t (get-char-table ?a (standard-syntax-table))
			      table))))
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\( "." table)
    (modify-syntax-entry ?\) "." table)
    (modify-syntax-entry ?\< "." table)
    (modify-syntax-entry ?\> "." table)
    (modify-syntax-entry ?\[ "." table)
    (modify-syntax-entry ?\] "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?@ "." table)
    table))

(defun rfc2047-encode-region (b e)
  "Encode words in region B to E that need encoding.
By default, the region is treated as containing RFC2822 addresses.
Dynamically bind `rfc2047-encoding-type' to change that."
  (save-restriction
    (narrow-to-region b e)
    (let ((encodable-regexp (if rfc2047-encode-encoded-words
				"[^\000-\177]\\|=\\?"
			      "[^\000-\177]"))
	  start				; start of current token
	  end				; end of current token
	  ;; Whether there's an encoded word before the current token,
	  ;; either immediately or separated by space.
	  last-encoded)
      (if (eq 'mime rfc2047-encoding-type)
	  ;; Simple case.  Continuous words in which all those contain
	  ;; non-ASCII characters are encoded collectively.  Encoding
	  ;; ASCII words, including `Re:' used in Subject headers, is
	  ;; avoided for interoperability with non-MIME clients and
	  ;; for making it easy to find keywords.
	  (progn
	    (goto-char (point-min))
	    (while (progn (skip-chars-forward " \t\n")
			  (not (eobp)))
	      (setq start (point))
	      (while (and (looking-at "[ \t\n]*\\([^ \t\n]+\\)")
			  (progn
			    (setq end (match-end 0))
			    (re-search-forward encodable-regexp end t)))
		(goto-char end))
	      (if (> (point) start)
		  (rfc2047-encode start (point))
		(goto-char end))))
	;; `address-mime' case -- take care of quoted words, comments.
	(with-syntax-table rfc2047-syntax-table
	  (goto-char (point-min))
	  (condition-case nil		; in case of unbalanced quotes
	      ;; Look for rfc2822-style: sequences of atoms, quoted
	      ;; strings, specials, whitespace.  (Specials mustn't be
	      ;; encoded.)
	      (while (not (eobp))
		;; Skip whitespace.
		(skip-chars-forward " \t\n")
		(setq start (point))
		(cond
		 ((not (char-after)))	; eob
		 ;; else token start
		 ((eq ?\" (char-syntax (char-after)))
		  ;; Quoted word.
		  (forward-sexp)
		  (setq end (point))
		  ;; Does it need encoding?
		  (goto-char start)
		  (skip-chars-forward "\000-\177" end)
		  (if (= end (point))
		      (setq last-encoded  nil)
		    ;; It needs encoding.  Strip the quotes first,
		    ;; since encoded words can't occur in quotes.
		    (goto-char end)
		    (delete-backward-char 1)
		    (goto-char start)
		    (delete-char 1)
		    (when last-encoded
		      ;; There was a preceding quoted word.  We need
		      ;; to include any separating whitespace in this
		      ;; word to avoid it getting lost.
		      (skip-chars-backward " \t")
		      ;; A space is needed between the encoded words.
		      (insert ? )
		      (setq start (point)
			    end (1+ end)))
		    ;; Adjust the end position for the deleted quotes.
		    (rfc2047-encode start (- end 2))
		    (setq last-encoded t))) ; record that it was encoded
		 ((eq ?. (char-syntax (char-after)))
		  ;; Skip other delimiters, but record that they've
		  ;; potentially separated quoted words.
		  (forward-char)
		  (setq last-encoded nil))
		 (t		    ; normal token/whitespace sequence
		  ;; Find the end.
		  (skip-chars-backward " \t\n")
		  (if (and (eq (char-before) ?\()
			   ;; Look for the end of parentheses.
			   (let ((string (buffer-substring (point)
							   (point-max)))
				 (default-major-mode 'fundamental-mode))
			     ;; Use `standard-syntax-table'.
			     (with-temp-buffer
			       (insert "(" string)
			       (goto-char (point-min))
			       (condition-case nil
				   (progn
				     (forward-list 1)
				     (setq end (- (point) 3)))
				 (error nil)))))
		      ;; Encode text as an unstructured field.
		      (let ((rfc2047-encoding-type 'mime))
			(rfc2047-encode-region start (+ (point) end))
			(forward-char))
		    ;; Skip one ASCII word, or encode continuous words
		    ;; in which all those contain non-ASCII characters.
		    (skip-chars-forward " \t\n")
		    (setq end nil)
		    (while (not end)
		      (when (looking-at "[\000-\177]+")
			(setq end (match-end 0))
			(if (re-search-forward "[ \t\n]\\|\\Sw" end t)
			    (goto-char (match-beginning 0))
			  (goto-char end)
			  (setq end nil)))
		      (unless end
			(setq end t)
			(when (looking-at "[^\000-\177]+")
			  (goto-char (match-end 0))
			  (while (and (looking-at "[ \t\n]+\\([^ \t\n]+\\)")
				      (setq end (match-end 0))
				      (string-match "[^\000-\177]"
						    (match-string 1)))
			    (goto-char end))
			  (when (looking-at "[^ \t\n]+")
			    (setq end (match-end 0))
			    (if (re-search-forward "\\Sw+" end t)
				;; There are special characters better
				;; to be encoded so that MTAs may parse
				;; them safely.
				(cond ((= end (point)))
				      ((looking-at "[^\000-\177]")
				       (setq end nil))
				      (t
				       (goto-char (1- (match-end 0)))
				       (unless (= (point) (match-beginning 0))
					 (insert " "))))
			      (goto-char end)
			      (skip-chars-forward " \t\n")
			      (if (and (looking-at "[^ \t\n]+")
				       (string-match "[^\000-\177]"
						     (match-string 0)))
				  (setq end nil)
				(goto-char end)))))))
		    (skip-chars-backward " \t\n")
		    (setq end (point))
		    (goto-char start)
		    (skip-chars-forward "\000-\177" end)
		    (if (= end (point))
			(setq last-encoded nil)
		      (rfc2047-encode start end)
		      (setq last-encoded t))))))
	    (error
	     (error "Invalid data for rfc2047 encoding: %s"
		    (buffer-substring b e)))))))
    (rfc2047-fold-region b (point))))

(defun rfc2047-encode-string (string)
  "Encode words in STRING.
By default, the string is treated as containing addresses (see
`rfc2047-encoding-type')."
  (with-temp-buffer
    (insert string)
    (rfc2047-encode-region (point-min) (point-max))
    (buffer-string)))

(defun rfc2047-encode-1 (column string cs encoder start space &optional eword)
  "Subroutine used by `rfc2047-encode'."
  (cond ((string-equal string "")
	 (or eword ""))
	((>= column 76)
	 (when (and eword
		    (string-match "\n[ \t]+\\'" eword))
	   ;; Reomove a superfluous empty line.
	   (setq eword (substring eword 0 (match-beginning 0))))
	 (rfc2047-encode-1 (length space) string cs encoder start " "
			   (concat eword "\n" space)))
	(t
	 (let ((index 0)
	       (limit (1- (length string)))
	       (prev "")
	       next)
	   (while (and prev
		       (<= index limit))
	     (setq next (concat start
				(funcall encoder
					 (if cs
					     (mm-encode-coding-string
					      (substring string 0 (1+ index))
					      cs)
					   (substring string 0 (1+ index))))
				"?="))
	     (if (<= (+ column (length next)) 76)
		 (setq prev next
		       index (1+ index))
	       (setq next prev
		     prev nil)))
	   (setq eword (concat eword next))
	   (if (> index limit)
	       eword
	     (when (string-match "\n[ \t]+\\'" eword)
	       ;; Reomove a superfluous empty line.
	       (setq eword (substring eword 0 (match-beginning 0))))
	     (rfc2047-encode-1 (length space) (substring string index)
			       cs encoder start " "
			       (concat eword "\n" space)))))))

(defun rfc2047-encode (b e)
  "Encode the word(s) in the region B to E.
By default, the region is treated as containing addresses (see
`rfc2047-encoding-type')."
  (let ((mime-charset (or (mm-find-mime-charset-region b e) (list 'us-ascii)))
	cs encoding space eword)
    (cond ((> (length mime-charset) 1)
	   (error "Can't rfc2047-encode `%s'"
		  (buffer-substring-no-properties b e)))
	  ((= (length mime-charset) 1)
	   (setq mime-charset (car mime-charset)
		 cs (mm-charset-to-coding-system mime-charset))
	   (unless (and (mm-multibyte-p)
			(mm-coding-system-p cs))
	     (setq cs nil))
	   (save-restriction
	     (narrow-to-region b e)
	     (setq encoding
		   (or (cdr (assq mime-charset
				  rfc2047-charset-encoding-alist))
		       ;; For the charsets that don't have a preferred
		       ;; encoding, choose the one that's shorter.
		       (if (eq (rfc2047-qp-or-base64) 'base64)
			   'B
			 'Q)))
	     (widen)
	     (goto-char b)
	     (unless (= 0 (skip-chars-backward " \t"))
	       (setq space (buffer-substring-no-properties (point) b)))
	     (setq eword (rfc2047-encode-1
			  (- b (point-at-bol))
			  (mm-replace-in-string
			   (buffer-substring-no-properties b e)
			   "\n\\([ \t]?\\)" "\\1")
			  cs
			  (or (cdr (assq encoding
					 rfc2047-encode-function-alist))
			      'identity)
			  (concat "=?" (downcase (symbol-name mime-charset))
				  "?" (upcase (symbol-name encoding)) "?")
			  (or space " ")))
	     (delete-region (if (eq (aref eword 0) ?\n)
				(point)
			      (goto-char b))
			    e)
	     (insert eword)
	     (unless (or (eolp)
			 (looking-at "[ \t\n)]"))
	       (insert " "))))
	  (t
	   (goto-char e)))))

(defun rfc2047-fold-field ()
  "Fold the current header field."
  (save-excursion
    (save-restriction
      (rfc2047-narrow-to-field)
      (rfc2047-fold-region (point-min) (point-max)))))

(defun rfc2047-fold-region (b e)
  "Fold long lines in region B to E."
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (let ((break nil)
	  (qword-break nil)
	  (first t)
	  (bol (save-restriction
		 (widen)
		 (point-at-bol))))
      (while (not (eobp))
	(when (and (or break qword-break)
		   (> (- (point) bol) 76))
	  (goto-char (or break qword-break))
	  (setq break nil
		qword-break nil)
	  (skip-chars-backward " \t")
	  (if (looking-at "[ \t]")
	      (insert ?\n)
	    (insert "\n "))
	  (setq bol (1- (point)))
	  ;; Don't break before the first non-LWSP characters.
	  (skip-chars-forward " \t")
	  (unless (eobp)
	    (forward-char 1)))
	(cond
	 ((eq (char-after) ?\n)
	  (forward-char 1)
	  (setq bol (point)
		break nil
		qword-break nil)
	  (skip-chars-forward " \t")
	  (unless (or (eobp) (eq (char-after) ?\n))
	    (forward-char 1)))
	 ((eq (char-after) ?\r)
	  (forward-char 1))
	 ((memq (char-after) '(?  ?\t))
	  (skip-chars-forward " \t")
	  (unless first ;; Don't break just after the header name.
	    (setq break (point))))
	 ((not break)
	  (if (not (looking-at "=\\?[^=]"))
	      (if (eq (char-after) ?=)
		  (forward-char 1)
		(skip-chars-forward "^ \t\n\r="))
	    ;; Don't break at the start of the field.
	    (unless (= (point) b)
	      (setq qword-break (point)))
	    (skip-chars-forward "^ \t\n\r")))
	 (t
	  (skip-chars-forward "^ \t\n\r")))
	(setq first nil))
      (when (and (or break qword-break)
		 (> (- (point) bol) 76))
	(goto-char (or break qword-break))
	(setq break nil
	      qword-break nil)
	(if (looking-at "[ \t]")
	    (insert ?\n)
	  (insert "\n "))
	(setq bol (1- (point)))
	;; Don't break before the first non-LWSP characters.
	(skip-chars-forward " \t")
	(unless (eobp)
	  (forward-char 1))))))

(defun rfc2047-unfold-field ()
  "Fold the current line."
  (save-excursion
    (save-restriction
      (rfc2047-narrow-to-field)
      (rfc2047-unfold-region (point-min) (point-max)))))

(defun rfc2047-unfold-region (b e)
  "Unfold lines in region B to E."
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (let ((bol (save-restriction
		 (widen)
		 (point-at-bol)))
	  (eol (point-at-eol)))
      (forward-line 1)
      (while (not (eobp))
	(if (and (looking-at "[ \t]")
		 (< (- (point-at-eol) bol) 76))
	    (delete-region eol (progn
				 (goto-char eol)
				 (skip-chars-forward "\r\n")
				 (point)))
	  (setq bol (point-at-bol)))
	(setq eol (point-at-eol))
	(forward-line 1)))))

(defun rfc2047-b-encode-string (string)
  "Base64-encode the header contained in STRING."
  (base64-encode-string string t))

(defun rfc2047-q-encode-string (string)
  "Quoted-printable-encode the header in STRING."
  (mm-with-unibyte-buffer
    (insert string)
    (quoted-printable-encode-region
     (point-min) (point-max) nil
     ;; = (\075), _ (\137), ? (\077) are used in the encoded word.
     ;; Avoid using 8bit characters.
     ;; This list excludes `especials' (see the RFC2047 syntax),
     ;; meaning that some characters in non-structured fields will
     ;; get encoded when they con't need to be.  The following is
     ;; what it used to be.
     ;;;  ;; Equivalent to "^\000-\007\011\013\015-\037\200-\377=_?"
     ;;;  "\010\012\014\040-\074\076\100-\136\140-\177")
     "-\b\n\f !#-'*+0-9A-Z\\^`-~\d")
    (subst-char-in-region (point-min) (point-max) ?  ?_)
    (buffer-string)))

;;;
;;; Functions for decoding RFC2047 messages
;;;

(eval-and-compile
  (defconst rfc2047-encoded-word-regexp
    "=\\?\\([^][\000-\040()<>@,\;:*\\\"/?.=]+\\)\\(?:\\*[^?]+\\)?\
\\?\\(B\\|Q\\)\\?\\([!->@-~ ]*\\)\\?="))

;; Fixme: This should decode in place, not cons intermediate strings.
;; Also check whether it needs to worry about delimiting fields like
;; encoding.

;; In fact it's reported that (invalid) encoding of mailboxes in
;; addr-specs is in use, so delimiting fields might help.  Probably
;; not decoding a word which isn't properly delimited is good enough
;; and worthwhile (is it more correct or not?), e.g. something like
;; `=?iso-8859-1?q?foo?=@'.

(defun rfc2047-decode-region (start end)
  "Decode MIME-encoded words in region between START and END."
  (interactive "r")
  (let ((case-fold-search t)
	b e)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	;; Remove whitespace between encoded words.
	(while (re-search-forward
		(eval-when-compile
		  (concat "\\(" rfc2047-encoded-word-regexp "\\)"
			  "\\(\n?[ \t]\\)+"
			  "\\(" rfc2047-encoded-word-regexp "\\)"))
		nil t)
	  (delete-region (goto-char (match-end 1)) (match-beginning 6)))
	;; Decode the encoded words.
	(setq b (goto-char (point-min)))
	(while (re-search-forward rfc2047-encoded-word-regexp nil t)
	  (setq e (match-beginning 0))
	  (insert (rfc2047-parse-and-decode
		   (prog1
		       (match-string 0)
		     (delete-region (match-beginning 0) (match-end 0)))))
	  ;; Remove newlines between decoded words, though such things
	  ;; essentially must not be there.
	  (save-restriction
	    (narrow-to-region e (point))
	    (goto-char e)
	    (while (re-search-forward "[\n\r]+" nil t)
	      (replace-match " "))
	    (goto-char (point-max)))
	  (when (and (mm-multibyte-p)
		     mail-parse-charset
		     (not (eq mail-parse-charset 'us-ascii))
		     (not (eq mail-parse-charset 'gnus-decoded)))
	    (mm-decode-coding-region b e mail-parse-charset))
	  (setq b (point)))
	(when (and (mm-multibyte-p)
		   mail-parse-charset
		   (not (eq mail-parse-charset 'us-ascii))
		   (not (eq mail-parse-charset 'gnus-decoded)))
	  (mm-decode-coding-region b (point-max) mail-parse-charset))))))

(defun rfc2047-decode-string (string)
  "Decode the quoted-printable-encoded STRING and return the results."
  (let ((m (mm-multibyte-p)))
    (if (string-match "=\\?" string)
	(with-temp-buffer
	  ;; Fixme: This logic is wrong, but seems to be required by
	  ;; Gnus summary buffer generation.  The value of `m' depends
	  ;; on the current buffer, not global multibyteness or that
	  ;; of the string.  Also the string returned should always be
	  ;; multibyte in a multibyte session, i.e. the buffer should
	  ;; be multibyte before `buffer-string' is called.
	  (when m
	    (mm-enable-multibyte))
	  (insert string)
	  (inline
	    (rfc2047-decode-region (point-min) (point-max)))
	  (buffer-string))
      ;; Fixme: As above, `m' here is inappropriate.
      (if (and m
	       mail-parse-charset
	       (not (eq mail-parse-charset 'us-ascii))
	       (not (eq mail-parse-charset 'gnus-decoded)))
	  ;; `decode-coding-string' in Emacs offers a third optional
	  ;; arg NOCOPY to avoid consing a new string if the decoding
	  ;; is "trivial".  Unfortunately it currently doesn't
	  ;; consider anything else than a `nil' coding system
	  ;; trivial.
	  ;; `rfc2047-decode-string' is called multiple times for each
	  ;; article during summary buffer generation, and we really
	  ;; want to avoid unnecessary consing.  So we bypass
	  ;; `decode-coding-string' if the string is purely ASCII.
	  (if (and (fboundp 'detect-coding-string)
		   ;; string is purely ASCII
		   (eq (detect-coding-string string t) 'undecided))
	      string
	    (mm-decode-coding-string string mail-parse-charset))
	(mm-string-as-multibyte string)))))

(defun rfc2047-parse-and-decode (word)
  "Decode WORD and return it if it is an encoded word.
Return WORD if it is not not an encoded word or if the charset isn't
decodable."
  (if (not (string-match rfc2047-encoded-word-regexp word))
      word
    (or
     (condition-case nil
	 (rfc2047-decode
	  (match-string 1 word)
	  (string-to-char (match-string 2 word))
	  (match-string 3 word))
       (error word))
     word)))				; un-decodable

(defun rfc2047-pad-base64 (string)
  "Pad STRING to quartets."
  ;; Be more liberal to accept buggy base64 strings. If
  ;; base64-decode-string accepts buggy strings, this function could
  ;; be aliased to identity.
  (if (= 0 (mod (length string) 4))
      string
    (when (string-match "=+$" string)
      (setq string (substring string 0 (match-beginning 0))))
    (case (mod (length string) 4)
      (0 string)
      (1 string) ;; Error, don't pad it.
      (2 (concat string "=="))
      (3 (concat string "=")))))

(defun rfc2047-decode (charset encoding string)
  "Decode STRING from the given MIME CHARSET in the given ENCODING.
Valid ENCODINGs are the characters \"B\" and \"Q\".
If your Emacs implementation can't decode CHARSET, return nil."
  (if (stringp charset)
      (setq charset (intern (downcase charset))))
  (if (or (not charset)
	  (eq 'gnus-all mail-parse-ignored-charsets)
	  (memq 'gnus-all mail-parse-ignored-charsets)
	  (memq charset mail-parse-ignored-charsets))
      (setq charset mail-parse-charset))
  (let ((cs (mm-charset-to-coding-system charset)))
    (if (and (not cs) charset
	     (listp mail-parse-ignored-charsets)
	     (memq 'gnus-unknown mail-parse-ignored-charsets))
	(setq cs (mm-charset-to-coding-system mail-parse-charset)))
    (when cs
      (when (and (eq cs 'ascii)
		 mail-parse-charset)
	(setq cs mail-parse-charset))
      (mm-decode-coding-string
       (cond
	((char-equal ?B encoding)
	 (base64-decode-string
	  (rfc2047-pad-base64 string)))
	((char-equal ?Q encoding)
	 (quoted-printable-decode-string
	  (mm-subst-char-in-string ?_ ? string t)))
	(t (error "Invalid encoding: %c" encoding)))
       cs))))

(provide 'rfc2047)

;;; rfc2047.el ends here

;;; nnheader.el --- header access macros for Semi-gnus and its backends

;; Copyright (C) 1987, 1988, 1989, 1990, 1993, 1994, 1995, 1996,
;;        1997, 1998, 2000
;;        Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; 	Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;	Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: mail, news, MIME

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

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'static))

(require 'mail-utils)
(require 'mime)

(defvar nnheader-max-head-length 4096
  "*Max length of the head of articles.")

(defvar nnheader-head-chop-length 2048
  "*Length of each read operation when trying to fetch HEAD headers.")

(defvar nnheader-file-name-translation-alist nil
  "*Alist that says how to translate characters in file names.
For instance, if \":\" is invalid as a file character in file names
on your system, you could say something like:

\(setq nnheader-file-name-translation-alist '((?: . ?_)))")

(defvar nnheader-text-coding-system
  (if (memq system-type '(windows-nt ms-dos ms-windows))
      'raw-text-dos
    'raw-text)
  "Text-safe coding system (For removing ^M).
This variable is a substitute for `mm-text-coding-system'.")

(defvar nnheader-text-coding-system-for-write nil
  "Text coding system for write.
This variable is a substitute for `mm-text-coding-system-for-write'.")

(eval-and-compile
  (autoload 'nnmail-message-id "nnmail")
  (autoload 'mail-position-on-field "sendmail")
  (autoload 'message-remove-header "message")
  (autoload 'gnus-point-at-eol "gnus-util")
  (autoload 'gnus-delete-line "gnus-util")
  (autoload 'gnus-buffer-live-p "gnus-util"))

;;; Header access macros.

;; These macros may look very much like the ones in GNUS 4.1.  They
;; are, in a way, but you should note that the indices they use have
;; been changed from the internal GNUS format to the NOV format.  The
;; makes it possible to read headers from XOVER much faster.
;;
;; The format of a header is now:
;; [number subject from date id references chars lines xref extra]
;;
;; (That next-to-last entry is defined as "misc" in the NOV format,
;; but Gnus uses it for xrefs.)

(require 'mmgnus)

(defmacro mail-header-number (header)
  "Return article number in HEADER."
  `(mime-entity-location-internal ,header))

(defmacro mail-header-set-number (header number)
  "Set article number of HEADER to NUMBER."
  `(mime-entity-set-location-internal ,header ,number))

(defalias 'mail-header-subject 'mime-gnus-entity-subject-internal)
(defalias 'mail-header-set-subject 'mime-gnus-entity-set-subject-internal)

(defalias 'mail-header-from 'mime-gnus-entity-from-internal)
(defalias 'mail-header-set-from 'mime-gnus-entity-set-from-internal)

(defalias 'mail-header-date 'mime-gnus-entity-date-internal)
(defalias 'mail-header-set-date 'mime-gnus-entity-set-date-internal)

(defalias 'mail-header-message-id 'mime-gnus-entity-id-internal)
(defalias 'mail-header-id 'mime-gnus-entity-id-internal)
(defalias 'mail-header-set-message-id 'mime-gnus-entity-set-id-internal)
(defalias 'mail-header-set-id 'mime-gnus-entity-set-id-internal)

(defalias 'mail-header-references 'mime-gnus-entity-references-internal)
(defalias 'mail-header-set-references
  'mime-gnus-entity-set-references-internal)

(defalias 'mail-header-chars 'mime-gnus-entity-chars-internal)
(defalias 'mail-header-set-chars 'mime-gnus-entity-set-chars-internal)

(defalias 'mail-header-lines 'mime-gnus-entity-lines-internal)
(defalias 'mail-header-set-lines 'mime-gnus-entity-set-lines-internal)

(defalias 'mail-header-xref 'mime-gnus-entity-xref-internal)
(defalias 'mail-header-set-xref 'mime-gnus-entity-set-xref-internal)

(defalias 'nnheader-decode-subject
  (mime-find-field-decoder 'Subject 'nov))
(defalias 'nnheader-decode-from
  (mime-find-field-decoder 'From 'nov))

(defalias 'mail-header-extra 'mime-gnus-entity-extra-internal)
(defalias 'mail-header-set-extra 'mime-gnus-entity-set-extra-internal)

(defun nnheader-decode-field-body (field-body field-name
					      &optional mode max-column)
  (mime-decode-field-body field-body
			  (if (stringp field-name)
			      (intern (capitalize field-name))
			    field-name)
			  mode max-column))

(defsubst make-full-mail-header (&optional number subject from date id
					   references chars lines xref
					   extra)
  "Create a new mail header structure initialized with the parameters given."
  (luna-make-entity (mm-expand-class-name 'gnus)
		    :location number
		    :subject (if subject
				 (nnheader-decode-subject subject))
		    :from (if from
			      (nnheader-decode-from from))
		    :date date
		    :id id
		    :references references
		    :chars chars
		    :lines lines
		    :xref xref
		    :original-header (list (cons 'Subject subject)
					   (cons 'From from))
		    :extra extra))

(defsubst make-full-mail-header-from-decoded-header
  (&optional number subject from date id references chars lines xref extra)
  "Create a new mail header structure initialized with the parameters given."
  (luna-make-entity (mm-expand-class-name 'gnus)
		    :location number
		    :subject subject
		    :from from
		    :date date
		    :id id
		    :references references
		    :chars chars
		    :lines lines
		    :xref xref
		    :extra extra))

(defsubst make-mail-header (&optional init)
  "Create a new mail header structure initialized with INIT."
  (make-full-mail-header init init init init init
			 init init init init init))

;; fake message-ids: generation and detection

(defvar nnheader-fake-message-id 1)

(defsubst nnheader-generate-fake-message-id ()
  (concat "fake+none+" (int-to-string (incf nnheader-fake-message-id))))

(defsubst nnheader-fake-message-id-p (id)
  (save-match-data			; regular message-id's are <.*>
    (string-match "\\`fake\\+none\\+[0-9]+\\'" id)))

;; Parsing headers and NOV lines.

(defsubst nnheader-header-value ()
  (buffer-substring (match-end 0) (std11-field-end)))

(defun nnheader-parse-head (&optional naked)
  (let ((case-fold-search t)
	(cur (current-buffer))
	(buffer-read-only nil)
	in-reply-to lines p ref)
    (goto-char (point-min))
    (when naked
      (insert "\n"))
    ;; Search to the beginning of the next header.  Error messages
    ;; do not begin with 2 or 3.
    (prog1
	(when (or naked (re-search-forward "^[23][0-9]+ " nil t))
	  ;; This implementation of this function, with nine
	  ;; search-forwards instead of the one re-search-forward and
	  ;; a case (which basically was the old function) is actually
	  ;; about twice as fast, even though it looks messier.	 You
	  ;; can't have everything, I guess.  Speed and elegance
	  ;; don't always go hand in hand.
	  (make-full-mail-header
	   ;; Number.
	   (if naked
	       (progn
		 (setq p (point-min))
		 0)
	     (prog1
		 (read cur)
	       (end-of-line)
	       (setq p (point))
	       (narrow-to-region (point)
				 (or (and (search-forward "\n.\n" nil t)
					  (- (point) 2))
				     (point)))))
	   ;; Subject.
	   (progn
	     (goto-char p)
	     (if (search-forward "\nsubject: " nil t)
		 (nnheader-header-value) "(none)"))
	   ;; From.
	   (progn
	     (goto-char p)
	     (if (or (search-forward "\nfrom: " nil t)
		     (search-forward "\nfrom:" nil t))
		 (nnheader-header-value) "(nobody)"))
	   ;; Date.
	   (progn
	     (goto-char p)
	     (if (search-forward "\ndate: " nil t)
		 (nnheader-header-value) ""))
	   ;; Message-ID.
	   (progn
	     (goto-char p)
	     (if (search-forward "\nmessage-id:" nil t)
		 (buffer-substring
		  (1- (or (search-forward "<" (gnus-point-at-eol) t)
			  (point)))
		  (or (search-forward ">" (gnus-point-at-eol) t) (point)))
	       ;; If there was no message-id, we just fake one to make
	       ;; subsequent routines simpler.
	       (nnheader-generate-fake-message-id)))
	   ;; References.
	   (progn
	     (goto-char p)
	     (if (search-forward "\nreferences: " nil t)
		 (nnheader-header-value)
	       ;; Get the references from the in-reply-to header if there
	       ;; were no references and the in-reply-to header looks
	       ;; promising.
	       (if (and (search-forward "\nin-reply-to: " nil t)
			(setq in-reply-to (nnheader-header-value))
			(string-match "<[^\n>]+>" in-reply-to))
		   (let (ref2)
		     (setq ref (substring in-reply-to (match-beginning 0)
					  (match-end 0)))
		     (while (string-match "<[^\n>]+>"
					  in-reply-to (match-end 0))
		       (setq ref2 (substring in-reply-to (match-beginning 0)
					     (match-end 0)))
		       (when (> (length ref2) (length ref))
			 (setq ref ref2)))
                     ref)
		 nil)))
	   ;; Chars.
	   0
	   ;; Lines.
	   (progn
	     (goto-char p)
	     (if (search-forward "\nlines: " nil t)
		 (if (numberp (setq lines (read cur)))
		     lines 0)
	       0))
	   ;; Xref.
	   (progn
	     (goto-char p)
	     (and (search-forward "\nxref: " nil t)
		  (nnheader-header-value)))

	   ;; Extra.
	   (when nnmail-extra-headers
	     (let ((extra nnmail-extra-headers)
		   out)
	       (while extra
		 (goto-char p)
		 (when (search-forward
			(concat "\n" (symbol-name (car extra)) ": ") nil t)
		   (push (cons (car extra) (nnheader-header-value))
			 out))
		 (pop extra))
	       out))))
      (when naked
	(goto-char (point-min))
	(delete-char 1)))))

(defmacro nnheader-nov-skip-field ()
  '(search-forward "\t" eol 'move))

(defmacro nnheader-nov-field ()
  '(buffer-substring (point) (if (nnheader-nov-skip-field) (1- (point)) eol)))

(defmacro nnheader-nov-read-integer ()
  '(prog1
       (if (eq (char-after) ?\t)
	   0
	 (let ((num (condition-case nil
			(read (current-buffer))
		      (error nil))))
	   (if (numberp num) num 0)))
     (unless (eobp)
       (search-forward "\t" eol 'move))))

(defmacro nnheader-nov-parse-extra ()
  '(let (out string)
     (while (not (memq (char-after) '(?\n nil)))
       (setq string (nnheader-nov-field))
       (when (string-match "^\\([^ :]+\\): " string)
	 (push (cons (intern (match-string 1 string))
		     (substring string (match-end 0)))
	       out)))
     out))

(defmacro nnheader-nov-read-message-id ()
  '(let ((id (nnheader-nov-field)))
     (if (string-match "^<[^>]+>$" id)
	 id
       (nnheader-generate-fake-message-id))))

(defun nnheader-parse-nov ()
  (let ((eol (gnus-point-at-eol)))
    (make-full-mail-header
     (nnheader-nov-read-integer)	; number
     (nnheader-nov-field)		; subject
     (nnheader-nov-field)		; from
     (nnheader-nov-field)		; date
     (nnheader-nov-read-message-id)	; id
     (nnheader-nov-field)		; refs
     (nnheader-nov-read-integer)	; chars
     (nnheader-nov-read-integer)	; lines
     (if (eq (char-after) ?\n)
	 nil
       (if (looking-at "Xref: ")
	   (goto-char (match-end 0)))
       (nnheader-nov-field))		; Xref
     (nnheader-nov-parse-extra))))	; extra

(defun nnheader-insert-nov (header)
  (princ (mail-header-number header) (current-buffer))
  (let ((p (point)))
    (insert
     "\t"
     (or (mime-entity-fetch-field header 'Subject) "(none)") "\t"
     (or (mime-entity-fetch-field header 'From) "(nobody)") "\t"
     (or (mail-header-date header) "") "\t"
     (or (mail-header-id header)
	 (nnmail-message-id))
     "\t"
     (or (mail-header-references header) "") "\t")
    (princ (or (mail-header-chars header) 0) (current-buffer))
    (insert "\t")
    (princ (or (mail-header-lines header) 0) (current-buffer))
    (insert "\t")
    (when (mail-header-xref header)
      (insert "Xref: " (mail-header-xref header)))
    (when (or (mail-header-xref header)
	      (mail-header-extra header))
      (insert "\t"))
    (when (mail-header-extra header)
      (let ((extra (mail-header-extra header)))
	(while extra
	  (insert (symbol-name (caar extra))
		  ": " (cdar extra) "\t")
	  (pop extra))))
    (insert "\n")
    (backward-char 1)
    (while (search-backward "\n" p t)
      (delete-char 1))
    (forward-line 1)))

(defun nnheader-insert-header (header)
  (insert
   "Subject: " (or (mail-header-subject header) "(none)") "\n"
   "From: " (or (mail-header-from header) "(nobody)") "\n"
   "Date: " (or (mail-header-date header) "") "\n"
   "Message-ID: " (or (mail-header-id header) (nnmail-message-id)) "\n"
   "References: " (or (mail-header-references header) "") "\n"
   "Lines: ")
  (princ (or (mail-header-lines header) 0) (current-buffer))
  (insert "\n\n"))

(defun nnheader-insert-article-line (article)
  (goto-char (point-min))
  (insert "220 ")
  (princ article (current-buffer))
  (insert " Article retrieved.\n")
  (search-forward "\n\n" nil 'move)
  (delete-region (point) (point-max))
  (forward-char -1)
  (insert "."))

(defun nnheader-nov-delete-outside-range (beg end)
  "Delete all NOV lines that lie outside the BEG to END range."
  ;; First we find the first wanted line.
  (nnheader-find-nov-line beg)
  (delete-region (point-min) (point))
  ;; Then we find the last wanted line.
  (when (nnheader-find-nov-line end)
    (forward-line 1))
  (delete-region (point) (point-max)))

(defun nnheader-find-nov-line (article)
  "Put point at the NOV line that start with ARTICLE.
If ARTICLE doesn't exist, put point where that line
would have been.  The function will return non-nil if
the line could be found."
  ;; This function basically does a binary search.
  (let ((max (point-max))
	(min (goto-char (point-min)))
	(cur (current-buffer))
	(prev (point-min))
	num found)
    (while (not found)
      (goto-char (/ (+ max min) 2))
      (beginning-of-line)
      (if (or (= (point) prev)
	      (eobp))
	  (setq found t)
	(setq prev (point))
	(while (and (not (numberp (setq num (read cur))))
		    (not (eobp)))
	  (gnus-delete-line))
	(cond ((> num article)
	       (setq max (point)))
	      ((< num article)
	       (setq min (point)))
	      (t
	       (setq found 'yes)))))
    ;; We may be at the first line.
    (when (and (not num)
	       (not (eobp)))
      (setq num (read cur)))
    ;; Now we may have found the article we're looking for, or we
    ;; may be somewhere near it.
    (when (and (not (eq found 'yes))
	       (not (eq num article)))
      (setq found (point))
      (while (and (< (point) max)
		  (or (not (numberp num))
		      (< num article)))
	(forward-line 1)
	(setq found (point))
	(or (eobp)
	    (= (setq num (read cur)) article)))
      (unless (eq num article)
	(goto-char found)))
    (beginning-of-line)
    (eq num article)))

(defun nnheader-retrieve-headers-from-directory* (articles
						  directory dependencies
						  &optional
						  fetch-old force-new large
						  backend)
  (with-temp-buffer
    (let* ((file nil)
	   (number (length articles))
	   (count 0)
	   (file-name-coding-system 'binary)
	   (pathname-coding-system 'binary)
	   (case-fold-search t)
	   (cur (current-buffer))
	   article
	   headers header id end ref in-reply-to lines chars ctype)
      ;; We don't support fetching by Message-ID.
      (if (stringp (car articles))
	  'headers
	(while articles
	  (when (and (file-exists-p
		      (setq file (expand-file-name
				  (int-to-string
				   (setq article (pop articles)))
				  directory)))
		     (not (file-directory-p file)))
	    (erase-buffer)
	    (nnheader-insert-head file)
	    (save-restriction
	      (std11-narrow-to-header)
	      (setq
	       header
	       (make-full-mail-header
		;; Number.
		article
		;; Subject.
		(or (std11-fetch-field "Subject")
		    "(none)")
		;; From.
		(or (std11-fetch-field "From")
		    "(nobody)")
		;; Date.
		(or (std11-fetch-field "Date")
		    "")
		;; Message-ID.
		(progn
		  (goto-char (point-min))
		  (setq id (if (re-search-forward
				"^Message-ID: *\\(<[^\n\t> ]+>\\)" nil t)
			       ;; We do it this way to make sure the Message-ID
			       ;; is (somewhat) syntactically valid.
			       (buffer-substring (match-beginning 1)
						 (match-end 1))
			     ;; If there was no message-id, we just fake one
			     ;; to make subsequent routines simpler.
			     (nnheader-generate-fake-message-id))))
		;; References.
		(progn
		  (goto-char (point-min))
		  (if (search-forward "\nReferences: " nil t)
		      (progn
			(setq end (point))
			(prog1
			    (buffer-substring (match-end 0) (std11-field-end))
			  (setq ref
				(buffer-substring
				 (progn
				   ;; (end-of-line)
				   (search-backward ">" end t)
				   (1+ (point)))
				 (progn
				   (search-backward "<" end t)
				   (point))))))
		    ;; Get the references from the in-reply-to header if there
		    ;; were no references and the in-reply-to header looks
		    ;; promising.
		    (if (and (search-forward "\nIn-Reply-To: " nil t)
			     (setq in-reply-to
				   (buffer-substring (match-end 0)
						     (std11-field-end)))
			     (string-match "<[^>]+>" in-reply-to))
			(let (ref2)
			  (setq ref (substring in-reply-to (match-beginning 0)
					       (match-end 0)))
			  (while (string-match "<[^>]+>"
					       in-reply-to (match-end 0))
			    (setq ref2
				  (substring in-reply-to (match-beginning 0)
					     (match-end 0)))
			    (when (> (length ref2) (length ref))
			      (setq ref ref2)))
			  ref)
		      (setq ref nil))))
		;; Chars.
		(progn
		  (goto-char (point-min))
		  (if (search-forward "\nChars: " nil t)
		      (if (numberp (setq chars (ignore-errors (read cur))))
			  chars 0)
		    0))
		;; Lines.
		(progn
		  (goto-char (point-min))
		  (if (search-forward "\nLines: " nil t)
		      (if (numberp (setq lines (ignore-errors (read cur))))
			  lines 0)
		    0))
		;; Xref.
		(std11-fetch-field "Xref")
		))
	      (goto-char (point-min))
	      (if (setq ctype (std11-fetch-field "Content-Type"))
		  (mime-entity-set-content-type-internal
		   header (mime-parse-Content-Type ctype)))
	      )
	    (when (setq header
			(gnus-dependencies-add-header
			 header dependencies force-new))
	      (push header headers))
	    )
	  (setq count (1+ count))

	  (and large
	       (zerop (% count 20))
	       (nnheader-message 5 "%s: Receiving headers... %d%%"
				 backend
				 (/ (* count 100) number))))

	(when large
	  (nnheader-message 5 "%s: Receiving headers...done" backend))

	headers))))

(defun nnheader-retrieve-headers-from-directory (articles
						 directory dependencies
						 &optional
						 fetch-old force-new large
						 backend)
  (cons 'header
	(nreverse (nnheader-retrieve-headers-from-directory*
		   articles directory dependencies
		   fetch-old force-new large backend))))

(defun nnheader-get-newsgroup-headers-xover* (sequence
					      &optional
					      force-new dependencies
					      group)
  "Parse the news overview data in the server buffer, and return a
list of headers that match SEQUENCE (see `nntp-retrieve-headers')."
  ;; Get the Xref when the users reads the articles since most/some
  ;; NNTP servers do not include Xrefs when using XOVER.
  ;; (setq gnus-article-internal-prepare-hook '(gnus-article-get-xrefs))
  (let ((cur nntp-server-buffer)
	number headers header)
    (save-excursion
      (set-buffer nntp-server-buffer)
      ;; Allow the user to mangle the headers before parsing them.
      (gnus-run-hooks 'gnus-parse-headers-hook)
      (goto-char (point-min))
      (while (not (eobp))
	(condition-case ()
	    (while (and sequence (not (eobp)))
	      (setq number (read cur))
	      (while (and sequence
			  (< (car sequence) number))
		(setq sequence (cdr sequence)))
	      (and sequence
		   (eq number (car sequence))
		   (progn
		     (setq sequence (cdr sequence))
		     (setq header (inline
				    (gnus-nov-parse-line
				     number dependencies force-new))))
		   (push header headers))
	      (forward-line 1))
	  (error
	   (gnus-error 4 "Strange nov line (%d)"
		       (count-lines (point-min) (point)))))
	(forward-line 1))
      ;; A common bug in inn is that if you have posted an article and
      ;; then retrieves the active file, it will answer correctly --
      ;; the new article is included.  However, a NOV entry for the
      ;; article may not have been generated yet, so this may fail.
      ;; We work around this problem by retrieving the last few
      ;; headers using HEAD.
      headers)))

;; Various cruft the backends and Gnus need to communicate.

(defvar nntp-server-buffer nil)
(defvar nntp-process-response nil)
(defvar gnus-verbose-backends 7
  "*A number that says how talkative the Gnus backends should be.")
(defvar gnus-nov-is-evil nil
  "If non-nil, Gnus backends will never output headers in the NOV format.")
(defvar news-reply-yank-from nil)
(defvar news-reply-yank-message-id nil)

(defvar nnheader-callback-function nil)

(defun nnheader-init-server-buffer ()
  "Initialize the Gnus-backend communication buffer."
  (save-excursion
    (unless (gnus-buffer-live-p nntp-server-buffer)
      (setq nntp-server-buffer (get-buffer-create " *nntpd*")))
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    (set (make-local-variable 'nntp-process-response) nil)
    t))

;;; Various functions the backends use.

(defun nnheader-file-error (file)
  "Return a string that says what is wrong with FILE."
  (format
   (cond
    ((not (file-exists-p file))
     "%s does not exist")
    ((file-directory-p file)
     "%s is a directory")
    ((not (file-readable-p file))
     "%s is not readable"))
   file))

(defun nnheader-insert-head (file)
  "Insert the head of the article."
  (when (file-exists-p file)
    (if (eq nnheader-max-head-length t)
	;; Just read the entire file.
	(nnheader-insert-file-contents file)
      ;; Read 1K blocks until we find a separator.
      (let ((beg 0)
	    format-alist)
	(while (and (eq nnheader-head-chop-length
			(nth 1 (nnheader-insert-file-contents
				file nil beg
				(incf beg nnheader-head-chop-length))))
		    (prog1 (not (search-forward "\n\n" nil t))
		      (goto-char (point-max)))
		    (or (null nnheader-max-head-length)
			(< beg nnheader-max-head-length))))))
    t))

(defun nnheader-article-p ()
  "Say whether the current buffer looks like an article."
  (goto-char (point-min))
  (if (not (search-forward "\n\n" nil t))
      nil
    (narrow-to-region (point-min) (1- (point)))
    (goto-char (point-min))
    (while (looking-at "[a-zA-Z][^ \t]+:.*\n\\([ \t].*\n\\)*\\|From .*\n")
      (goto-char (match-end 0)))
    (prog1
	(eobp)
      (widen))))

(defun nnheader-insert-references (references message-id)
  "Insert a References header based on REFERENCES and MESSAGE-ID."
  (if (and (not references) (not message-id))
      ;; This is invalid, but not all articles have Message-IDs.
      ()
    (mail-position-on-field "References")
    (let ((begin (save-excursion (beginning-of-line) (point)))
	  (fill-column 78)
	  (fill-prefix "\t"))
      (when references
	(insert references))
      (when (and references message-id)
	(insert " "))
      (when message-id
	(insert message-id))
      ;; Fold long References lines to conform to RFC1036 (sort of).
      ;; The region must end with a newline to fill the region
      ;; without inserting extra newline.
      (fill-region-as-paragraph begin (1+ (point))))))

(defun nnheader-replace-header (header new-value)
  "Remove HEADER and insert the NEW-VALUE."
  (save-excursion
    (save-restriction
      (nnheader-narrow-to-headers)
      (prog1
	  (message-remove-header header)
	(goto-char (point-max))
	(insert header ": " new-value "\n")))))

(defun nnheader-narrow-to-headers ()
  "Narrow to the head of an article."
  (widen)
  (narrow-to-region
   (goto-char (point-min))
   (if (search-forward "\n\n" nil t)
       (1- (point))
     (point-max)))
  (goto-char (point-min)))

(defun nnheader-set-temp-buffer (name &optional noerase)
  "Set-buffer to an empty (possibly new) buffer called NAME with undo disabled."
  (set-buffer (get-buffer-create name))
  (buffer-disable-undo)
  (unless noerase
    (erase-buffer))
  (current-buffer))

(eval-when-compile (defvar jka-compr-compression-info-list))
(defvar nnheader-numerical-files
  (if (boundp 'jka-compr-compression-info-list)
      (concat "\\([0-9]+\\)\\("
	      (mapconcat (lambda (i) (aref i 0))
			 jka-compr-compression-info-list "\\|")
	      "\\)?")
    "[0-9]+$")
  "Regexp that match numerical files.")

(defvar nnheader-numerical-short-files (concat "^" nnheader-numerical-files)
  "Regexp that matches numerical file names.")

(defvar nnheader-numerical-full-files (concat "/" nnheader-numerical-files)
  "Regexp that matches numerical full file paths.")

(defsubst nnheader-file-to-number (file)
  "Take a FILE name and return the article number."
  (if (string= nnheader-numerical-short-files "^[0-9]+$")
      (string-to-int file)
    (string-match nnheader-numerical-short-files file)
    (string-to-int (match-string 0 file))))

(defun nnheader-directory-files-safe (&rest args)
  ;; It has been reported numerous times that `directory-files'
  ;; fails with an alarming frequency on NFS mounted file systems.
  ;; This function executes that function twice and returns
  ;; the longest result.
  (let ((first (apply 'directory-files args))
	(second (apply 'directory-files args)))
    (if (> (length first) (length second))
	first
      second)))

(defun nnheader-directory-articles (dir)
  "Return a list of all article files in directory DIR."
  (mapcar 'nnheader-file-to-number
	  (nnheader-directory-files-safe
	   dir nil nnheader-numerical-short-files t)))

(defun nnheader-article-to-file-alist (dir)
  "Return an alist of article/file pairs in DIR."
  (mapcar (lambda (file) (cons (nnheader-file-to-number file) file))
	  (nnheader-directory-files-safe
	   dir nil nnheader-numerical-short-files t)))

(defun nnheader-fold-continuation-lines ()
  "Fold continuation lines in the current buffer."
  (nnheader-replace-regexp "\\(\r?\n[ \t]+\\)+" " "))

(defun nnheader-translate-file-chars (file &optional full)
  "Translate FILE into something that can be a file name.
If FULL, translate everything."
  (if (null nnheader-file-name-translation-alist)
      ;; No translation is necessary.
      file
    (let* ((i 0)
	   trans leaf path len)
      (if full
	  ;; Do complete translation.
	  (setq leaf (copy-sequence file)
		path ""
		i (if (and (< 1 (length leaf)) (eq ?: (aref leaf 1)))
		      2 0))
	;; We translate -- but only the file name.  We leave the directory
	;; alone.
	(if (memq system-type '(win32 w32 mswindows windows-nt))
	    ;; This is needed on NT and stuff, because
	    ;; file-name-nondirectory is not enough to split
	    ;; file names, containing ':', e.g.
	    ;; "d:\\Work\\News\\nntp+news.fido7.ru:fido7.ru.gnu.SCORE"
	    ;; 
	    ;; we are trying to correctly split such names:
	    ;; "d:file.name" -> "a:" "file.name"
	    ;; "aaa:bbb.ccc" -> "" "aaa:bbb.ccc"
	    ;; "d:aaa\\bbb:ccc"   -> "d:aaa\\" "bbb:ccc"
	    ;; etc.
	    ;; to translate then only the file name part.
	    (progn
	      (setq leaf file
		    path "")
	      (if (string-match "\\(^\\w:\\|[/\\]\\)\\([^/\\]+\\)$" file)
		  (setq leaf (substring file (match-beginning 2))
			path (substring file 0 (match-beginning 2)))))
	  ;; Fall back on this.
	  (setq leaf (file-name-nondirectory file)
		path (file-name-directory file))))
      (setq len (length leaf))
      (while (< i len)
	(when (setq trans (cdr (assq (aref leaf i)
				     nnheader-file-name-translation-alist)))
	  (aset leaf i trans))
	(incf i))
      (concat path leaf))))

(defun nnheader-report (backend &rest args)
  "Report an error from the BACKEND.
The first string in ARGS can be a format string."
  (set (intern (format "%s-status-string" backend))
       (if (< (length args) 2)
	   (car args)
	 (apply 'format args)))
  nil)

(defun nnheader-get-report (backend)
  "Get the most recent report from BACKEND."
  (condition-case ()
      (nnheader-message 5 "%s" (symbol-value (intern (format "%s-status-string"
							     backend))))
    (error (nnheader-message 5 ""))))

(defun nnheader-insert (format &rest args)
  "Clear the communication buffer and insert FORMAT and ARGS into the buffer.
If FORMAT isn't a format string, it and all ARGS will be inserted
without formatting."
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (if (string-match "%" format)
	(insert (apply 'format format args))
      (apply 'insert format args))
    t))

(static-if (fboundp 'subst-char-in-string)
    (defsubst nnheader-replace-chars-in-string (string from to)
      (subst-char-in-string from to string))
  (defun nnheader-replace-chars-in-string (string from to)
    "Replace characters in STRING from FROM to TO."
    (let ((string (substring string 0))	;Copy string.
	  (len (length string))
	  (idx 0))
      ;; Replace all occurrences of FROM with TO.
      (while (< idx len)
	(when (= (aref string idx) from)
	  (aset string idx to))
	(setq idx (1+ idx)))
      string)))

(defun nnheader-replace-duplicate-chars-in-string (string from to)
  "Replace characters in STRING from FROM to TO."
  (let ((string (substring string 0))	;Copy string.
	(len (length string))
	(idx 0) prev i)
    ;; Replace all occurrences of FROM with TO.
    (while (< idx len)
      (setq i (aref string idx))
      (when (and (eq prev from) (= i from))
	(aset string (1- idx) to)
	(aset string idx to))
      (setq prev i)
      (setq idx (1+ idx)))
    string))

(defun nnheader-file-to-group (file &optional top)
  "Return a group name based on FILE and TOP."
  (nnheader-replace-chars-in-string
   (if (not top)
       file
     (condition-case ()
	 (substring (expand-file-name file)
		    (length
		     (expand-file-name
		      (file-name-as-directory top))))
       (error "")))
   ?/ ?.))

(defun nnheader-message (level &rest args)
  "Message if the Gnus backends are talkative."
  (if (or (not (numberp gnus-verbose-backends))
	  (<= level gnus-verbose-backends))
      (apply 'message args)
    (apply 'format args)))

(defun nnheader-be-verbose (level)
  "Return whether the backends should be verbose on LEVEL."
  (or (not (numberp gnus-verbose-backends))
      (<= level gnus-verbose-backends)))

(defvar nnheader-pathname-coding-system 'binary
  "*Coding system for pathname.")

(defun nnheader-group-pathname (group dir &optional file)
  "Make pathname for GROUP."
  (concat
   (let ((dir (file-name-as-directory (expand-file-name dir))))
     ;; If this directory exists, we use it directly.
     (file-name-as-directory
      (if (file-directory-p (concat dir group))
	  (expand-file-name group dir)
	;; If not, we translate dots into slashes.
	(expand-file-name (encode-coding-string
			   (nnheader-replace-chars-in-string group ?. ?/)
			   nnheader-pathname-coding-system)
			  dir))))
   (cond ((null file) "")
	 ((numberp file) (int-to-string file))
	 (t file))))

(defun nnheader-functionp (form)
  "Return non-nil if FORM is funcallable."
  (or (and (symbolp form) (fboundp form))
      (and (listp form) (eq (car form) 'lambda))))

(defun nnheader-concat (dir &rest files)
  "Concat DIR as directory to FILES."
  (apply 'concat (file-name-as-directory dir) files))

(defun nnheader-ms-strip-cr ()
  "Strip ^M from the end of all lines."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\r$" nil t)
      (delete-backward-char 1))))

(defun nnheader-file-size (file)
  "Return the file size of FILE or 0."
  (or (nth 7 (file-attributes file)) 0))

(defun nnheader-find-etc-directory (package &optional file)
  "Go through the path and find the \".../etc/PACKAGE\" directory.
If FILE, find the \".../etc/PACKAGE\" file instead."
  (let ((path load-path)
	dir result)
    ;; We try to find the dir by looking at the load path,
    ;; stripping away the last component and adding "etc/".
    (while path
      (if (and (car path)
	       (file-exists-p
		(setq dir (concat
			   (file-name-directory
			    (directory-file-name (car path)))
			   "etc/" package
			   (if file "" "/"))))
	       (or file (file-directory-p dir)))
	  (setq result dir
		path nil)
	(setq path (cdr path))))
    result))

(defvar ange-ftp-path-format)
(defvar efs-path-regexp)
(defun nnheader-re-read-dir (path)
  "Re-read directory PATH if PATH is on a remote system."
  (if (and (fboundp 'efs-re-read-dir) (boundp 'efs-path-regexp))
      (when (string-match efs-path-regexp path)
	(efs-re-read-dir path))
    (when (and (fboundp 'ange-ftp-re-read-dir) (boundp 'ange-ftp-path-format))
      (when (string-match (car ange-ftp-path-format) path)
	(ange-ftp-re-read-dir path)))))

(defvar nnheader-file-coding-system 'raw-text
  "Coding system used in file backends of Gnus.")

(defun nnheader-insert-file-contents (filename &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
  (let ((format-alist nil)
	(auto-mode-alist (nnheader-auto-mode-alist))
	(default-major-mode 'fundamental-mode)
	(enable-local-variables nil)
        (after-insert-file-functions nil)
	(enable-local-eval nil)
	(find-file-hooks nil))
    (insert-file-contents-as-coding-system
     nnheader-file-coding-system filename visit beg end replace)))

(defun nnheader-find-file-noselect (&rest args)
  (let ((format-alist nil)
	(auto-mode-alist (nnheader-auto-mode-alist))
	(default-major-mode 'fundamental-mode)
	(enable-local-variables nil)
	(after-insert-file-functions nil)
	(enable-local-eval nil)
	(find-file-hooks nil))
    (apply 'find-file-noselect-as-coding-system
	   nnheader-file-coding-system args)))

(defun nnheader-auto-mode-alist ()
  "Return an `auto-mode-alist' with only the .gz (etc) thingies."
  (let ((alist auto-mode-alist)
	out)
    (while alist
      (when (listp (cdar alist))
	(push (car alist) out))
      (pop alist))
    (nreverse out)))

(defun nnheader-directory-regular-files (dir)
  "Return a list of all regular files in DIR."
  (let ((files (directory-files dir t))
	out)
    (while files
      (when (file-regular-p (car files))
	(push (car files) out))
      (pop files))
    (nreverse out)))

(defun nnheader-directory-files (&rest args)
  "Same as `directory-files', but prune \".\" and \"..\"."
  (let ((files (apply 'directory-files args))
	out)
    (while files
      (unless (member (file-name-nondirectory (car files)) '("." ".."))
	(push (car files) out))
      (pop files))
    (nreverse out)))

(defmacro nnheader-skeleton-replace (from &optional to regexp)
  `(let ((new (generate-new-buffer " *nnheader replace*"))
	 (cur (current-buffer))
	 (start (point-min)))
     (set-buffer cur)
     (goto-char (point-min))
     (while (,(if regexp 're-search-forward 'search-forward)
	     ,from nil t)
       (insert-buffer-substring
	cur start (prog1 (match-beginning 0) (set-buffer new)))
       (goto-char (point-max))
       ,(when to `(insert ,to))
       (set-buffer cur)
       (setq start (point)))
     (insert-buffer-substring
      cur start (prog1 (point-max) (set-buffer new)))
     (copy-to-buffer cur (point-min) (point-max))
     (kill-buffer (current-buffer))
     (set-buffer cur)))

(defun nnheader-replace-string (from to)
  "Do a fast replacement of FROM to TO from point to `point-max'."
  (nnheader-skeleton-replace from to))

(defun nnheader-replace-regexp (from to)
  "Do a fast regexp replacement of FROM to TO from point to `point-max'."
  (nnheader-skeleton-replace from to t))

(defun nnheader-strip-cr ()
  "Strip all \r's from the current buffer."
  (nnheader-skeleton-replace "\r"))

(defalias 'nnheader-run-at-time 'run-at-time)
(defalias 'nnheader-cancel-timer 'cancel-timer)
(defalias 'nnheader-cancel-function-timers 'cancel-function-timers)

(defun nnheader-Y-or-n-p (prompt)
  "Ask user a \"Y/n\" question. Return t if answer is neither \"n\", \"N\" nor \"C-g\"."
  (let ((cursor-in-echo-area t)
	(echo-keystrokes 0)
	(inhibit-quit t)
	ans)
    (let (message-log-max)
      (while (not (memq ans '(?\  ?N ?Y ?\C-g ?\e ?\n ?\r ?n ?y)))
	(message "%s(Y/n) " prompt)
	(setq ans (read-char-exclusive))))
    (if (memq ans '(?\C-g ?N ?n))
	(progn
	  (message "%s(Y/n) No" prompt)
	  nil)
      (message "%s(Y/n) Yes" prompt)
      t)))

(when (featurep 'xemacs)
  (require 'nnheaderxm))

(run-hooks 'nnheader-load-hook)

(provide 'nnheader)

;;; nnheader.el ends here

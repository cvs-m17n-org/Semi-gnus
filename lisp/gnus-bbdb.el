;; gnus-bbdb.el --- Interface to T-gnus

;; Copyright (c) 1991,1992,1993 Jamie Zawinski <jwz@netscape.com>.
;; Copyright (C) 1995,1996,1997 Shuhei KOBAYASHI
;; Copyright (C) 1997,1998 MORIOKA Tomohiko
;; Copyright (C) 1998,1999 Keiichi Suzuki <keiichi@nanap.org>

;; Author: Keiichi Suzuki <keiichi@nanap.org>
;; Author: Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;; Keywords: BBDB, MIME, multimedia, multilingual, mail, news

;; This file is part of T-gnus.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'bbdb)
(require 'bbdb-com)
(require 'gnus)
(require 'std11)
(eval-when-compile
  (defvar bbdb-pop-up-elided-display)	; default unbound.
  (require 'gnus-win)
  (require 'cl))

(defvar gnus-bbdb/decode-field-body-function 'nnheader-decode-field-body
  "*Field body decoder.")

(defmacro gnus-bbdb/decode-field-body (field-body field-name)
  `(or (and (functionp gnus-bbdb/decode-field-body-function)
	    (funcall gnus-bbdb/decode-field-body-function
		     ,field-body ,field-name))
       ,field-body))

;;;###autoload
(defun gnus-bbdb/update-record (&optional offer-to-create)
  "returns the record corresponding to the current GNUS message, creating
or modifying it as necessary.  A record will be created if
bbdb/news-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation."
  (if bbdb-use-pop-up
      (gnus-bbdb/pop-up-bbdb-buffer offer-to-create)
    (let ((from (mime-entity-fetch-field gnus-current-headers "from")))
      (when from
	(setq from (gnus-bbdb/extract-address-components
		    (gnus-bbdb/decode-field-body from 'From))))
      (when (and (car (cdr from))
		 (string-match (bbdb-user-mail-names)
			       (car (cdr from))))
	;; if logged-in user sent this, use recipients.
	(let ((to (mime-entity-fetch-field gnus-current-headers "to")))
	  (when to
	    (setq from
		  (gnus-bbdb/extract-address-components
		   (gnus-bbdb/decode-field-body to 'To))))))
      (when from
	(save-excursion
	  (bbdb-annotate-message-sender from t
					(or (bbdb-invoke-hook-for-value
					     bbdb/news-auto-create-p)
					    offer-to-create)
					offer-to-create))))))

;;;###autoload
(defun gnus-bbdb/annotate-sender (string &optional replace)
  "Add a line to the end of the Notes field of the BBDB record 
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any)."
  (interactive (list (if bbdb-readonly-p
			 (error "The Insidious Big Brother Database is read-only.")
		       (read-string "Comments: "))))
  (bbdb-annotate-notes (gnus-bbdb/update-record t) string 'notes replace))

(defun gnus-bbdb/edit-notes (&optional arg)
  "Edit the notes field or (with a prefix arg) a user-defined field
of the BBDB record corresponding to the sender of this message."
  (interactive "P")
  (let ((record (or (gnus-bbdb/update-record t) (error ""))))
    (bbdb-display-records (list record))
    (if arg
	(bbdb-record-edit-property record nil t)
      (bbdb-record-edit-notes record t))))

;;;###autoload
(defun gnus-bbdb/show-sender ()
  "Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings."
  (interactive)
  (let ((record (gnus-bbdb/update-record t)))
    (if record
	(bbdb-display-records (list record))
	(error "unperson"))))


(defun gnus-bbdb/pop-up-bbdb-buffer (&optional offer-to-create)
  "Make the *BBDB* buffer be displayed along with the GNUS windows,
displaying the record corresponding to the sender of the current message."
  (let* ((bbdb-gag-messages t)
	 (bbdb-electric-p nil)
	 (record
	  (let (bbdb-use-pop-up)
	    (gnus-bbdb/update-record offer-to-create)))
	 (bbdb-display-layout
	  (cond ((boundp 'bbdb-pop-up-display-layout)
		 (symbol-value 'bbdb-pop-up-display-layout))
		((boundp 'bbdb-pop-up-elided-display)
		 (symbol-value 'bbdb-pop-up-elided-display))))
	 (bbdb-elided-display bbdb-display-layout))
    (save-current-buffer
      ;; display the bbdb buffer iff there is a record for this article.
      (cond
       (record
	(bbdb-pop-up-bbdb-buffer
	 (lambda (w)
	   (with-current-buffer (window-buffer w)
	     (memq major-mode
		   '(mime-view-mode gnus-article-mode)))))
	(bbdb-display-records (list record)))
       ((and (not bbdb-inside-electric-display)
	     (get-buffer-window bbdb-buffer-name))
	(delete-other-windows)
	(if (assq 'article gnus-buffer-configuration)
	    (gnus-configure-windows 'article)
	  (gnus-configure-windows 'SelectArticle))
	(let ((w (get-buffer-window gnus-summary-buffer)))
	  (if w (select-window w))))))
    record))

;;;###autoload
(defun gnus-bbdb/split-mail (header-field bbdb-field
					  &optional regexp group)
  "Mail split method for `nnmail-split-fancy'.
HEADER-FIELD is a regexp or list of regexps as mail header field name
for gathering mail addresses.  If HEADER-FIELD is a string, then it's
used for just matching pattern.  If HEADER-FIELD is a list of strings,
then these strings have priorities in the order.

BBDB-FIELD is field name of BBDB.
Optional argument REGEXP is regexp string for matching BBDB-FIELD value.
If REGEXP is nil or not specified, then all BBDB-FIELD value is matched.

If GROUP is nil or not specified, then BBDB-FIELD value is returned as
group name.  If GROUP is a symbol `&', then list of all matching group's
BBDB-FIELD values is returned.  Otherwise, GROUP is returned."
  (if (listp header-field)
      (if (eq group '&)
	  (gnus-bbdb/split-mail (mapconcat 'identity header-field "\\|")
				bbdb-field regexp group)
	(let (rest)
	  (while (and header-field
		      (null (setq rest (gnus-bbdb/split-mail
					(car header-field) bbdb-field
					regexp group))))
	    (setq header-field (cdr header-field)))
	  rest))
    (let ((pat (concat "^\\(" header-field "\\):[ \t]"))
	  header-values)
      (goto-char (point-min))
      (while (re-search-forward pat nil t)
	(setq header-values (cons (buffer-substring (point)
						    (std11-field-end))
				  header-values)))
      (let ((address-regexp
	     (with-temp-buffer
	       (let (lal)
		 (while header-values
		   (setq lal (std11-parse-addresses-string
			      (pop header-values)))
		   (while lal
		     (gnus-bbdb/insert-address-regexp (pop lal)))))
	       (buffer-string))))
	(unless (zerop (length address-regexp))
	  (gnus-bbdb/split-mail-1 address-regexp bbdb-field regexp group))))))

(defun gnus-bbdb/insert-address-regexp (address)
  "Insert string of address part from parsed ADDRESS of RFC 822."
  (cond ((eq (car address) 'group)
	 (setq address (cdr address))
	 (while address
	   (gnus-bbdb/insert-address-regexp (pop address))))
	((eq (car address) 'mailbox)
	 (unless (eq (point) (point-min))
	   (insert "\\|"))
	 (let ((addr (nth 1 address)))
	   (insert (regexp-quote (std11-addr-to-string
				  (if (eq (car addr) 'phrase-route-addr)
				      (nth 2 addr)
				    (cdr addr)))))))))

(defun gnus-bbdb/split-mail-1 (address-regexp bbdb-field regexp group)
  (let ((records (bbdb-search (bbdb-records) nil nil address-regexp))
	prop rest)
    (or regexp (setq regexp ""))
    (catch 'done
      (cond
       ((eq group '&)
	(while records
	  (when (and (setq prop (bbdb-record-getprop (car records) bbdb-field))
		     (string-match regexp prop)
		     (not (member prop rest)))
	    (setq rest (cons prop rest)))
	  (setq records (cdr records)))
	(throw 'done (when rest (cons '& rest))))
       (t
	(while records
	  (when (or (null bbdb-field) 
		    (and (setq prop (bbdb-record-getprop (car records)
							 bbdb-field))
			 (string-match regexp prop)))
	    (throw 'done (or group prop)))
	  (setq records (cdr records))))))))

;;
;; Announcing BBDB entries in the summary buffer
;;

(defcustom gnus-bbdb/lines-and-from-length 18
  "*The number of characters used to display From: info in Gnus, if you have
set gnus-optional-headers to 'gnus-bbdb/lines-and-from."
  :group 'bbdb-mua-specific-gnus
  :type 'integer)

(defcustom gnus-bbdb/summary-mark-known-posters t
  "*If t, mark messages created by people with records in the BBDB.
In GNUS, this marking will take place in the subject list (assuming
`gnus-optional-headers' contains `gnus-bbdb/lines-and-from').  In Gnus, the
marking will take place in the Summary buffer if the format code defined by
`gnus-bbdb/summary-user-format-letter' is used in `gnus-summary-line-format'.
This variable has no effect on the marking controlled by
`gnus-bbdb/summary-in-bbdb-format-letter'."
  :group 'bbdb-mua-specific-gnus
  :type '(choice (const :tag "Mark known posters" t)
		 (const :tag "Do not mark known posters" nil)))
(static-when (and (fboundp 'defvaralias)
		  (subrp (symbol-function 'defvaralias)))
  (defvaralias 'gnus-bbdb/mark-known-posters
    'gnus-bbdb/summary-mark-known-posters))

(defcustom gnus-bbdb/summary-known-poster-mark "+"
  "This is the default character to prefix author names with if
gnus-bbdb/summary-mark-known-posters is t.  If the poster's record has
an entry in the field named by bbdb-message-marker-field, then that will
be used instead."
  :group 'bbdb-mua-specific-gnus
  :type 'character)

(defcustom gnus-bbdb/summary-show-bbdb-names t
  "*If both this variable and `gnus-bbdb/summary-prefer-real-names' are true,
then for messages from authors who are in your database, the name
displayed will be the primary name in the database, rather than the
one in the From line of the message.  This doesn't affect the names of
people who aren't in the database, of course.  (`gnus-optional-headers'
must be `gnus-bbdb/lines-and-from' for GNUS users.)"
  :group 'bbdb-mua-specific-gnus
  :type 'boolean)
(static-when (and (fboundp 'defvaralias)
		  (subrp (symbol-function 'defvaralias)))
  (defvaralias 'gnus-bbdb/header-show-bbdb-names
    'gnus-bbdb/summary-show-bbdb-names))

(defcustom gnus-bbdb/summary-prefer-bbdb-data t
  "If t, then for posters who are in our BBDB, replace the information
provided in the From header with data from the BBDB."
  :group 'bbdb-mua-specific-gnus
  :type 'boolean)

(defcustom gnus-bbdb/summary-prefer-real-names t
  "If t, then display the poster's name from the BBDB if we have one,
otherwise display his/her primary net address if we have one.  If it
is set to the symbol bbdb, then real names will be used from the BBDB
if present, otherwise the net address in the post will be used.  If
gnus-bbdb/summary-prefer-bbdb-data is nil, then this has no effect.
See `gnus-bbdb/lines-and-from' for GNUS users, or
`gnus-bbdb/summary-user-format-letter' for Gnus users."
  :group 'bbdb-mua-specific-gnus
  :type '(choice (const :tag "Prefer real names" t)
		 (const :tag "Prefer network addresses" nil)))
(static-when (and (fboundp 'defvaralias)
		  (subrp (symbol-function 'defvaralias)))
  (defvaralias 'gnus-bbdb/header-prefer-real-names
    'gnus-bbdb/summary-prefer-real-names))

(defcustom gnus-bbdb/summary-user-format-letter "B"
  "This is the gnus-user-format-function- that will be used to insert
the information from the BBDB in the summary buffer (using
`gnus-bbdb/summary-get-author').  This format code is meant to replace
codes that insert sender names or addresses (like %A or %n). Unless
you've alread got other code using user format B, you might as well
stick with the default.  Additionally, if the value of this variable
is nil, no format function will be installed for
`gnus-bbdb/summary-get-author'.  See also
`gnus-bbdb/summary-in-bbdb-format-letter', which installs a format
code for `gnus-bbdb/summary-author-in-bbdb'"
  :group 'bbdb-mua-specific-gnus
  :type 'character)

(defcustom gnus-bbdb/summary-in-bbdb-format-letter "b"
  "This is the gnus-user-format-function- that will be used to insert
`gnus-bbdb/summary-known-poster-mark' (using
`gnus-bbdb/summary-author-in-bbdb') if the poster is in the BBDB, and
\" \" if not.  If the value of this variable is nil, no format code
will be installed for `gnus-bbdb/summary-author-in-bbdb'.  See also
`gnus-bbdb/summary-user-format-letter', which installs a format code
for `gnus-bbdb/summary-get-author'."
  :group 'bbdb-mua-specific-gnus
  :type 'character)

(defcustom bbdb-message-marker-field 'mark-char
  "*The field whose value will be used to mark messages by this user in Gnus."
  :group 'bbdb-mua-specific-gnus
  :type 'symbol)

;;;###autoload
(defun gnus-bbdb/lines-and-from (header)
  "Useful as the value of gnus-optional-headers in *GNUS* (not Gnus).
NOTE: This variable no longer seems to be present in Gnus.  It seems
to have been replaced by `message-default-headers', which only takes
strings.  In the future this should change."
  (let* ((length gnus-bbdb/lines-and-from-length)
	 (lines (mail-header-lines header))
	 (from (mail-header-from header))
	 (data (and (or gnus-bbdb/summary-mark-known-posters
			gnus-bbdb/summary-show-bbdb-names)
		    (condition-case ()
			(gnus-bbdb/extract-address-components from)
		      (error nil))))
	 (name (car data))
	 (net (car (cdr data)))
	 (record (and data 
		      (bbdb-search-simple name 
		       (if (and net bbdb-canonicalize-net-hook)
			   (bbdb-canonicalize-address net)
			 net))))
	 string L)

    (if (and record name (member (downcase name) (bbdb-record-net record)))
	;; bogon!
	(setq record nil))

    (setq name 
	  (or (and gnus-bbdb/summary-prefer-bbdb-data
		   (or (and gnus-bbdb/summary-prefer-real-names
			    (and record (bbdb-record-name record)))
		       (and record (bbdb-record-net record)
			    (nth 0 (bbdb-record-net record)))))
	      (and gnus-bbdb/summary-prefer-real-names
		   (or (and (equal gnus-bbdb/summary-prefer-real-names 'bbdb)
			    net)
		       name))
	      net from "**UNKNOWN**"))
      ;; GNUS can't cope with extra square-brackets appearing in the summary.
      (if (and name (string-match "[][]" name))
	  (progn (setq name (copy-sequence name))
		 (while (string-match "[][]" name)
		   (aset name (match-beginning 0) ? ))))
      (setq string (format "%s%3d:%s"
			   (if (and record gnus-bbdb/summary-mark-known-posters)
			       (or (bbdb-record-getprop
				    record bbdb-message-marker-field)
				   "*")
			     " ")
			   lines (or name from))
	    L (length string))
      (cond ((> L length) (substring string 0 length))
	    ((< L length) (concat string (make-string (- length L) ? )))
	    (t string))))

(defun gnus-bbdb/summary-get-author (header)
  "Given a Gnus message header, returns the appropriate piece of
information to identify the author in a Gnus summary line, depending on
the settings of the various configuration variables.  See the
documentation for the following variables for more details:
  `gnus-bbdb/summary-mark-known-posters'
  `gnus-bbdb/summary-known-poster-mark'
  `gnus-bbdb/summary-prefer-bbdb-data'
  `gnus-bbdb/summary-prefer-real-names'
This function is meant to be used with the user function defined in
  `gnus-bbdb/summary-user-format-letter'"
  (let* ((from (mail-header-from header))
	 (data (and gnus-bbdb/summary-show-bbdb-names
		    (condition-case ()
			(gnus-bbdb/extract-address-components from)
		      (error nil))))
	 (name (car data))
	 (net (car (cdr data)))
	 (record (and data 
		      (bbdb-search-simple name 
		       (if (and net bbdb-canonicalize-net-hook)
			   (bbdb-canonicalize-address net)
			 net)))))
    (if (and record name (member (downcase name) (bbdb-record-net record)))
	;; bogon!
	(setq record nil))
    (setq name 
	  (or (and gnus-bbdb/summary-prefer-bbdb-data
		   (or (and gnus-bbdb/summary-prefer-real-names
			    (and record (bbdb-record-name record)))
		       (and record (bbdb-record-net record)
			    (nth 0 (bbdb-record-net record)))))
	      (and gnus-bbdb/summary-prefer-real-names
		   (or (and (equal gnus-bbdb/summary-prefer-real-names 'bbdb)
			    net)
		       name))
	      net from "**UNKNOWN**"))
    (format "%s%s"
	    (or (and record gnus-bbdb/summary-mark-known-posters
		     (or (bbdb-record-getprop
			  record bbdb-message-marker-field)
			 gnus-bbdb/summary-known-poster-mark))
		" ")
	    name)))

;; DEBUG: (gnus-bbdb/summary-author-in-bbdb "From: simmonmt@acm.org")
(defun gnus-bbdb/summary-author-in-bbdb (header)
  "Given a Gnus message header, returns a mark if the poster is in the BBDB, \" \" otherwise.  The mark itself is the value of the field indicated by `bbdb-message-marker-field' (`mark-char' by default) if the indicated field is in the poster's record, and `gnus-bbdb/summary-known-poster-mark' otherwise."
  (let* ((from (mail-header-from header))
	 (data (condition-case ()
		   (gnus-bbdb/extract-address-components from)
		 (error nil)))
	 (name (car data))
	 (net (cadr data))
	 record)
    (if (and data
	     (setq record
		   (bbdb-search-simple
		    name (if (and net bbdb-canonicalize-net-hook)
			     (bbdb-canonicalize-address net)
			   net))))
	(or (bbdb-record-getprop
	     record bbdb-message-marker-field)
	    gnus-bbdb/summary-known-poster-mark) " ")))

;;
;; Scoring
;;

(defcustom gnus-bbdb/score-field 'gnus-score
  "This variable contains the name of the BBDB field which should be
checked for a score to add to the net addresses in the same record."
  :group 'bbdb-mua-specific-gnus-scoring
  :type 'symbol)

(defcustom gnus-bbdb/score-default nil
  "If this is set, then every net address in the BBDB that does not have
an associated score field will be assigned this score.  A value of nil
implies a default score of zero."
  :group 'bbdb-mua-specific-gnus-scoring
  :type '(choice (const :tag "Do not assign default score")
		 (integer :tag "Assign this default score" 0)))

(defvar gnus-bbdb/score-default-internal nil
  "Internal variable for detecting changes to
`gnus-bbdb/score-default'.  You should not set this variable directly -
set `gnus-bbdb/score-default' instead.")

(defvar gnus-bbdb/score-alist nil
  "The text version of the scoring structure returned by
gnus-bbdb/score.  This is built automatically from the BBDB.")

(defvar gnus-bbdb/score-rebuild-alist t
  "Set to t to rebuild gnus-bbdb/score-alist on the next call to
gnus-bbdb/score.  This will be set automatically if you change a BBDB
record which contains a gnus-score field.")

(defun gnus-bbdb/score-invalidate-alist (rec)
  "This function is called through bbdb-after-change-hook, and sets
gnus-bbdb/score-rebuild-alist to t if the changed record contains a
gnus-score field."
  (if (bbdb-record-getprop rec gnus-bbdb/score-field)
      (setq gnus-bbdb/score-rebuild-alist t)))

;;;###autoload
(defun gnus-bbdb/score (group)
  "This returns a score alist for GNUS.  A score pair will be made for
every member of the net field in records which also have a gnus-score
field.  This allows the BBDB to serve as a supplemental global score
file, with the advantage that it can keep up with multiple and changing
addresses better than the traditionally static global scorefile."
  (list (list
   (condition-case nil
       (read (gnus-bbdb/score-as-text group))
     (error (setq gnus-bbdb/score-rebuild-alist t)
	    (message "Problem building BBDB score table.")
	    (ding) (sit-for 2)
	    nil)))))

(defun gnus-bbdb/score-as-text (group)
  "Returns a SCORE file format string built from the BBDB."
  (cond ((or (cond ((/= (or gnus-bbdb/score-default 0)
			(or gnus-bbdb/score-default-internal 0))
		    (setq gnus-bbdb/score-default-internal
			  gnus-bbdb/score-default)
		    t))
	    (not gnus-bbdb/score-alist)
	    gnus-bbdb/score-rebuild-alist)
    (setq gnus-bbdb/score-rebuild-alist nil)
    (setq gnus-bbdb/score-alist
	  (concat "((touched nil) (\"from\"\n"
		  (mapconcat
		   (lambda (rec)
		     (let ((score (or (bbdb-record-getprop rec
							   gnus-bbdb/score-field)
				      gnus-bbdb/score-default))
			   (net (bbdb-record-net rec)))
		       (if (not (and score net)) nil
			 (mapconcat
			  (lambda (addr)
			    (concat "(\"" addr "\" " score ")\n"))
			  net ""))))
		   (bbdb-records) "")
		  "))"))))
  gnus-bbdb/score-alist)

(defun gnus-bbdb/extract-field-value-init ()
  (function gnus-bbdb/extract-field-value))

(defun gnus-bbdb/extract-field-value (field-name)
  "Given the name of a field (like \"Subject\") this returns the value of
that field in the current message, or nil.  This works whether you're in
Semi-gnus, Rmail, or VM.  This works on multi-line fields, but if more than
one field of the same name is present, only the last is returned.  It is
expected that the current buffer has a message in it, and (point) is at the
beginning of the message headers."
  ;; we can't special-case VM here to use its cache, because the cache has
  ;; divided real-names from addresses; the actual From: and Subject: fields
  ;; exist only in the message.
  (let (value)
    (when (setq value (mime-entity-fetch-field
		       gnus-current-headers field-name))
      (gnus-bbdb/decode-field-body value field-name))))

;;; @ mail-extr
;;;

(defvar gnus-bbdb/canonicalize-full-name-methods
  '(gnus-bbdb/canonicalize-dots
    gnus-bbdb/canonicalize-spaces))

(defun gnus-bbdb/extract-address-components (str)
  (let* ((ret     (std11-extract-address-components str))
         (phrase  (car ret))
         (address (car (cdr ret)))
         (methods gnus-bbdb/canonicalize-full-name-methods))
    (while (and phrase methods)
      (setq phrase  (funcall (car methods) phrase)
            methods (cdr methods)))
    (if (string= address "") (setq address nil))
    (if (string= phrase "") (setq phrase nil))
    (when (or phrase address)
      (list phrase address))))

;;; @ full-name canonicalization methods
;;;

(defun gnus-bbdb/canonicalize-spaces (str)
  (let (dest)
    (while (string-match "\\s +" str)
      (setq dest (cons (substring str 0 (match-beginning 0)) dest))
      (setq str (substring str (match-end 0))))
    (or (string= str "")
        (setq dest (cons str dest)))
    (setq dest (nreverse dest))
    (mapconcat 'identity dest " ")))

(defun gnus-bbdb/canonicalize-dots (str)
  (let (dest)
    (while (string-match "\\." str)
      (setq dest (cons (substring str 0 (match-end 0)) dest))
      (setq str (substring str (match-end 0))))
    (or (string= str "")
        (setq dest (cons str dest)))
    (setq dest (nreverse dest))
    (mapconcat 'identity dest " ")))

;;
;; Insinuation
;;

;;;###autoload
(defun gnus-bbdb-insinuate ()
  "Call this function to hook BBDB into Semi-gnus."
;;  (setq gnus-optional-headers 'gnus-bbdb/lines-and-from)
  (when (boundp 'bbdb-extract-field-value-function-list)
    (add-to-list 'bbdb-extract-field-value-function-list
		 'gnus-bbdb/extract-field-value-init))
  (add-hook 'gnus-article-prepare-hook 'gnus-bbdb/update-record)
  (add-hook 'gnus-save-newsrc-hook 'bbdb-offer-save)
  (define-key gnus-summary-mode-map ":" 'gnus-bbdb/show-sender)
  (define-key gnus-summary-mode-map ";" 'gnus-bbdb/edit-notes)

  ;; Set up user field for use in gnus-summary-line-format
  (let ((get-author-user-fun (intern
			      (concat "gnus-user-format-function-"
				      gnus-bbdb/summary-user-format-letter)))
	(in-bbdb-user-fun (intern
			   (concat "gnus-user-format-function-"
				   gnus-bbdb/summary-in-bbdb-format-letter))))
					; The big one - whole name
    (cond (gnus-bbdb/summary-user-format-letter
	   (if (and (fboundp get-author-user-fun)
		    (not (eq (symbol-function get-author-user-fun)
			     'gnus-bbdb/summary-get-author)))
	       (bbdb-warn
		(format "`gnus-user-format-function-%s' already seems to be in use.
Please redefine `gnus-bbdb/summary-user-format-letter' to a different letter."
			gnus-bbdb/summary-user-format-letter))
	     (fset get-author-user-fun 'gnus-bbdb/summary-get-author))))
    
    ; One tick.  One tick only, please
    (cond (gnus-bbdb/summary-in-bbdb-format-letter
	   (if (and (fboundp in-bbdb-user-fun)
		    (not (eq (symbol-function in-bbdb-user-fun)
			     'gnus-bbdb/summary-author-in-bbdb)))
	       (bbdb-warn
		(format "`gnus-user-format-function-%s' already seems to be in use.
Redefine `gnus-bbdb/summary-in-bbdb-format-letter' to a different letter."
			gnus-bbdb/summary-in-bbdb-format-letter))
	     (fset in-bbdb-user-fun 'gnus-bbdb/summary-author-in-bbdb)))))
  
  ;; Scoring
  (add-hook 'bbdb-after-change-hook 'gnus-bbdb/score-invalidate-alist)
;  (setq gnus-score-find-score-files-function
;	(if (boundp 'gnus-score-find-score-files-function)
;	    (cond ((functionp gnus-score-find-score-files-function)
;		   (list gnus-score-find-score-files-function
;			 'gnus-bbdb/score))
;		  ((listp gnus-score-find-score-files-function)
;		   (append gnus-score-find-score-files-function
;			   'gnus-bbdb/score))
;		  (t 'gnus-bbdb/score))
;	  'gnus-bbdb/score))
  )

;;;###autoload
(defun gnus-bbdb-insinuate-message ()
  "Call this function to hook BBDB into message-mode."
  (define-key message-mode-map "\M-\t" 'bbdb-complete-name))

(provide 'gnus-bbdb)

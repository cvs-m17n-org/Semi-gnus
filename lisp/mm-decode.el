;;; mm-decode.el --- Functions for decoding MIME things
;; Copyright (C) 1998,99 Free Software Foundation, Inc.

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

;;; Code:

(require 'mail-parse)
(require 'mm-mailcap)
(require 'mm-bodies)
(require 'mmgnus)

;;; Convenience macros.

(defsubst mm-handle-p (handle)
  (memq (luna-class-name handle)
	'(mmgnus-entity mime-gnus-entity)))
(defalias 'mm-handle-body 'mmgnus-entity-body-internal)
(defalias 'mm-handle-set-body 'mmgnus-entity-set-body-internal)
(defsubst mm-handle-multipart/mixed-p (handle)
  (string= (mime-entity-content-type-internal handle) "multipart/mixed"))
(defalias 'mm-handle-type 'mime-entity-content-type-internal)
(defsubst mm-handle-type-parameters (handle)
  (mime-content-type-parameters (mm-handle-type handle)))
(defsubst mm-handle-media-type (handle)
  (mime-type/subtype-string
   (mime-content-type-primary-type (mm-handle-type handle))
   (mime-content-type-subtype (mm-handle-type handle))))
(defsubst mm-handle-media-supertype (handle)
  (and (mime-content-type-primary-type (mm-handle-type handle))
       (symbol-name (mime-content-type-primary-type (mm-handle-type handle)))))
(defsubst mm-handle-media-subtype (handle)
  (and (mime-content-type-subtype (mm-handle-type handle))
       (symbol-name (mime-content-type-subtype (mm-handle-type handle)))))
(defsubst mm-handle-encoding (handle)
  (and (mime-entity-encoding-internal handle)
       (intern (mime-entity-encoding-internal handle))))
(defalias 'mm-handle-child 'mime-entity-children-internal)
(defalias 'mm-handle-set-child 'mime-entity-set-children-internal)
(defalias 'mm-handle-parent 'mime-entity-parent-internal)
(defalias 'mm-handle-set-parent 'mime-entity-set-parent-internal)
(defalias 'mm-handle-undisplayer 'mmgnus-entity-undisplayer-internal)
(defalias 'mm-handle-set-undisplayer 'mmgnus-entity-set-undisplayer-internal)
(defalias 'mm-handle-disposition 'mime-entity-content-disposition-internal)
(defsubst mm-handle-disposition-type (handle)
  (mime-content-disposition-type (mm-handle-disposition handle)))
(defsubst mm-handle-disposition-parameters (handle)
  (mime-content-disposition-parameters (mm-handle-disposition handle)))
(defalias 'mm-handle-description 'mmgnus-entity-content-description-internal)
(defalias 'mm-handle-cache 'mmgnus-entity-cache-internal)
(defalias 'mm-handle-set-cache 'mmgnus-entity-set-cache-internal)
(defalias 'mm-handle-id 'mmgnus-entity-content-id-internal)
(defalias 'mm-handle-header 'mmgnus-entity-header-internal)
(defalias 'mm-handle-set-header 'mmgnus-entity-set-header-internal)
(defsubst mm-make-handle (&optional parent body type encoding undisplayer
				    disposition description cache
				    id child header)
  (luna-make-entity 'mmgnus-entity
		    :parent parent
		    :body body
		    :content-type type
		    :encoding (if (and encoding
				       (symbolp encoding))
				  (symbol-name encoding)
				encoding)
		    :undisplayer undisplayer
		    :content-disposition disposition
		    :content-description description
		    :cache cache
		    :content-id id
		    :children child
		    :header header))

(defvar mm-inline-media-tests
  '(("image/jpeg"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'jpeg handle)))
    ("image/png"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'png handle)))
    ("image/gif"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'gif handle)))
    ("image/tiff"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'tiff handle)) )
    ("image/xbm"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xbm handle)))
    ("image/x-xbitmap"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xbm handle)))
    ("image/xpm"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xpm handle)))
    ("image/x-pixmap"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xpm handle)))
    ("image/bmp"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'bmp handle)))
    ("text/plain" mm-inline-text identity)
    ("text/enriched" mm-inline-text identity)
    ("text/richtext" mm-inline-text identity)
    ("text/x-patch" mm-display-patch-inline
     (lambda (handle)
       (locate-library "diff-mode")))
    ("text/html"
     mm-inline-text
     (lambda (handle)
       (locate-library "w3")))
    ("text/x-vcard"
     mm-inline-text
     (lambda (handle)
       (or (featurep 'vcard)
	   (locate-library "vcard"))))
    ("message/delivery-status" mm-inline-text identity)
    ("message/rfc822" mm-inline-message identity)
    ("text/.*" mm-inline-text identity)
    ("audio/wav" mm-inline-audio
     (lambda (handle)
       (and (or (featurep 'nas-sound) (featurep 'native-sound))
	    (device-sound-enabled-p))))
    ("audio/au"
     mm-inline-audio
     (lambda (handle)
       (and (or (featurep 'nas-sound) (featurep 'native-sound))
	    (device-sound-enabled-p))))
    ("application/pgp-signature" ignore identity)
    ("multipart/alternative" ignore identity)
    ("multipart/mixed" ignore identity)
    ("multipart/related" ignore identity))
  "Alist of media types/test that say whether the media types can be displayed inline.")

(defvar mm-inlined-types
  '("image/.*" "text/.*" "message/delivery-status" "message/rfc822"
    "application/pgp-signature")
  "List of media types that are to be displayed inline.")
  
(defvar mm-automatic-display
  '("text/plain" "text/enriched" "text/richtext" "text/html"
    "text/x-vcard" "image/.*" "message/delivery-status" "multipart/.*"
    "message/rfc822" "text/x-patch" "application/pgp-signature")
  "A list of MIME types to be displayed automatically.")

(defvar mm-attachment-override-types '("text/x-vcard")
  "Types that should have \"attachment\" ignored if they can be displayed inline.")

(defvar mm-inline-override-types nil
  "Types that should be treated as attachments even if they can be displayed inline.")

(defvar mm-inline-override-types nil
  "Types that should be treated as attachments even if they can be displayed inline.")

(defvar mm-automatic-external-display nil
  "List of MIME type regexps that will be displayed externally automatically.")

(defvar mm-discouraged-alternatives nil
  "List of MIME types that are discouraged when viewing multipart/alternative.
Viewing agents are supposed to view the last possible part of a message,
as that is supposed to be the richest.  However, users may prefer other
types instead, and this list says what types are most unwanted.  If,
for instance, text/html parts are very unwanted, and text/richtech are
somewhat unwanted, then the value of this variable should be set
to:

 (\"text/html\" \"text/richtext\")")

(defvar mm-tmp-directory
  (cond ((fboundp 'temp-directory) (temp-directory))
	((boundp 'temporary-file-directory) temporary-file-directory)
	("/tmp/"))
  "Where mm will store its temporary files.")

(defvar mm-inline-large-images nil
  "If non-nil, then all images fit in the buffer.")

;;; Internal variables.

(defvar mm-dissection-list nil)
(defvar mm-last-shell-command "")
(defvar mm-content-id-alist nil)

;;; The functions.

(defun mm-dissect-buffer-header (handle &optional no-strict-mime)
  (save-excursion
    (let (ctl type cte cd description id result header-string header-end)
      (save-restriction
	(mail-narrow-to-head)
	(when (or no-strict-mime
		  (mail-fetch-field "mime-version"))
	  (setq ctl (mail-fetch-field "content-type")
		ctl (ignore-errors (mail-header-parse-content-type ctl))
		cte (mail-fetch-field "content-transfer-encoding")
		cd (mail-fetch-field "content-disposition")
		description (mail-fetch-field "content-description")
		id (mail-fetch-field "content-id")))
	(setq header-end (point-max)
	      header-string (buffer-substring (point-min) header-end)))
      (unless ctl
	(setq ctl (mail-header-parse-content-type "text/plain")))
      (setq cte (and cte (downcase (mail-header-remove-whitespace
				    (mail-header-remove-comments
				     cte))))
	    cd (and cd (ignore-errors
			 (mail-header-parse-content-disposition cd))))
      (if handle
	  (progn
	    (mime-entity-set-content-type-internal handle ctl)
	    (mime-entity-set-encoding-internal handle cte)
	    (mime-entity-set-content-disposition-internal handle cd)
	    (mmgnus-entity-set-content-description-internal handle description)
	    (mmgnus-entity-set-header-internal handle header-string)
	    (setq result handle))
	(setq result (mm-make-handle nil nil ctl cte nil cd
				     description nil id nil header-string)))
      (when id
	(when (string-match " *<\\(.*\\)> *" id)
	  (setq id (match-string 1 id)))
	(mmgnus-entity-set-content-id-internal result id))
      result)))

(defun mm-dissect-buffer (handle &optional no-strict-mime)
  "Dissect the current buffer and return a list of MIME handles."
  (save-excursion
    (let* ((result (mm-dissect-buffer-header handle no-strict-mime))
	   (ctl (mime-entity-content-type-internal result))
	   (type (mime-content-type-primary-type ctl)))
      (cond
       ((and (eq gnus-mime-display-part-function
		 'gnus-mime-display-part-with-mime-view)
	     (eq type 'message))
	(mm-dissect-message result ctl))
       ((eq type 'multipart)
	(mm-dissect-multipart result ctl))
       (t
	(mm-dissect-singlepart result ctl no-strict-mime)))
      (when (mm-handle-id result)
	(push (cons (mm-handle-id result) result) mm-content-id-alist))
      result)))

(defun mm-dissect-singlepart (handle ctl &optional force)
  (mm-handle-set-body handle (mm-copy-to-buffer))
  (push (mm-handle-body handle) mm-dissection-list)
  handle)

(defun mm-remove-all-parts ()
  "Remove all MIME handles."
  (interactive)
  (mapcar 'mm-remove-part mm-dissection-list)
  (setq mm-dissection-list nil))

(defun mm-dissect-message (handle ctl)
  (goto-char (point-min))
  (save-excursion
    (save-restriction
      (when (re-search-forward "\n\n" nil t)
	(narrow-to-region (point) (point-max))
	(let ((part (mm-dissect-buffer nil t)))
	  (mm-handle-set-parent part handle)
	  (mm-handle-set-child handle
			       (cons part (mm-handle-child handle))))))))

(defun mm-dissect-multipart (handle ctl)
  (goto-char (point-min))
  (let* ((node-id (and handle (mime-entity-node-id-internal handle)))
	 (this-node 0)
	 (boundary (concat "\n--" (mail-content-type-get ctl 'boundary)))
	 (close-delimiter (concat (regexp-quote boundary) "--[ \t]*$"))
	start parts
	(end (save-excursion
	       (goto-char (point-max))
	       (if (re-search-backward close-delimiter nil t)
		   (match-beginning 0)
		 (point-max)))))
    (while (search-forward boundary end t)
      (goto-char (match-beginning 0))
      (when start
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (point))
	    (let ((part (mm-make-handle handle nil nil nil nil nil
					nil nil nil nil nil)))
	      (mime-entity-set-node-id-internal part (cons this-node node-id))
	      (setq this-node (1+ this-node))
	      (mm-dissect-buffer part t)
	      (setq parts (cons part parts))))))
      (forward-line 2)
      (setq start (point)))
    (when start
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (let ((part (mm-make-handle handle nil nil nil nil nil
				      nil nil nil nil nil)))
	    (mime-entity-set-node-id-internal part (cons this-node node-id))
	    (mm-dissect-buffer part t)
	    (setq parts (cons part parts))))))
    (mm-handle-set-child handle (nreverse parts))
    handle))

(defun mm-copy-to-buffer ()
  "Copy the contents of the current buffer to a fresh buffer."
  (save-excursion
    (let ((obuf (current-buffer))
	  beg)
      (goto-char (point-min))
      (search-forward-regexp "^\n" nil t)
      (setq beg (point))
      (set-buffer (generate-new-buffer " *mm*"))
      (insert-buffer-substring obuf beg)
      (current-buffer))))

(defun mm-display-part (handle &optional no-default)
  "Display the MIME part represented by HANDLE.
Returns nil if the part is removed; inline if displayed inline;
external if displayed external."
  (save-excursion
    (mm-mailcap-parse-mailcaps)
    (if (mm-handle-displayed-p handle)
	(mm-remove-part handle)
      (let* ((type (mm-handle-media-type handle))
	     (method (mm-mailcap-mime-info type)))
	(if (mm-inlined-p handle)
	    (progn
	      (forward-line 1)
	      (mm-display-inline handle)
	      'inline)
	  (when (or method
		    (not no-default))
	    (if (and (not method)
		     (equal "text" (mm-handle-media-subtype handle)))
		(progn
		  (forward-line 1)
		  (mm-insert-inline handle (mm-get-part handle))
		  'inline)
	      (mm-display-external
	       handle (or method 'mm-mailcap-save-binary-file))
	      'external)))))))

(defun mm-display-external (handle method)
  "Display HANDLE using METHOD."
  (mm-with-unibyte-buffer
    (if (functionp method)
	(let ((cur (current-buffer)))
	  (if (eq method 'mm-mailcap-save-binary-file)
	      (progn
		(set-buffer (generate-new-buffer "*mm*"))
		(setq method nil))
	    (mm-insert-part handle)
	    (let ((win (get-buffer-window cur t)))
	      (when win
		(select-window win)))
	    (switch-to-buffer (generate-new-buffer "*mm*")))
	  (buffer-disable-undo)
	  (mm-set-buffer-file-coding-system mm-binary-coding-system)
	  (insert-buffer-substring cur)
	  (message "Viewing with %s" method)
	  (let ((mm (current-buffer))
		(non-viewer (assq 'non-viewer
				  (mm-mailcap-mime-info
				   (mm-handle-media-type handle) t))))
	    (unwind-protect
		(if method
		    (funcall method)
		  (mm-save-part handle))
	      (when (and (not non-viewer)
			 method)
		(mm-handle-set-undisplayer handle mm)))))
      ;; The function is a string to be executed.
      (mm-insert-part handle)
      (let* ((dir (make-temp-name (expand-file-name "emm." mm-tmp-directory)))
	     (filename (mail-content-type-get
			(mm-handle-disposition handle) 'filename))
	     (mime-info (mm-mailcap-mime-info
			 (mm-handle-media-type handle) t))
	     (needsterm (or (assoc "needsterm" mime-info)
			    (assoc "needsterminal" mime-info)))
	     (copiousoutput (assoc "copiousoutput" mime-info))
	     process file buffer)
	;; We create a private sub-directory where we store our files.
	(make-directory dir)
	(set-file-modes dir 448)
	(if filename
	    (setq file (expand-file-name (file-name-nondirectory filename)
					 dir))
	  (setq file (make-temp-name (expand-file-name "mm." dir))))
	(let ((coding-system-for-write mm-binary-coding-system))
	  (write-region (point-min) (point-max) file nil 'nomesg))
	(message "Viewing with %s" method)
	(unwind-protect
	    (setq process
		  (cond (needsterm
			 (start-process "*display*" nil
					"xterm"
					"-e" shell-file-name 
					shell-command-switch
					(mm-mailcap-command
					 method file (mm-handle-type handle))))
			(copiousoutput
			 (start-process "*display*"
					(setq buffer 
					      (generate-new-buffer "*mm*"))
					shell-file-name
					shell-command-switch
					(mm-mailcap-command
					 method file (mm-handle-type handle)))
			 (switch-to-buffer buffer))
			(t
			 (start-process "*display*"
					(setq buffer
					      (generate-new-buffer "*mm*"))
					shell-file-name
					shell-command-switch
					(mm-mailcap-command
					 method file (mm-handle-type handle))))))
	  (mm-handle-set-undisplayer handle (cons file buffer)))
	(message "Displaying %s..." (format method file))))))

(defun mm-mailcap-command (method file type-list)
  (let ((ctl (cdr type-list))
	(beg 0)
	out sub total)
    (while (string-match "%{\\([^}]+\\)}\\|%s\\|%t" method beg)
      (push (substring method beg (match-beginning 0)) out)
      (setq beg (match-end 0)
	    total (match-string 0 method)
	    sub (match-string 1 method))
      (cond
       ((string= total "%s")
	(push (mm-quote-arg file) out))
       ((string= total "%t")
	(push (mm-quote-arg (car type-list)) out))
       (t
	(push (mm-quote-arg (or (mime-parameter sub ctl) "")) out))))
    (push (substring method beg (length method)) out)
    (mapconcat 'identity (nreverse out) "")))
    
(defun mm-remove-parts (handles)
  "Remove the displayed MIME parts represented by HANDLE."
  (cond
   ((listp handles)
    (let (handle)
      (while (setq handle (pop handles))
	(mm-remove-parts handle))))
   ((mm-handle-child handles)
    (mm-remove-parts (mm-handle-child handles))
    (mm-remove-part handles))
   (t
    (mm-remove-part handles))))

(defun mm-destroy-parts (handles)
  "Remove the displayed MIME parts represented by HANDLE."
  (cond
   ((listp handles)
    (let (handle)
      (while (setq handle (pop handles))
	(mm-destroy-parts handle))))
   ((mm-handle-child handles)
    (mm-destroy-parts (mm-handle-child handles))
    (mm-destroy-part handles)
    (mm-handle-set-child handles nil))
   (t
    (mm-destroy-part handles))))

(defun mm-remove-part (handle)
  "Remove the displayed MIME part represented by HANDLE."
  (when (mm-handle-p handle)
    (let ((object (mm-handle-undisplayer handle)))
      (ignore-errors
	(cond
	 ;; Internally displayed part.
	 ((mm-annotationp object)
	  (delete-annotation object))
	 ((or (functionp object)
	      (and (listp object)
		   (eq (car object) 'lambda)))
	  (funcall object))
	 ;; Externally displayed part.
	 ((consp object)
	  (ignore-errors (delete-file (car object)))
	  (ignore-errors (delete-directory (file-name-directory (car object))))
	  (ignore-errors (kill-buffer (cdr object))))
	 ((bufferp object)
	  (when (buffer-live-p object)
	    (kill-buffer object)))))
      (mm-handle-set-undisplayer handle nil))))

(defun mm-display-inline (handle)
  (let* ((type (mm-handle-media-type handle))
	 (function (cadr (mm-assoc-string-match mm-inline-media-tests type))))
    (funcall function handle)
    (goto-char (point-min))))

(defun mm-assoc-string-match (alist type)
  (dolist (elem alist)
    (when (string-match (car elem) type)
      (return elem))))

(defun mm-inlinable-p (handle)
  "Say whether HANDLE can be displayed inline."
  (let ((alist mm-inline-media-tests)
	(type (mm-handle-media-type handle))
	test)
    (while alist
      (when (string-match (caar alist) type)
	(setq test (caddar alist)
	      alist nil)
	(setq test (funcall test handle)))
      (pop alist))
    test))

(defun mm-automatic-display-p (handle)
  "Say whether the user wants HANDLE to be displayed automatically."
  (let ((methods mm-automatic-display)
	(type (mm-handle-media-type handle))
	method result)
    (while (setq method (pop methods))
      (when (and (not (mm-inline-override-p handle))
		 (string-match method type)
		 (mm-inlinable-p handle))
	(setq result t
	      methods nil)))
    result))

(defun mm-inlined-p (handle)
  "Say whether the user wants HANDLE to be displayed automatically."
  (let ((methods mm-inlined-types)
	(type (mm-handle-media-type handle))
	method result)
    (while (setq method (pop methods))
      (when (and (not (mm-inline-override-p handle))
		 (string-match method type)
		 (mm-inlinable-p handle))
	(setq result t
	      methods nil)))
    result))

(defun mm-attachment-override-p (handle)
  "Say whether HANDLE should have attachment behavior overridden."
  (let ((types mm-attachment-override-types)
	(type (mm-handle-media-type handle))
	ty)
    (catch 'found
      (while (setq ty (pop types))
	(when (and (string-match ty type)
		   (mm-inlinable-p handle))
	  (throw 'found t))))))

(defun mm-inline-override-p (handle)
  "Say whether HANDLE should have inline behavior overridden."
  (let ((types mm-inline-override-types)
	(type (mm-handle-media-type handle))
	ty)
    (catch 'found
      (while (setq ty (pop types))
	(when (string-match ty type)
	  (throw 'found t))))))

(defun mm-automatic-external-display-p (type)
  "Return the user-defined method for TYPE."
  (let ((methods mm-automatic-external-display)
	method result)
    (while (setq method (pop methods))
      (when (string-match method type)
	(setq result t
	      methods nil)))
    result))

(defun mm-destroy-part (handle)
  "Destroy the data structures connected to HANDLE."
  (when (mm-handle-p handle)
    (mm-remove-part handle)
    (when (buffer-live-p (mm-handle-body handle))
      (kill-buffer (mm-handle-body handle))
      (mm-handle-set-body handle nil))))

(defun mm-handle-displayed-p (handle)
  "Say whether HANDLE is displayed or not."
  (mm-handle-undisplayer handle))

;;;
;;; Functions for outputting parts
;;;

(defun mm-get-part (handle)
  "Return the contents of HANDLE as a string."
  (mm-with-unibyte-buffer
    (mm-insert-part handle)
    (buffer-string)))

(defun mm-insert-part (handle)
  "Insert the contents of HANDLE in the current buffer."
  (let ((cur (current-buffer)))
    (save-excursion
      (if (member (mm-handle-media-supertype handle) '("text" "message"))
	  (with-temp-buffer
	    (insert-buffer-substring (mm-handle-body handle))
	    (mm-decode-content-transfer-encoding
	     (mm-handle-encoding handle)
	     (mm-handle-media-type handle))
	    (let ((temp (current-buffer)))
	      (set-buffer cur)
	      (insert-buffer-substring temp)))
	(mm-with-unibyte-buffer
	  (insert-buffer-substring (mm-handle-body handle))
	  (mm-decode-content-transfer-encoding
	   (mm-handle-encoding handle)
	   (mm-handle-media-type handle))
	  (let ((temp (current-buffer)))
	    (set-buffer cur)
	    (insert-buffer-substring temp)))))))

(defvar mm-default-directory nil)

(defun mm-save-part (handle)
  "Write HANDLE to a file."
  (let* ((name (mail-content-type-get (mm-handle-type handle) 'name))
	 (filename (mail-content-type-get
		    (mm-handle-disposition handle) 'filename))
	 file)
    (when filename
      (setq filename (file-name-nondirectory filename)))
    (setq file
	  (read-file-name "Save MIME part to: "
			  (expand-file-name
			   (or filename name "")
			   (or mm-default-directory default-directory))))
    (setq mm-default-directory (file-name-directory file))
    (when (or (not (file-exists-p file))
	      (yes-or-no-p (format "File %s already exists; overwrite? "
				   file)))
      (mm-save-part-to-file handle file))))

(defun mm-save-part-to-file (handle file)
  (mm-with-unibyte-buffer
    (mm-insert-part handle)
    (let ((coding-system-for-write 'binary)
	  ;; Don't re-compress .gz & al.  Arguably we should make
	  ;; `file-name-handler-alist' nil, but that would chop
	  ;; ange-ftp, which is reasonable to use here.
	  (inhibit-file-name-operation 'write-region)
	  (inhibit-file-name-handlers
	   (cons 'jka-compr-handler inhibit-file-name-handlers)))
      (write-region (point-min) (point-max) file))))

(defun mm-pipe-part (handle)
  "Pipe HANDLE to a process."
  (let* ((name (mail-content-type-get (mm-handle-type handle) 'name))
	 (command
	  (read-string "Shell command on MIME part: " mm-last-shell-command)))
    (mm-with-unibyte-buffer
      (mm-insert-part handle)
      (shell-command-on-region (point-min) (point-max) command nil))))

(defun mm-interactively-view-part (handle)
  "Display HANDLE using METHOD."
  (let* ((type (mm-handle-media-type handle))
	 (methods
	  (mapcar (lambda (i) (list (cdr (assoc 'viewer i))))
		  (mm-mailcap-mime-info type 'all)))
	 (method (completing-read "Viewer: " methods)))
    (mm-display-external (copy-sequence handle) method)))

(defun mm-preferred-alternative (handles &optional preferred)
  "Say which of HANDLES are preferred."
  (let ((prec (if preferred (list preferred)
		(mm-preferred-alternative-precedence handles)))
	p h result type handle)
    (while (setq p (pop prec))
      (setq h handles)
      (while h
	(setq handle (car h))
	(setq type (mm-handle-media-type handle))
	(when (and (equal p type)
		   (mm-automatic-display-p handle)
		   (or (mm-handle-child handle)
		       (not (mm-handle-disposition handle))
		       (eq (mm-handle-disposition-type handle) 'inline)))
	  (setq result handle
		h nil
		prec nil))
	(pop h)))
    result))

(defun mm-preferred-alternative-precedence (handles)
  "Return the precedence based on HANDLES and mm-discouraged-alternatives."
  (let ((seq (nreverse (mapcar (lambda (h)
				 (mm-handle-media-type h))
			       handles))))
    (dolist (disc (reverse mm-discouraged-alternatives))
      (dolist (elem (copy-sequence seq))
	(when (string-match disc elem)
	  (setq seq (nconc (delete elem seq) (list elem))))))
    seq))

(defun mm-get-content-id (id)
  "Return the handle(s) referred to by ID."
  (cdr (assoc id mm-content-id-alist)))

(defun mm-get-image (handle)
  "Return an image instance based on HANDLE."
  (let ((type (mm-handle-media-subtype handle))
	spec)
    ;; Allow some common translations.
    (setq type
	  (cond
	   ((equal type "x-pixmap")
	    "xpm")
	   ((equal type "x-xbitmap")
	    "xbm")
	   (t type)))
    (or (mm-handle-cache handle)
	(mm-with-unibyte-buffer
	  (mm-insert-part handle)
	  (prog1
	      (setq spec
		    (ignore-errors
		      (cond
		       ((equal type "xbm")
			;; xbm images require special handling, since
			;; the only way to create glyphs from these
			;; (without a ton of work) is to write them
			;; out to a file, and then create a file
			;; specifier.
			(let ((file (make-temp-name
				     (expand-file-name "emm.xbm"
						       mm-tmp-directory))))
			  (unwind-protect
			      (progn
				(write-region (point-min) (point-max) file)
				(make-glyph (list (cons 'x file))))
			    (ignore-errors
			      (delete-file file)))))
		       (t
			(make-glyph
			 (vector (intern type) :data (buffer-string)))))))
	    (mm-handle-set-cache handle spec))))))

(defun mm-image-fit-p (handle)
  "Say whether the image in HANDLE will fit the current window."
  (let ((image (mm-get-image handle)))
    (or mm-inline-large-images
	(and (< (glyph-width image) (window-pixel-width))
	     (< (glyph-height image) (window-pixel-height))))))

(defun mm-valid-image-format-p (format)
  "Say whether FORMAT can be displayed natively by Emacs."
  (and (fboundp 'valid-image-instantiator-format-p)
       (valid-image-instantiator-format-p format)))

(defun mm-valid-and-fit-image-p (format handle)
  "Say whether FORMAT can be displayed natively and HANDLE fits the window."
  (and window-system
       (mm-valid-image-format-p format)
       (mm-image-fit-p handle)))

(provide 'mm-decode)

;; mm-decode.el ends here

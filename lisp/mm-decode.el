;;; mm-decode.el --- Functions for decoding MIME things
;; Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

;; Jaap-Henk Hoepman (jhh@xs4all.nl): 
;;
;; Added support for delayed destroy of external MIME viewers. All external
;; viewers for mime types in mm-keep-viewer-alive-types will remain active
;; after switching articles or groups, and will only be removed when exiting
;; gnus.
;; 

;;; Code:

(require 'mail-parse)
(require 'gnus-mailcap)
(require 'mm-bodies)
(eval-when-compile (require 'cl))

(eval-and-compile
  (autoload 'mm-inline-partial "mm-partial")
  (autoload 'mm-inline-external-body "mm-extern")
  (autoload 'mm-insert-inline "mm-view"))

(add-hook 'gnus-exit-gnus-hook 'mm-destroy-postponed-undisplay-list)

(defgroup mime-display ()
  "Display of MIME in mail and news articles."
  :link '(custom-manual "(emacs-mime)Customization")
  :version "21.1"
  :group 'mail
  :group 'news
  :group 'multimedia)

(defgroup mime-security ()
  "MIME security in mail and news articles."
  :link '(custom-manual "(emacs-mime)Customization")
  :group 'mail
  :group 'news
  :group 'multimedia)

;;; Convenience macros.

(defmacro mm-handle-buffer (handle)
  `(nth 0 ,handle))
(defmacro mm-handle-type (handle)
  `(nth 1 ,handle))
(defsubst mm-handle-media-type (handle)
  (if (stringp (car handle))
      (car handle)
    (car (mm-handle-type handle))))
(defsubst mm-handle-media-supertype (handle)
  (car (split-string (mm-handle-media-type handle) "/")))
(defsubst mm-handle-media-subtype (handle)
  (cadr (split-string (mm-handle-media-type handle) "/")))
(defmacro mm-handle-encoding (handle)
  `(nth 2 ,handle))
(defmacro mm-handle-undisplayer (handle)
  `(nth 3 ,handle))
(defmacro mm-handle-set-undisplayer (handle function)
  `(setcar (nthcdr 3 ,handle) ,function))
(defmacro mm-handle-disposition (handle)
  `(nth 4 ,handle))
(defmacro mm-handle-description (handle)
  `(nth 5 ,handle))
(defmacro mm-handle-cache (handle)
  `(nth 6 ,handle))
(defmacro mm-handle-set-cache (handle contents)
  `(setcar (nthcdr 6 ,handle) ,contents))
(defmacro mm-handle-id (handle)
  `(nth 7 ,handle))
(defmacro mm-handle-multipart-original-buffer (handle)
  `(get-text-property 0 'buffer (car ,handle)))
(defmacro mm-handle-multipart-from (handle)
  `(get-text-property 0 'from (car ,handle)))
(defmacro mm-handle-multipart-ctl-parameter (handle parameter)
  `(get-text-property 0 ,parameter (car ,handle)))

(defmacro mm-make-handle (&optional buffer type encoding undisplayer
				    disposition description cache
				    id)
  `(list ,buffer ,type ,encoding ,undisplayer
	 ,disposition ,description ,cache ,id))

(defcustom mm-inline-media-tests
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
    ("image/x-portable-bitmap"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'pbm handle)))
    ("text/plain" mm-inline-text identity)
    ("text/enriched" mm-inline-text identity)
    ("text/richtext" mm-inline-text identity)
    ("text/x-patch" mm-display-patch-inline
     (lambda (handle)
       (locate-library "diff-mode")))
    ("application/emacs-lisp" mm-display-elisp-inline identity)
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
    ("message/partial" mm-inline-partial identity)
    ("message/external-body" mm-inline-external-body identity)
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
    ("application/x-pkcs7-signature" ignore identity)
    ("application/pkcs7-signature" ignore identity)
    ("multipart/alternative" ignore identity)
    ("multipart/mixed" ignore identity)
    ("multipart/related" ignore identity)
    ;; Default to displaying as text
    (".*" mm-inline-text identity))
  "Alist of media types/tests saying whether types can be displayed inline."
  :type '(repeat (list (string :tag "MIME type")
		       (function :tag "Display function")
		       (function :tag "Display test")))
  :group 'mime-display)

(defcustom mm-inlined-types
  '("image/.*" "text/.*" "message/delivery-status" "message/rfc822"
    "message/partial" "message/external-body" "application/emacs-lisp"
    "application/pgp-signature" "application/x-pkcs7-signature"
    "application/pkcs7-signature")
  "List of media types that are to be displayed inline.
See also `mm-inline-media-tests', which says how to display a media
type inline."
  :type '(repeat string)
  :group 'mime-display)

(defcustom mm-keep-viewer-alive-types
  '("application/postscript" "application/msword" "application/vnd.ms-excel"
    "application/pdf" "application/x-dvi")
  "List of media types for which the external viewer will not be killed
when selecting a different article."
  :type '(repeat string)
  :group 'mime-display)
 
(defcustom mm-automatic-display
  '("text/plain" "text/enriched" "text/richtext" "text/html"
    "text/x-vcard" "image/.*" "message/delivery-status" "multipart/.*"
    "message/rfc822" "text/x-patch" "application/pgp-signature"
    "application/emacs-lisp" "application/x-pkcs7-signature"
    "application/pkcs7-signature")
  "A list of MIME types to be displayed automatically."
  :type '(repeat string)
  :group 'mime-display)

(defcustom mm-attachment-override-types '("text/x-vcard")
  "Types to have \"attachment\" ignored if they can be displayed inline."
  :type '(repeat string)
  :group 'mime-display)

(defcustom mm-inline-override-types nil
  "Types to be treated as attachments even if they can be displayed inline."
  :type '(repeat string)
  :group 'mime-display)

(defcustom mm-automatic-external-display nil
  "List of MIME type regexps that will be displayed externally automatically."
  :type '(repeat string)
  :group 'mime-display)

(defcustom mm-discouraged-alternatives nil
  "List of MIME types that are discouraged when viewing multipart/alternative.
Viewing agents are supposed to view the last possible part of a message,
as that is supposed to be the richest.  However, users may prefer other
types instead, and this list says what types are most unwanted.  If,
for instance, text/html parts are very unwanted, and text/richtext are
somewhat unwanted, then the value of this variable should be set
to:

 (\"text/html\" \"text/richtext\")"
  :type '(repeat string)
  :group 'mime-display)

(defcustom mm-tmp-directory
  (cond ((fboundp 'temp-directory) (temp-directory))
	((boundp 'temporary-file-directory) temporary-file-directory)
	("/tmp/"))
  "Where mm will store its temporary files."
  :type 'directory
  :group 'mime-display)

(defcustom mm-inline-large-images nil
  "If non-nil, then all images fit in the buffer."
  :type 'boolean
  :group 'mime-display)

(defvar mm-file-name-rewrite-functions nil
  "*List of functions used for rewriting file names of MIME parts.
Each function takes a file name as input and returns a file name.

Ready-made functions include
`mm-file-name-delete-whitespace',
`mm-file-name-trim-whitespace',
`mm-file-name-collapse-whitespace',
`mm-file-name-replace-whitespace',
`capitalize', `downcase', `upcase', and
`upcase-initials'.")

(defvar mm-file-name-replace-whitespace nil
  "String used for replacing whitespace characters; default is `\"_\"'.")

(defcustom mm-default-directory nil
  "The default directory where mm will save files.
If not set, `default-directory' will be used."
  :type 'directory
  :group 'mime-display)

;;; Internal variables.

(defvar mm-dissection-list nil)
(defvar mm-last-shell-command "")
(defvar mm-content-id-alist nil)
(defvar mm-postponed-undisplay-list nil)

;; According to RFC2046, in particular, in a digest, the default
;; Content-Type value for a body part is changed from "text/plain" to
;; "message/rfc822".
(defvar mm-dissect-default-type "text/plain")

(autoload 'mml2015-verify "mml2015")
(autoload 'mml2015-verify-test "mml2015")
(autoload 'mml-smime-verify "mml-smime")
(autoload 'mml-smime-verify-test "mml-smime")

(defvar mm-verify-function-alist
  '(("application/pgp-signature" mml2015-verify "PGP" mml2015-verify-test)
    ("application/x-gnus-pgp-signature" mm-uu-pgp-signed-extract-1 "PGP"
     mm-uu-pgp-signed-test)
    ("application/pkcs7-signature" mml-smime-verify "S/MIME"
     mml-smime-verify-test)
    ("application/x-pkcs7-signature" mml-smime-verify "S/MIME"
     mml-smime-verify-test)))

(defcustom mm-verify-option 'never
  "Option of verifying signed parts.
`never', not verify; `always', always verify;
`known', only verify known protocols. Otherwise, ask user."
  :type '(choice (item always)
		 (item never)
		 (item :tag "only known protocols" known)
		 (item :tag "ask" nil))
  :group 'mime-security)

(autoload 'mml2015-decrypt "mml2015")
(autoload 'mml2015-decrypt-test "mml2015")

(defvar mm-decrypt-function-alist
  '(("application/pgp-encrypted" mml2015-decrypt "PGP" mml2015-decrypt-test)
    ("application/x-gnus-pgp-encrypted" mm-uu-pgp-encrypted-extract-1 "PGP"
     mm-uu-pgp-encrypted-test)))

(defcustom mm-decrypt-option nil
  "Option of decrypting encrypted parts.
`never', not decrypt; `always', always decrypt;
`known', only decrypt known protocols. Otherwise, ask user."
  :type '(choice (item always)
		 (item never)
		 (item :tag "only known protocols" known)
		 (item :tag "ask" nil))
  :group 'mime-security)

(defvar mm-viewer-completion-map
  (let ((map (make-sparse-keymap 'mm-viewer-completion-map)))
    (set-keymap-parent map minibuffer-local-completion-map)
    map)
  "Keymap for input viewer with completion.")

;; Should we bind other key to minibuffer-complete-word?
(define-key mm-viewer-completion-map " " 'self-insert-command)

(defvar mm-viewer-completion-map
  (let ((map (make-sparse-keymap 'mm-viewer-completion-map)))
    (set-keymap-parent map minibuffer-local-completion-map)
    map)
  "Keymap for input viewer with completion.")

;; Should we bind other key to minibuffer-complete-word?
(define-key mm-viewer-completion-map " " 'self-insert-command)

;;; The functions.

(defun mm-alist-to-plist (alist)
  "Convert association list ALIST into the equivalent property-list form.
The plist is returned.  This converts from

\((a . 1) (b . 2) (c . 3))

into

\(a 1 b 2 c 3)

The original alist is not modified.  See also `destructive-alist-to-plist'."
  (let (plist)
    (while alist
      (let ((el (car alist)))
	(setq plist (cons (cdr el) (cons (car el) plist))))
      (setq alist (cdr alist)))
    (nreverse plist)))

(defun mm-keep-viewer-alive-p (handle)
  "Say whether external viewer for HANDLE should stay alive."
  (let ((types mm-keep-viewer-alive-types)
	(type (mm-handle-media-type handle))
	ty)
    (catch 'found
      (while (setq ty (pop types))
	(when (string-match ty type)
	  (throw 'found t))))))

(defun mm-handle-set-external-undisplayer (handle function)
 "Set the undisplayer for this handle; postpone undisplaying of viewers
for types in mm-keep-viewer-alive-types."
 (if (mm-keep-viewer-alive-p handle)
     (let ((new-handle (copy-sequence handle)))
       (mm-handle-set-undisplayer new-handle function)
       (mm-handle-set-undisplayer handle nil)
       (push new-handle mm-postponed-undisplay-list))
   (mm-handle-set-undisplayer handle function)))

(defun mm-destroy-postponed-undisplay-list ()
  (message "Destroying external MIME viewers")
  (mm-destroy-parts mm-postponed-undisplay-list))

(defun mm-dissect-buffer (&optional no-strict-mime)
  "Dissect the current buffer and return a list of MIME handles."
  (save-excursion
    (let (ct ctl type subtype cte cd description id result from)
      (save-restriction
	(mail-narrow-to-head)
	(when (or no-strict-mime
		  (mail-fetch-field "mime-version"))
	  (setq ct (mail-fetch-field "content-type")
		ctl (ignore-errors (mail-header-parse-content-type ct))
		cte (mail-fetch-field "content-transfer-encoding")
		cd (mail-fetch-field "content-disposition")
		description (mail-fetch-field "content-description")
		from (mail-fetch-field "from")
		id (mail-fetch-field "content-id"))
	  ;; FIXME: In some circumstances, this code is running within
	  ;; an unibyte macro.  mail-extract-address-components
	  ;; creates unibyte buffers. This `if', though not a perfect
	  ;; solution, avoids most of them.
	  (if from
	      (setq from (cadr (mail-extract-address-components from))))))
      (when cte
	(setq cte (mail-header-strip cte)))
      (if (or (not ctl)
	      (not (string-match "/" (car ctl))))
	  (mm-dissect-singlepart
	   (list mm-dissect-default-type)
	   (and cte (intern (downcase (mail-header-remove-whitespace
				       (mail-header-remove-comments
					cte)))))
	   no-strict-mime
	   (and cd (ignore-errors (mail-header-parse-content-disposition cd)))
	   description)
	(setq type (split-string (car ctl) "/"))
	(setq subtype (cadr type)
	      type (pop type))
	(setq
	 result
	 (cond
	  ((equal type "multipart")
	   (let ((mm-dissect-default-type (if (equal subtype "digest")
					      "message/rfc822"
					    "text/plain")))
             (add-text-properties 0 (length (car ctl))
                                  (mm-alist-to-plist (cdr ctl)) (car ctl))

	     ;; what really needs to be done here is a way to link a
	     ;; MIME handle back to it's parent MIME handle (in a multilevel
	     ;; MIME article).  That would probably require changing
	     ;; the mm-handle API so we simply store the multipart buffert
	     ;; name as a text property of the "multipart/whatever" string.
             (add-text-properties 0 (length (car ctl))
				  (list 'buffer (mm-copy-to-buffer))
                                  (car ctl))
             (add-text-properties 0 (length (car ctl))
				  (list 'from from)
                                  (car ctl))
	     (cons (car ctl) (mm-dissect-multipart ctl))))
	  (t
	   (mm-dissect-singlepart
	    ctl
	    (and cte (intern (downcase (mail-header-remove-whitespace
					(mail-header-remove-comments
					 cte)))))
	    no-strict-mime
	    (and cd (ignore-errors (mail-header-parse-content-disposition cd)))
	    description id))))
	(when id
	  (when (string-match " *<\\(.*\\)> *" id)
	    (setq id (match-string 1 id)))
	  (push (cons id result) mm-content-id-alist))
	result))))

(defun mm-dissect-singlepart (ctl cte &optional force cdl description id)
  (when (or force
	    (if (equal "text/plain" (car ctl))
		(assoc 'format ctl)
	      t))
    (let ((res (mm-make-handle
		(mm-copy-to-buffer) ctl cte nil cdl description nil id)))
      (push (car res) mm-dissection-list)
      res)))

(defun mm-remove-all-parts ()
  "Remove all MIME handles."
  (interactive)
  (mapcar 'mm-remove-part mm-dissection-list)
  (setq mm-dissection-list nil))

(defun mm-dissect-multipart (ctl)
  (goto-char (point-min))
  (let* ((boundary (concat "\n--" (mail-content-type-get ctl 'boundary)))
	 (close-delimiter (concat (regexp-quote boundary) "--[ \t]*$"))
	 start parts
	 (end (save-excursion
		(goto-char (point-max))
		(if (re-search-backward close-delimiter nil t)
		    (match-beginning 0)
		  (point-max)))))
    (setq boundary (concat (regexp-quote boundary) "[ \t]*$"))
    (while (and (< (point) end) (re-search-forward boundary end t))
      (goto-char (match-beginning 0))
      (when start
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (point))
	    (setq parts (nconc (list (mm-dissect-buffer t)) parts)))))
      (forward-line 2)
      (setq start (point)))
    (when (and start (< start end))
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (setq parts (nconc (list (mm-dissect-buffer t)) parts)))))
    (mm-possibly-verify-or-decrypt (nreverse parts) ctl)))

(defun mm-copy-to-buffer ()
  "Copy the contents of the current buffer to a fresh buffer."
  (save-excursion
    (let ((flag enable-multibyte-characters)
	  (new-buffer (generate-new-buffer " *mm*")))
      (goto-char (point-min))
      (search-forward-regexp "^\n" nil t)
      (save-restriction
	(narrow-to-region (point) (point-max))
	(when flag
	  (set-buffer-multibyte nil))
	(copy-to-buffer new-buffer (point-min) (point-max))
	(when flag
	  (set-buffer-multibyte t)))
      new-buffer)))

(defun mm-display-parts (handle &optional no-default)
  (if (stringp (car handle))
      (mapcar 'mm-display-parts (cdr handle))
    (if (bufferp (car handle))
	(save-restriction
	  (narrow-to-region (point) (point))
	  (mm-display-part handle)
	  (goto-char (point-max)))
      (mapcar 'mm-display-parts handle))))

(defun mm-display-part (handle &optional no-default)
  "Display the MIME part represented by HANDLE.
Returns nil if the part is removed; inline if displayed inline;
external if displayed external."
  (save-excursion
    (mailcap-parse-mailcaps)
    (if (mm-handle-displayed-p handle)
	(mm-remove-part handle)
      (let* ((type (mm-handle-media-type handle))
	     (method (mailcap-mime-info type)))
	(if (and (mm-inlinable-p handle)
		 (mm-inlined-p handle))
	    (progn
	      (forward-line 1)
	      (mm-display-inline handle)
	      'inline)
	  (when (or method
		    (not no-default))
	    (if (and (not method)
		     (equal "text" (car (split-string type))))
		(progn
		  (forward-line 1)
		  (mm-insert-inline handle (mm-get-part handle))
		  'inline)
	      (mm-display-external
	       handle (or method 'mailcap-save-binary-file)))))))))

(defun mm-display-external (handle method)
  "Display HANDLE using METHOD."
  (let ((outbuf (current-buffer)))
    (mm-with-unibyte-buffer
      (if (functionp method)
	  (let ((cur (current-buffer)))
	    (if (eq method 'mailcap-save-binary-file)
		(progn
		  (set-buffer (generate-new-buffer " *mm*"))
		  (setq method nil))
	      (mm-insert-part handle)
	      (let ((win (get-buffer-window cur t)))
		(when win
		  (select-window win)))
	      (switch-to-buffer (generate-new-buffer " *mm*")))
	    (buffer-disable-undo)
	    (mm-set-buffer-file-coding-system mm-binary-coding-system)
	    (insert-buffer-substring cur)
	    (goto-char (point-min))
	    (message "Viewing with %s" method)
	    (let ((mm (current-buffer))
		  (non-viewer (assq 'non-viewer
				    (mailcap-mime-info
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
	       (mime-info (mailcap-mime-info
			   (mm-handle-media-type handle) t))
	       (needsterm (or (assoc "needsterm" mime-info)
			      (assoc "needsterminal" mime-info)))
	       (copiousoutput (assoc "copiousoutput" mime-info))
	       file buffer)
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
	  (cond (needsterm
		 (unwind-protect
		     (start-process "*display*" nil
				    "xterm"
				    "-e" shell-file-name
				    shell-command-switch
				    (mm-mailcap-command
				     method file (mm-handle-type handle)))
		   (mm-handle-set-external-undisplayer handle (cons file buffer)))
		 (message "Displaying %s..." (format method file))
		 'external)
		(copiousoutput
		 (with-current-buffer outbuf
		   (forward-line 1)
		   (mm-insert-inline
		    handle
		    (unwind-protect
			(progn
			  (call-process shell-file-name nil
					(setq buffer
					      (generate-new-buffer " *mm*"))
					nil
					shell-command-switch
					(mm-mailcap-command
					 method file (mm-handle-type handle)))
			  (if (buffer-live-p buffer)
			      (save-excursion
				(set-buffer buffer)
				(buffer-string))))
		      (progn
			(ignore-errors (delete-file file))
			(ignore-errors (delete-directory
					(file-name-directory file)))
			(ignore-errors (kill-buffer buffer))))))
		 'inline)
		(t
		 (unwind-protect
		     (start-process "*display*"
				    (setq buffer
					  (generate-new-buffer " *mm*"))
				    shell-file-name
				    shell-command-switch
				    (mm-mailcap-command
				     method file (mm-handle-type handle)))
		   (mm-handle-set-external-undisplayer handle (cons file buffer)))
		 (message "Displaying %s..." (format method file))
		 'external)))))))

(defun mm-mailcap-command (method file type-list)
  (let ((ctl (cdr type-list))
	(beg 0)
	(uses-stdin t)
	out sub total)
    (while (string-match "%{\\([^}]+\\)}\\|%s\\|%t\\|%%" method beg)
      (push (substring method beg (match-beginning 0)) out)
      (setq beg (match-end 0)
	    total (match-string 0 method)
	    sub (match-string 1 method))
      (cond
       ((string= total "%%")
	(push "%" out))
       ((string= total "%s")
	(setq uses-stdin nil)
	(push (mm-quote-arg file) out))
       ((string= total "%t")
	(push (mm-quote-arg (car type-list)) out))
       (t
	(push (mm-quote-arg (or (cdr (assq (intern sub) ctl)) "")) out))))
    (push (substring method beg (length method)) out)
    (if uses-stdin
	(progn
	  (push "<" out)
	  (push (mm-quote-arg file) out)))
    (mapconcat 'identity (nreverse out) "")))

(defun mm-remove-parts (handles)
  "Remove the displayed MIME parts represented by HANDLES."
  (if (and (listp handles)
	   (bufferp (car handles)))
      (mm-remove-part handles)
    (let (handle)
      (while (setq handle (pop handles))
	(cond
	 ((stringp handle)
	  (when (buffer-live-p (get-text-property 0 'buffer handle))
	    (kill-buffer (get-text-property 0 'buffer handle))))
	 ((and (listp handle)
	       (stringp (car handle)))
	  (mm-remove-parts (cdr handle)))
	 (t
	  (mm-remove-part handle)))))))

(defun mm-destroy-parts (handles)
  "Remove the displayed MIME parts represented by HANDLES."
  (if (and (listp handles)
	   (bufferp (car handles)))
      (mm-destroy-part handles)
    (let (handle)
      (while (setq handle (pop handles))
	(cond
	 ((stringp handle)
	  (when (buffer-live-p (get-text-property 0 'buffer handle))
	    (kill-buffer (get-text-property 0 'buffer handle))))
	 ((and (listp handle)
	       (stringp (car handle)))
	  (mm-destroy-parts handle))
	 (t
	  (mm-destroy-part handle)))))))

(defun mm-remove-part (handle)
  "Remove the displayed MIME part represented by HANDLE."
  (when (listp handle)
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

(defun mm-automatic-display-p (handle)
  "Say whether the user wants HANDLE to be displayed automatically."
  (let ((methods mm-automatic-display)
	(type (mm-handle-media-type handle))
	method result)
    (while (setq method (pop methods))
      (when (and (not (mm-inline-override-p handle))
		 (string-match method type))
	(setq result t
	      methods nil)))
    result))

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

(defun mm-inlined-p (handle)
  "Say whether the user wants HANDLE to be displayed inline."
  (let ((methods mm-inlined-types)
	(type (mm-handle-media-type handle))
	method result)
    (while (setq method (pop methods))
      (when (and (not (mm-inline-override-p handle))
		 (string-match method type))
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
  (when (listp handle)
    (mm-remove-part handle)
    (when (buffer-live-p (mm-handle-buffer handle))
      (kill-buffer (mm-handle-buffer handle)))))

(defun mm-handle-displayed-p (handle)
  "Say whether HANDLE is displayed or not."
  (mm-handle-undisplayer handle))

;;;
;;; Functions for outputting parts
;;;

(defun mm-get-part (handle)
  "Return the contents of HANDLE as a string."
  (mm-with-unibyte-buffer
    (insert (with-current-buffer (mm-handle-buffer handle)
	      (mm-with-unibyte-current-buffer-mule4
		(buffer-string))))
    (mm-decode-content-transfer-encoding
     (mm-handle-encoding handle)
     (mm-handle-media-type handle))
    (buffer-string)))

(defun mm-insert-part (handle)
  "Insert the contents of HANDLE in the current buffer."
  (let ((cur (current-buffer)))
    (save-excursion
      (if (member (mm-handle-media-supertype handle) '("text" "message"))
	  (with-temp-buffer
 	    (insert-buffer-substring (mm-handle-buffer handle))
	    (mm-decode-content-transfer-encoding
	     (mm-handle-encoding handle)
	     (mm-handle-media-type handle))
	    (let ((temp (current-buffer)))
	      (set-buffer cur)
	      (insert-buffer-substring temp)))
	(mm-with-unibyte-buffer
	  (insert-buffer-substring (mm-handle-buffer handle))
	  (mm-decode-content-transfer-encoding
	   (mm-handle-encoding handle)
	   (mm-handle-media-type handle))
	  (let ((temp (current-buffer)))
	    (set-buffer cur)
	    (insert-buffer-substring temp)))))))

(defun mm-file-name-delete-whitespace (file-name)
  "Remove all whitespace characters from FILE-NAME."
  (while (string-match "\\s-+" file-name)
    (setq file-name (replace-match "" t t file-name)))
  file-name)

(defun mm-file-name-trim-whitespace (file-name)
  "Remove leading and trailing whitespace characters from FILE-NAME."
  (when (string-match "\\`\\s-+" file-name)
    (setq file-name (substring file-name (match-end 0))))
  (when (string-match "\\s-+\\'" file-name)
    (setq file-name (substring file-name 0 (match-beginning 0))))
  file-name)

(defun mm-file-name-collapse-whitespace (file-name)
  "Collapse multiple whitespace characters in FILE-NAME."
  (while (string-match "\\s-\\s-+" file-name)
    (setq file-name (replace-match " " t t file-name)))
  file-name)

(defun mm-file-name-replace-whitespace (file-name)
  "Replace whitespace characters in FILE-NAME with underscores.
Set `mm-file-name-replace-whitespace' to any other string if you do not
like underscores."
  (let ((s (or mm-file-name-replace-whitespace "_")))
    (while (string-match "\\s-" file-name)
      (setq file-name (replace-match s t t file-name))))
  file-name)

(defun mm-save-part (handle)
  "Write HANDLE to a file."
  (let* ((name (mail-content-type-get (mm-handle-type handle) 'name))
	 (filename (mail-content-type-get
		    (mm-handle-disposition handle) 'filename))
	 file)
    (when filename
      (setq filename (gnus-map-function mm-file-name-rewrite-functions
					(file-name-nondirectory filename))))
    (setq file
	  (read-file-name "Save MIME part to: "
			  (expand-file-name
			   (or filename name "")
			   (or mm-default-directory default-directory))))
    (setq mm-default-directory (file-name-directory file))
    (and (or (not (file-exists-p file))
	     (yes-or-no-p (format "File %s already exists; overwrite? "
				  file)))
	 (progn
	   (mm-save-part-to-file handle file)
	   file))))

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
      (let ((coding-system-for-write 'binary))
	(shell-command-on-region (point-min) (point-max) command nil)))))

(defun mm-interactively-view-part (handle)
  "Display HANDLE using METHOD."
  (let* ((type (mm-handle-media-type handle))
	 (methods
	  (mapcar (lambda (i) (list (cdr (assoc 'viewer i))))
		  (mailcap-mime-info type 'all)))
	 (method (let ((minibuffer-local-completion-map
			mm-viewer-completion-map))
		   (completing-read "Viewer: " methods))))
    (when (string= method "")
      (error "No method given"))
    (if (string-match "^[^% \t]+$" method)
	(setq method (concat method " %s")))
    (mm-display-external handle method)))

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
		   (or (stringp (car handle))
		       (not (mm-handle-disposition handle))
		       (equal (car (mm-handle-disposition handle))
			      "inline")))
	  (setq result handle
		h nil
		prec nil))
	(pop h)))
    result))

(defun mm-preferred-alternative-precedence (handles)
  "Return the precedence based on HANDLES and `mm-discouraged-alternatives'."
  (let ((seq (nreverse (mapcar #'mm-handle-media-type
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
	   ((equal type "x-portable-bitmap")
	    "pbm")
	   (t type)))
    (or (mm-handle-cache handle)
	(mm-with-unibyte-buffer
	  (mm-insert-part handle)
	  (prog1
	      (setq spec
		    (ignore-errors
		     ;; Avoid testing `make-glyph' since W3 may define
		     ;; a bogus version of it.
		      (if (fboundp 'create-image)
			  (create-image (buffer-string) (intern type) 'data-p)
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
			   (vector (intern type) :data (buffer-string))))))))
	    (mm-handle-set-cache handle spec))))))

(defun mm-image-fit-p (handle)
  "Say whether the image in HANDLE will fit the current window."
  (let ((image (mm-get-image handle)))
    (if (fboundp 'glyph-width)
	;; XEmacs' glyphs can actually tell us about their width, so
	;; lets be nice and smart about them.
	(or mm-inline-large-images
	    (and (< (glyph-width image) (window-pixel-width))
		 (< (glyph-height image) (window-pixel-height))))
      (let* ((size (image-size image))
	     (w (car size))
	     (h (cdr size)))
	(or mm-inline-large-images
	    (and (< h (1- (window-height))) ; Don't include mode line.
		 (< w (window-width))))))))

(defun mm-valid-image-format-p (format)
  "Say whether FORMAT can be displayed natively by Emacs."
  (cond
   ;; Handle XEmacs
   ((fboundp 'valid-image-instantiator-format-p)
    (valid-image-instantiator-format-p format))
   ;; Handle Emacs 21
   ((fboundp 'image-type-available-p)
    (and (display-graphic-p)
	 (image-type-available-p format)))
   ;; Nobody else can do images yet.
   (t
    nil)))

(defun mm-valid-and-fit-image-p (format handle)
  "Say whether FORMAT can be displayed natively and HANDLE fits the window."
  (and (mm-valid-image-format-p format)
       (mm-image-fit-p handle)))

(defun mm-find-part-by-type (handles type &optional notp recursive)
  "Search in HANDLES for part with TYPE.
If NOTP, returns first non-matching part.
If RECURSIVE, search recursively."
  (let (handle)
    (while handles
      (if (and recursive (stringp (caar handles)))
	  (if (setq handle (mm-find-part-by-type (cdar handles) type
						 notp recursive))
	      (setq handles nil))
	(if (if notp
		(not (equal (mm-handle-media-type (car handles)) type))
	      (equal (mm-handle-media-type (car handles)) type))
	    (setq handle (car handles)
		  handles nil)))
      (setq handles (cdr handles)))
    handle))

(defun mm-find-raw-part-by-type (ctl type &optional notp)
  (goto-char (point-min))
  (let* ((boundary (concat "--" (mm-handle-multipart-ctl-parameter ctl
								   'boundary)))
	 (close-delimiter (concat "^" (regexp-quote boundary) "--[ \t]*$"))
	 start
	 (end (save-excursion
		(goto-char (point-max))
		(if (re-search-backward close-delimiter nil t)
		    (match-beginning 0)
		  (point-max))))
	 result)
    (setq boundary (concat "^" (regexp-quote boundary) "[ \t]*$"))
    (while (and (not result)
		(re-search-forward boundary end t))
      (goto-char (match-beginning 0))
      (when start
	(save-excursion
	  (save-restriction
	    (narrow-to-region start (1- (point)))
	    (when (let ((ctl (ignore-errors
			       (mail-header-parse-content-type
				(mail-fetch-field "content-type")))))
		    (if notp
			(not (equal (car ctl) type))
		      (equal (car ctl) type)))
	      (setq result (buffer-substring (point-min) (point-max)))))))
      (forward-line 1)
      (setq start (point)))
    (when (and (not result) start)
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (when (let ((ctl (ignore-errors
			     (mail-header-parse-content-type
			      (mail-fetch-field "content-type")))))
		  (if notp
		      (not (equal (car ctl) type))
		    (equal (car ctl) type)))
	    (setq result (buffer-substring (point-min) (point-max)))))))
    result))

(defvar mm-security-handle nil)

(defsubst mm-set-handle-multipart-parameter (handle parameter value)
  ;; HANDLE could be a CTL.
  (if handle
      (put-text-property 0 (length (car handle)) parameter value
			 (car handle))))

(defun mm-possibly-verify-or-decrypt (parts ctl)
  (let ((subtype (cadr (split-string (car ctl) "/")))
	(mm-security-handle ctl) ;; (car CTL) is the type.
	protocol func functest)
    (cond
     ((equal subtype "signed")
      (unless (and (setq protocol
			 (mm-handle-multipart-ctl-parameter ctl 'protocol))
		   (not (equal protocol "multipart/mixed")))
	;; The message is broken or draft-ietf-openpgp-multsig-01.
	(let ((protocols mm-verify-function-alist))
	  (while protocols
	    (if (and (or (not (setq functest (nth 3 (car protocols))))
			 (funcall functest parts ctl))
		     (mm-find-part-by-type parts (caar protocols) nil t))
		(setq protocol (caar protocols)
		      protocols nil)
	      (setq protocols (cdr protocols))))))
      (setq func (nth 1 (assoc protocol mm-verify-function-alist)))
      (if (cond
	   ((eq mm-verify-option 'never) nil)
	   ((eq mm-verify-option 'always) t)
	   ((eq mm-verify-option 'known)
	    (and func
		 (or (not (setq functest
				(nth 3 (assoc protocol
					      mm-verify-function-alist))))
		     (funcall functest parts ctl))))
	   (t (y-or-n-p
	       (format "Verify signed (%s) part? "
		       (or (nth 2 (assoc protocol mm-verify-function-alist))
			   (format "protocol=%s" protocol))))))
	  (save-excursion
	    (if func
		(funcall func parts ctl)
	      (mm-set-handle-multipart-parameter
	       mm-security-handle 'gnus-details
	       (format "Unknown sign protocol (%s)" protocol))))))
     ((equal subtype "encrypted")
      (unless (setq protocol
		    (mm-handle-multipart-ctl-parameter ctl 'protocol))
	;; The message is broken.
	(let ((parts parts))
	  (while parts
	    (if (assoc (mm-handle-media-type (car parts))
		       mm-decrypt-function-alist)
		(setq protocol (mm-handle-media-type (car parts))
		      parts nil)
	      (setq parts (cdr parts))))))
      (setq func (nth 1 (assoc protocol mm-decrypt-function-alist)))
      (if (cond
	   ((eq mm-decrypt-option 'never) nil)
	   ((eq mm-decrypt-option 'always) t)
	   ((eq mm-decrypt-option 'known)
	    (and func
		 (or (not (setq functest
				(nth 3 (assoc protocol
					      mm-decrypt-function-alist))))
		     (funcall functest parts ctl))))
	   (t (y-or-n-p
	       (format "Decrypt (%s) part? "
		       (or (nth 2 (assoc protocol mm-decrypt-function-alist))
			   (format "protocol=%s" protocol))))))
	  (save-excursion
	    (if func
		(setq parts (funcall func parts ctl))
	      (mm-set-handle-multipart-parameter
	       mm-security-handle 'gnus-details
	       (format "Unknown encrypt protocol (%s)" protocol))))))
     (t nil))
    parts))

(defun mm-multiple-handles (handles)
   (and (listp (car handles)) 
	(> (length handles) 1)))

(defun mm-merge-handles (handles1 handles2) 
  (append
   (if (listp (car handles1)) 
       handles1
     (list handles1))
   (if (listp (car handles2))
       handles2
     (list handles2))))

(provide 'mm-decode)

;;; mm-decode.el ends here

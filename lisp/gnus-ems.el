;;; gnus-ems.el --- functions for making Semi-gnus work under different Emacsen
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;         Tatsuya Ichikawa <t-ichi@niagara.shiojiri.ne.jp>
;; Keywords: news

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

(eval-when-compile
  (require 'cl)
  (require 'ring))

;;; Function aliases later to be redefined for XEmacs usage.

(defvar gnus-mouse-2 [mouse-2])
(defvar gnus-down-mouse-3 [down-mouse-3])
(defvar gnus-down-mouse-2 [down-mouse-2])
(defvar gnus-widget-button-keymap nil)
(defvar gnus-mode-line-modified
  (if (or (featurep 'xemacs)
	  (< emacs-major-version 20))
      '("--**-" . "-----")
    '("**" "--")))

(eval-and-compile
  (autoload 'gnus-xmas-define "gnus-xmas")
  (autoload 'gnus-xmas-redefine "gnus-xmas")
  (autoload 'appt-select-lowest-window "appt"))

(cond ((featurep 'xemacs)
       (autoload 'smiley-region "smiley"))
      ;; override XEmacs version
      ((>= emacs-major-version 21)
       (autoload 'smiley-region "smiley-ems"))
      (t
       (autoload 'smiley-region "smiley-mule")))

(defun gnus-kill-all-overlays ()
  "Delete all overlays in the current buffer."
  (let* ((overlayss (overlay-lists))
	 (buffer-read-only nil)
	 (overlays (delq nil (nconc (car overlayss) (cdr overlayss)))))
    (while overlays
      (delete-overlay (pop overlays)))))

;;; Mule functions.

(eval-and-compile
  (defalias 'gnus-char-width
    (if (fboundp 'char-width)
	'char-width
      (lambda (ch) 1)))) ;; A simple hack.

(eval-and-compile
  (if (featurep 'xemacs)
      (gnus-xmas-define)
    (defvar gnus-mouse-face-prop 'mouse-face
      "Property used for highlighting mouse regions.")))

(eval-and-compile
  (let ((case-fold-search t))
    (cond
     ((string-match "windows-nt\\|os/2\\|emx\\|cygwin32"
		    (symbol-name system-type))
      (setq nnheader-file-name-translation-alist
	    (append nnheader-file-name-translation-alist
		    (mapcar (lambda (c) (cons c ?_))
			    '(?: ?* ?\" ?< ?> ??))
		    (if (string-match "windows-nt\\|cygwin32"
				      (symbol-name system-type))
			nil
		      '((?+ . ?-)))))))))

(defvar gnus-tmp-unread)
(defvar gnus-tmp-replied)
(defvar gnus-tmp-score-char)
(defvar gnus-tmp-indentation)
(defvar gnus-tmp-opening-bracket)
(defvar gnus-tmp-lines)
(defvar gnus-tmp-name)
(defvar gnus-tmp-closing-bracket)
(defvar gnus-tmp-subject-or-nil)
(defvar gnus-check-before-posting)

(defun gnus-ems-redefine ()
  (cond
   ((featurep 'xemacs)
    (gnus-xmas-redefine))

   ((featurep 'mule)
    ;; Mule and new Emacs definitions

    ;; [Note] Now there are three kinds of mule implementations,
    ;; original MULE, XEmacs/mule and Emacs 20+ including
    ;; MULE features.  Unfortunately these API are different.  In
    ;; particular, Emacs (including original MULE) and XEmacs are
    ;; quite different.  However, this version of Gnus doesn't support
    ;; anything other than XEmacs 20+ and Emacs 20.3+.

    ;; Predicates to check are following:
    ;; (boundp 'MULE) is t only if MULE (original; anything older than
    ;;                     Mule 2.3) is running.
    ;; (featurep 'mule) is t when every mule variants are running.

    ;; It is possible to detect XEmacs/mule by (featurep 'mule) and
    ;; checking `emacs-version'.  In this case, the implementation for
    ;; XEmacs/mule may be shareable between XEmacs and XEmacs/mule.

    (defvar gnus-summary-display-table nil
      "Display table used in summary mode buffers.")

    (defalias 'gnus-summary-set-display-table (lambda ()))

    (if (fboundp 'truncate-string-to-width)
	(fset 'gnus-truncate-string 'truncate-string-to-width)
      (fset 'gnus-truncate-string 'truncate-string))

    (when (boundp 'gnus-check-before-posting)
      (setq gnus-check-before-posting
	    (delq 'long-lines
		  (delq 'control-chars gnus-check-before-posting))))
    ))
  (when (featurep 'mule)
    (defun gnus-tilde-max-form (el max-width)
      "Return a form that limits EL to MAX-WIDTH."
      (let ((max (abs max-width)))
	(if (symbolp el)
	    (if (< max-width 0)
		`(let ((width (string-width ,el)))
		   (gnus-truncate-string ,el width (- width ,max)))
	      `(gnus-truncate-string ,el ,max))
	  (if (< max-width 0)
	      `(let* ((val (eval ,el))
		      (width (string-width val)))
		 (gnus-truncate-string val width (- width ,max)))
	    `(let ((val (eval ,el)))
	       (gnus-truncate-string val ,max))))))

    (defun gnus-tilde-cut-form (el cut-width)
      "Return a form that cuts CUT-WIDTH off of EL."
      (let ((cut (abs cut-width)))
	(if (symbolp el)
	    (if (< cut-width 0)
		`(gnus-truncate-string ,el (- (string-width ,el) ,cut))
	      `(gnus-truncate-string ,el (string-width ,el) ,cut))
	  (if (< cut-width 0)
	      `(let ((val (eval ,el)))
		 (gnus-truncate-string val (- (string-width val) ,cut)))
	    `(let ((val (eval ,el)))
	       (gnus-truncate-string val (string-width val) ,cut))))))
    ))

(defun gnus-region-active-p ()
  "Say whether the region is active."
  (and (boundp 'transient-mark-mode)
       transient-mark-mode
       (boundp 'mark-active)
       mark-active))

(if (fboundp 'add-minor-mode)
    (defalias 'gnus-add-minor-mode 'add-minor-mode)
  (defun gnus-add-minor-mode (mode name map &rest rest)
    (set (make-local-variable mode) t)
    (unless (assq mode minor-mode-alist)
      (push `(,mode ,name) minor-mode-alist))
    (unless (assq mode minor-mode-map-alist)
      (push (cons mode map)
	    minor-mode-map-alist))))

(defun gnus-x-splash ()
  "Show a splash screen using a pixmap in the current buffer."
  (let ((dir (nnheader-find-etc-directory "gnus"))
	pixmap file height beg i)
    (save-excursion
      (switch-to-buffer (gnus-get-buffer-create gnus-group-buffer))
      (let ((buffer-read-only nil)
	    width height)
	(erase-buffer)
	(when (and dir
		   (file-exists-p (setq file
					(expand-file-name "x-splash" dir))))
	  (with-temp-buffer
	    (insert-file-contents-as-binary file)
	    (goto-char (point-min))
	    (ignore-errors
	      (setq pixmap (read (current-buffer))))))
	(when pixmap
	  (make-face 'gnus-splash)
	  (setq height (/ (car pixmap) (frame-char-height))
		width (/ (cadr pixmap) (frame-char-width)))
	  (set-face-foreground 'gnus-splash "Brown")
	  (set-face-stipple 'gnus-splash pixmap)
	  (insert-char ?\n (* (/ (window-height) 2 height) height))
	  (setq i height)
	  (while (> i 0)
	    (insert-char ?\  (* (/ (window-width) 2 width) width))
	    (setq beg (point))
	    (insert-char ?\  width)
	    (set-text-properties beg (point) '(face gnus-splash))
	    (insert ?\n)
	    (decf i))
	  (goto-char (point-min))
	  (sit-for 0))))))

;;; Image functions.

(defun gnus-image-type-available-p (type)
  (and (fboundp 'image-type-available-p)
       (image-type-available-p type)))

(defun gnus-create-image (file &optional type data-p &rest props)
  (let ((face (plist-get props :face)))
    (when face
      (setq props (plist-put props :foreground (face-foreground face)))
      (setq props (plist-put props :background (face-background face))))
    (apply 'create-image file type data-p props)))

(defun gnus-put-image (glyph &optional string)
  (insert-image glyph (or string " "))
  (unless string
    (put-text-property (1- (point)) (point)
		       'gnus-image-text-deletable t))
  glyph)

(defun gnus-remove-image (image)
  (dolist (position (message-text-with-property 'display))
    (when (equal (get-text-property position 'display) image)
      (put-text-property position (1+ position) 'display nil)
      (when (get-text-property position 'gnus-image-text-deletable)
	(delete-region position (1+ position))))))

(defun-maybe assoc-ignore-case (key alist)
  "Like `assoc', but assumes KEY is a string and ignores case when comparing."
  (setq key (downcase key))
  (let (element)
    (while (and alist (not element))
      (if (equal key (downcase (car (car alist))))
	  (setq element (car alist)))
      (setq alist (cdr alist)))
    element))


;;; Language support staffs.

(defvar-maybe current-language-environment "English"
  "The language environment.")

(defvar-maybe language-info-alist nil
  "Alist of language environment definitions.")

(defun-maybe get-language-info (lang-env key)
  "Return information listed under KEY for language environment LANG-ENV."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (let ((lang-slot (assoc-ignore-case lang-env language-info-alist)))
    (if lang-slot
	(cdr (assq key (cdr lang-slot))))))

(defun-maybe set-language-info (lang-env key info)
  "Modify part of the definition of language environment LANG-ENV."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (let (lang-slot key-slot)
    (setq lang-slot (assoc lang-env language-info-alist))
    (if (null lang-slot)		; If no slot for the language, add it.
	(setq lang-slot (list lang-env)
	      language-info-alist (cons lang-slot language-info-alist)))
    (setq key-slot (assq key lang-slot))
    (if (null key-slot)			; If no slot for the key, add it.
	(progn
	  (setq key-slot (list key))
	  (setcdr lang-slot (cons key-slot (cdr lang-slot)))))
    (setcdr key-slot info)))

(provide 'gnus-ems)

;;; gnus-ems.el ends here

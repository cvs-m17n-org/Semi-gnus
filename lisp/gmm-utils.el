;;; gmm-utils.el --- Utility functions for Gnus, Message and MML

;; Copyright (C) 2006 Free Software Foundation, Inc.

;; Author: Reiner Steib <reiner.steib@gmx.de>
;; Keywords: news

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library provides self-contained utility functions.  The functions are
;; used in Gnus, Message and MML, but within this library there are no
;; dependencies on Gnus, Message, or MML or Gnus.

;;; Code:

(defgroup gmm nil
  "Utility functions for Gnus, Message and MML"
  :prefix "gmm-"
  :version "23.0" ;; No Gnus
  :group 'lisp)

;; Helper functions from `gnus-utils.el': gmm-verbose, gmm-message, gmm-error

(defcustom gmm-verbose 7
  "Integer that says how verbose gmm should be.
The higher the number, the more messages will flash to say what
it done.  At zero, it will be totally mute; at five, it will
display most important messages; and at ten, it will keep on
jabbering all the time."
  :type 'integer
  :group 'gmm)

;;;###autoload
(defun gmm-message (level &rest args)
  "If LEVEL is lower than `gmm-verbose' print ARGS using `message'.

Guideline for numbers:
1 - error messages, 3 - non-serious error messages, 5 - messages for things
that take a long time, 7 - not very important messages on stuff, 9 - messages
inside loops."
  (if (<= level gmm-verbose)
      (apply 'message args)
    ;; We have to do this format thingy here even if the result isn't
    ;; shown - the return value has to be the same as the return value
    ;; from `message'.
    (apply 'format args)))

;;;###autoload
(defun gmm-error (level &rest args)
  "Beep an error if LEVEL is equal to or less than `gmm-verbose'.
ARGS are passed to `message'."
  (when (<= (floor level) gmm-verbose)
    (apply 'message args)
    (ding)
    (let (duration)
      (when (and (floatp level)
		 (not (zerop (setq duration (* 10 (- level (floor level)))))))
	(sit-for duration))))
  nil)

;;;###autoload
(defun gmm-widget-p (symbol)
  "Non-nil iff SYMBOL is a widget."
  (get symbol 'widget-type))

;; Copy of the `nnmail-lazy' code from `nnmail.el':
(define-widget 'gmm-lazy 'default
  "Base widget for recursive datastructures.

This is copy of the `lazy' widget in Emacs 22.1 provided for compatibility."
  :format "%{%t%}: %v"
  :convert-widget 'widget-value-convert-widget
  :value-create (lambda (widget)
                  (let ((value (widget-get widget :value))
                        (type (widget-get widget :type)))
                    (widget-put widget :children
                                (list (widget-create-child-value
                                       widget (widget-convert type) value)))))
  :value-delete 'widget-children-value-delete
  :value-get (lambda (widget)
               (widget-value (car (widget-get widget :children))))
  :value-inline (lambda (widget)
                  (widget-apply (car (widget-get widget :children))
                                :value-inline))
  :default-get (lambda (widget)
                 (widget-default-get
                  (widget-convert (widget-get widget :type))))
  :match (lambda (widget value)
           (widget-apply (widget-convert (widget-get widget :type))
                         :match value))
  :validate (lambda (widget)
              (widget-apply (car (widget-get widget :children)) :validate)))

;; Note: The format of `gmm-tool-bar-item' may change if some future Emacs
;; version will provide customizable tool bar buttons using a different
;; interface.

(define-widget 'gmm-tool-bar-item (if (gmm-widget-p 'lazy) 'lazy 'gmm-lazy)
  "Tool bar list item."
  :tag "Tool bar item"
  :type '(choice
	  (list :tag "Command and Icon"
		(function :tag "Command")
		(string :tag "Icon file")
		(choice
		 (const :tag "Default map" nil)
		 ;; Note: Usually we need non-nil attributes if map is t.
		 (const :tag "No menu" t)
		 (sexp :tag "Other map"))
		(plist :inline t :tag "Properties"))
	  (list :tag "Separator"
		(const :tag "No command" gmm-ignore)
		(string :tag "Icon file")
		(const :tag "No map")
		(plist :inline t :tag "Properties"))))

(define-widget 'gmm-tool-bar-zap-list (if (gmm-widget-p 'lazy) 'lazy 'gmm-lazy)
  "Tool bar zap list."
  :tag "Tool bar zap list"
  :type '(choice (const :tag "Zap all" t)
		 (const :tag "Keep all" nil)
		 (list
		  ;; :value
		  ;; Work around (bug in customize?), see
		  ;; <news:v9is48jrj1.fsf@marauder.physik.uni-ulm.de>
		  ;; (new-file open-file dired kill-buffer write-file
		  ;; 	    print-buffer customize help)
		  (set :inline t
		       (const new-file)
		       (const open-file)
		       (const dired)
		       (const kill-buffer)
		       (const save-buffer)
		       (const write-file)
		       (const undo)
		       (const cut)
		       (const copy)
		       (const paste)
		       (const search-forward)
		       (const print-buffer)
		       (const customize)
		       (const help))
		  (repeat :inline t
			  :tag "Other"
			  (symbol :tag "Icon item")))))

(defvar tool-bar-map)

;;;###autoload
(defun gmm-tool-bar-from-list (icon-list zap-list default-map)
  "Make a tool bar from ICON-LIST.

Within each entry of ICON-LIST, the first element is a menu
command, the second element is an icon file name and the third
element is a test function.  You can use \\[describe-key]
<menu-entry> to find out the name of a menu command.  The fourth
and all following elements are passed a the PROPS argument to the
function `tool-bar-local-item'.

If ZAP-LIST is a list, remove those item from the default
`tool-bar-map'.  If it is t, start with a new sparse map.  You
can use \\[describe-key] <icon> to find out the name of an icon
item.  When \\[describe-key] <icon> shows \"<tool-bar> <new-file>
runs the command find-file\", then use `new-file' in ZAP-LIST.

DEFAULT-MAP specifies the default key map for ICON-LIST."
  (let (;; For Emacs 21, we must let-bind `tool-bar-map'.  In Emacs 22, we
	;; could use some other local variable.
	(tool-bar-map (if (eq zap-list t)
			  (make-sparse-keymap)
			(copy-keymap tool-bar-map))))
    (when (listp zap-list)
      ;; Zap some items which aren't relevant for this mode and take up space.
      (dolist (key zap-list)
	(define-key tool-bar-map (vector key) nil)))
    (mapc (lambda (el)
	    (let ((command (car el))
		  (icon (nth 1 el))
		  (fmap (or (nth 2 el) default-map))
		  (props  (cdr (cdr (cdr el)))) )
	      ;; command may stem from different from-maps:
	      (cond ((eq command 'gmm-ignore)
		     ;; The dummy `gmm-ignore', see `gmm-tool-bar-item'
		     ;; widget.  Suppress tooltip by adding `:enable nil'.
		     (if (fboundp 'tool-bar-local-item)
			 (apply 'tool-bar-local-item icon nil nil
				tool-bar-map :enable nil props)
		       ;; (tool-bar-local-item ICON DEF KEY MAP &rest PROPS)
		       ;; (tool-bar-add-item ICON DEF KEY &rest PROPS)
		       (apply 'tool-bar-add-item icon nil nil :enable nil props)))
		    ((equal fmap t) ;; Not a menu command
		     (if (fboundp 'tool-bar-local-item)
			 (apply 'tool-bar-local-item
				icon command
				(intern icon) ;; reuse icon or fmap here?
				tool-bar-map props)
		       ;; Emacs 21 compatibility:
		       (apply 'tool-bar-add-item
			      icon command
			      (intern icon)
			      props)))
		    (t ;; A menu command
		     (if (fboundp 'tool-bar-local-item-from-menu)
			 (apply 'tool-bar-local-item-from-menu
				;; (apply 'tool-bar-local-item icon def key
				;; tool-bar-map props)
				command icon tool-bar-map (symbol-value fmap)
				props)
		       ;; Emacs 21 compatibility:
		       (apply 'tool-bar-add-item-from-menu
			      command icon (symbol-value fmap)
			      props))))
	      t))
	  (if (symbolp icon-list)
	      (eval icon-list)
	    icon-list))
    tool-bar-map))

;; WARNING: The following is subject to change.  Don't rely on it yet.

;; From MH-E without modifications:

(defmacro gmm-defun-compat (name function arg-list &rest body)
  "Create function NAME.
If FUNCTION exists, then NAME becomes an alias for FUNCTION.
Otherwise, create function NAME with ARG-LIST and BODY."
  (let ((defined-p (fboundp function)))
    (if defined-p
        `(defalias ',name ',function)
      `(defun ,name ,arg-list ,@body))))

(gmm-defun-compat gmm-image-search-load-path
  image-search-load-path (file &optional path)
  "Emacs 21 and XEmacs don't have `image-search-load-path'.
This function returns nil on those systems."
  nil)

;; From MH-E with modifications:

(defvar gmm-image-load-path nil
  "Directory where images are found.
See the function `gmm-image-load-path'.")

(defun gmm-image-load-path (library image &optional path)
  "Return a suitable search path for images of LIBRARY.

Images for LIBRARY are found in \"../../etc/images\" relative to
the files in \"lisp/LIBRARY\", in `image-load-path', or in
`load-path'.

This function returns value of `load-path' augmented with the
path to IMAGE.  If PATH is given, it is used instead of
`load-path'."
  (unless library (error "No library specified."))
  (unless image   (error "No image specified."))
  (cond (gmm-image-load-path) ;; User setting exists.
	((let (gmm-library-name) ;; Try relative setting
	   ;; First, find library in the load-path.
	   (setq gmm-library-name (locate-library library))
	   (if (not gmm-library-name)
	       (error "Cannot find library `%s' in load-path" library))
	   ;; And then set gmm-image-load-path relative to that.
	   (setq gmm-image-load-path
		 (expand-file-name (concat
				    (file-name-directory gmm-library-name)
				    "../../etc/images")))
	   (file-exists-p (expand-file-name image gmm-image-load-path))))
	((let ((img image)
	       (dir (or
		     ;; Images in image-load-path.
		     (gmm-image-search-load-path image)
		     ;; Images in load-path.
		     (locate-library image)))
	       parent)
	   (and dir
		(setq dir (file-name-directory dir))
		(progn
		  ;; Remove subdirectories.
		  (while (setq parent (file-name-directory img))
		    (setq img (directory-file-name parent)
			  dir (expand-file-name "../" dir)))
		  (setq gmm-image-load-path dir))))))
  ;;
  (unless (file-exists-p gmm-image-load-path)
    (error "Directory `%s' in gmm-image-load-path does not exist"
	     gmm-image-load-path))
  (unless (file-exists-p (expand-file-name image gmm-image-load-path))
    (error "Directory `%s' in gmm-image-load-path does not contain image `%s'."
	   gmm-image-load-path image))
  ;; Return augmented `image-load-path' or `load-path'.
  (cond ((and path (symbolp path))
	 (nconc (list gmm-image-load-path)
		(delete gmm-image-load-path (if (boundp path)
						(symbol-value path)
					      nil))))
	(t
	 (nconc (list gmm-image-load-path)
		(delete gmm-image-load-path load-path)))))

(provide 'gmm-utils)

;;; gmm-utils.el ends here

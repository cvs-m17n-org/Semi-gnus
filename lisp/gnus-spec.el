;;; gnus-spec.el --- format spec functions for Gnus
;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	Katsumi Yamaoka <yamaoka@jpl.org>
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'alist)
(require 'gnus)

(defcustom gnus-use-correct-string-widths t
  "*If non-nil, use correct functions for dealing with wide characters."
  :group 'gnus-format
  :type 'boolean)

;;; Internal variables.

(defvar gnus-summary-mark-positions nil)
(defvar gnus-group-mark-positions nil)
(defvar gnus-group-indentation "")

;; Format specs.  The chunks below are the machine-generated forms
;; that are to be evaled as the result of the default format strings.
;; We write them in here to get them byte-compiled.  That way the
;; default actions will be quite fast, while still retaining the full
;; flexibility of the user-defined format specs.

;; First we have lots of dummy defvars to let the compiler know these
;; are really dynamic variables.

(defvar gnus-tmp-unread)
(defvar gnus-tmp-replied)
(defvar gnus-tmp-score-char)
(defvar gnus-tmp-indentation)
(defvar gnus-tmp-opening-bracket)
(defvar gnus-tmp-lines)
(defvar gnus-tmp-name)
(defvar gnus-tmp-closing-bracket)
(defvar gnus-tmp-subject-or-nil)
(defvar gnus-tmp-subject)
(defvar gnus-tmp-marked)
(defvar gnus-tmp-marked-mark)
(defvar gnus-tmp-subscribed)
(defvar gnus-tmp-process-marked)
(defvar gnus-tmp-number-of-unread)
(defvar gnus-tmp-group-name)
(defvar gnus-tmp-group)
(defvar gnus-tmp-article-number)
(defvar gnus-tmp-unread-and-unselected)
(defvar gnus-tmp-news-method)
(defvar gnus-tmp-news-server)
(defvar gnus-tmp-article-number)
(defvar gnus-mouse-face)
(defvar gnus-mouse-face-prop)

(defun gnus-summary-line-format-spec ()
  (insert gnus-tmp-unread gnus-tmp-replied
	  gnus-tmp-score-char gnus-tmp-indentation)
  (gnus-put-text-property
   (point)
   (progn
     (insert
      gnus-tmp-opening-bracket
      (format "%4d: %-20s"
	      gnus-tmp-lines
	      (if (> (length gnus-tmp-name) 20)
		  (substring gnus-tmp-name 0 20)
		gnus-tmp-name))
      gnus-tmp-closing-bracket)
     (point))
   gnus-mouse-face-prop gnus-mouse-face)
  (insert " " gnus-tmp-subject-or-nil "\n"))

(defvar gnus-summary-line-format-spec
  (gnus-byte-code 'gnus-summary-line-format-spec))

(defun gnus-summary-dummy-line-format-spec ()
  (insert "*  ")
  (gnus-put-text-property
   (point)
   (progn
     (insert ":				 :")
     (point))
   gnus-mouse-face-prop gnus-mouse-face)
  (insert " " gnus-tmp-subject "\n"))

(defvar gnus-summary-dummy-line-format-spec
  (gnus-byte-code 'gnus-summary-dummy-line-format-spec))

(defun gnus-group-line-format-spec ()
  (insert gnus-tmp-marked-mark gnus-tmp-subscribed
	  gnus-tmp-process-marked
	  gnus-group-indentation
	  (format "%5s: " gnus-tmp-number-of-unread))
  (gnus-put-text-property
   (point)
   (progn
     (insert gnus-tmp-group "\n")
     (1- (point)))
   gnus-mouse-face-prop gnus-mouse-face))
(defvar gnus-group-line-format-spec
  (gnus-byte-code 'gnus-group-line-format-spec))

(defvar gnus-format-specs
  `((group ("%M\%S\%p\%P\%5y: %(%g%)%l\n" ,gnus-group-line-format-spec))
    (summary-dummy ("*  %(:                          :%) %S\n"
		    ,gnus-summary-dummy-line-format-spec))
    (summary ("%U\%R\%z\%I\%(%[%4L: %-23,23n%]%) %s\n"
	      ,gnus-summary-line-format-spec)))
  "Alist of format specs.")

(defvar gnus-format-specs-compiled nil
  "Alist of compiled format specs.  Each element should be the form:
\(TYPE (FORMAT-STRING-1 . COMPILED-FUNCTION-1)
		 :
       (FORMAT-STRING-n . COMPILED-FUNCTION-n)).")

(defvar gnus-article-mode-line-format-spec nil)
(defvar gnus-summary-mode-line-format-spec nil)
(defvar gnus-group-mode-line-format-spec nil)

;;; Phew.  All that gruft is over with, fortunately.

;;;###autoload
(defun gnus-update-format (var)
  "Update the format specification near point."
  (interactive
   (list
    (save-excursion
      (eval-defun nil)
      ;; Find the end of the current word.
      (re-search-forward "[ \t\n]" nil t)
      ;; Search backward.
      (when (re-search-backward "\\(gnus-[-a-z]+-line-format\\)" nil t)
	(match-string 1)))))
  (let* ((type (intern (progn (string-match "gnus-\\([-a-z]+\\)-line" var)
			      (match-string 1 var))))
	 (value (symbol-value (intern var)))
	 (spec (set
		(intern (format "%s-spec" var))
		(gnus-parse-format
		 value (symbol-value (intern (format "%s-alist" var)))
		 (not (string-match "mode" var)))))
	 (entry (assq type gnus-format-specs)))
    (if entry
	(let ((elem (assoc value entry)))
	  (if elem
	      (setcdr elem spec)
	    (setcdr entry (cons (cons value elem) (cdr entry)))))
      (push (list type (cons value spec)) gnus-format-specs))
    (gnus-product-variable-touch 'gnus-format-specs)

    (pop-to-buffer "*Gnus Format*")
    (erase-buffer)
    (lisp-interaction-mode)
    (insert (pp-to-string spec))))

(put 'gnus-search-or-regist-spec 'lisp-indent-function 1)
(defmacro gnus-search-or-regist-spec (mspec &rest body)
  (let ((specs (nth 0 mspec)) (type (nth 1 mspec)) (format (nth 2 mspec))
	(spec (nth 3 mspec)) (entry (nth 4 mspec)) (elem (nth 5 mspec)))
    `(let* ((,entry (assq ,type ,specs))
	    (,elem (assoc ,format (cdr ,entry))))
       (or (cdr ,elem)
	   (when (progn ,@body)
	     (if ,entry
		 (if ,elem
		     (setcdr ,elem ,spec)
		   (setcdr ,entry (cons (cons ,format ,spec) (cdr ,entry))))
	       (push (list ,type (cons ,format ,spec)) ,specs))
	     (gnus-product-variable-touch (quote ,specs)))
	   ,spec))))

(defun gnus-update-format-specification-1 (type format val)
  (set (intern (format "gnus-%s-line-format-spec" type))
       (gnus-search-or-regist-spec (gnus-format-specs-compiled
				    type format val entry elem)
	 (when (and gnus-compile-user-specs val)
	   (setq val (prog1
			 (progn
			   (fset 'gnus-tmp-func `(lambda () ,val))
			   (require 'bytecomp)
			   (let (byte-compile-warnings)
			     (byte-compile 'gnus-tmp-func))
			   (gnus-byte-code 'gnus-tmp-func))
		       (when (get-buffer "*Compile-Log*")
			 (bury-buffer "*Compile-Log*"))
		       (when (get-buffer "*Compile-Log-Show*")
			 (bury-buffer "*Compile-Log-Show*"))))))))

(defun gnus-update-format-specifications (&optional force &rest types)
  "Update all (necessary) format specifications."
  ;; Make the indentation array.
  ;; See whether all the stored info needs to be flushed.
  (when force
    (message "%s" "Force update format specs.")
    (setq gnus-format-specs nil
	  gnus-format-specs-compiled nil)
    (gnus-product-variable-touch 'gnus-format-specs
				 'gnus-format-specs-compiled))

  ;; Go through all the formats and see whether they need updating.
  (let (type val)
    (save-excursion
      (while (setq type (pop types))
	;; Jump to the proper buffer to find out the value of the
	;; variable, if possible.  (It may be buffer-local.)
	(let* ((new-format
		(let ((buffer (intern (format "gnus-%s-buffer" type))))
		  (when (and (boundp buffer)
			     (setq val (symbol-value buffer))
			     (gnus-buffer-exists-p val))
		    (set-buffer val))
		  (symbol-value
		   (intern (format "gnus-%s-line-format" type))))))
	  (or (gnus-update-format-specification-1 type new-format nil)
	      ;; This is a new format.
	      (gnus-update-format-specification-1
	       type new-format
	       (gnus-search-or-regist-spec (gnus-format-specs
					    type new-format val entry elem)
		 (setq val (if (stringp new-format)
			       ;; This is a "real" format.
			       (gnus-parse-format
				new-format
				(symbol-value
				 (intern (format "gnus-%s-line-format-alist"
						 type)))
				(not (string-match "mode$"
						   (symbol-name type))))
			     ;; This is a function call or something.
			     new-format))))))))))

(defvar gnus-mouse-face-0 'highlight)
(defvar gnus-mouse-face-1 'highlight)
(defvar gnus-mouse-face-2 'highlight)
(defvar gnus-mouse-face-3 'highlight)
(defvar gnus-mouse-face-4 'highlight)

(defun gnus-mouse-face-function (form type)
  `(gnus-put-text-property
    (point) (progn ,@form (point))
    gnus-mouse-face-prop
    ,(if (equal type 0)
	 'gnus-mouse-face
       `(quote ,(symbol-value (intern (format "gnus-mouse-face-%d" type)))))))

(defvar gnus-face-0 'bold)
(defvar gnus-face-1 'italic)
(defvar gnus-face-2 'bold-italic)
(defvar gnus-face-3 'bold)
(defvar gnus-face-4 'bold)

(defun gnus-face-face-function (form type)
  `(gnus-add-text-properties
    (point) (progn ,@form (point))
    '(gnus-face t face ,(symbol-value (intern (format "gnus-face-%d" type))))))

(defun gnus-balloon-face-function (form type)
  `(gnus-put-text-property
    (point) (progn ,@form (point))
    'balloon-help
    ,(intern (format "gnus-balloon-face-%d" type))))

(defun gnus-spec-tab (column)
  (if (> column 0)
      `(insert (make-string (max (- ,column (current-column)) 0) ? ))
    `(progn
       (if (> (current-column) ,(abs column))
	   (delete-region (point)
			  (- (point) (- (current-column) ,(abs column))))
	 (insert (make-string (max (- ,(abs column) (current-column)) 0)
			      ? ))))))

(defun gnus-correct-length (string)
  "Return the correct width of STRING."
  (let ((length 0))
    (mapcar (lambda (char) (incf length (gnus-char-width char))) string)
    length))

(defun gnus-correct-substring (string start &optional end)
  (let ((wstart 0)
	(wend 0)
	(wseek 0)
	(seek 0)
	(length (length string))
	(string (concat string "\0")))
    ;; Find the start position.
    (while (and (< seek length)
		(< wseek start))
      (incf wseek (gnus-char-width (aref string seek)))
      (incf seek))
    (setq wstart seek)
    ;; Find the end position.
    (while (and (<= seek length)
		(or (not end)
		    (<= wseek end)))
      (incf wseek (gnus-char-width (aref string seek)))
      (incf seek))
    (setq wend seek)
    (substring string wstart (1- wend))))

(defun gnus-tilde-max-form (el max-width)
  "Return a form that limits EL to MAX-WIDTH."
  (let ((max (abs max-width))
	(length-fun (if gnus-use-correct-string-widths
		      'gnus-correct-length
		    'length))
	(substring-fun (if gnus-use-correct-string-widths
		       'gnus-correct-substring
		     'substring)))
    (if (symbolp el)
	`(if (> (,length-fun ,el) ,max)
	     ,(if (< max-width 0)
		  `(,substring-fun ,el (- (,length-fun ,el) ,max))
		`(,substring-fun ,el 0 ,max))
	   ,el)
      `(let ((val (eval ,el)))
	 (if (> (,length-fun val) ,max)
	     ,(if (< max-width 0)
		  `(,substring-fun val (- (,length-fun val) ,max))
		`(,substring-fun val 0 ,max))
	   val)))))

(defun gnus-tilde-cut-form (el cut-width)
  "Return a form that cuts CUT-WIDTH off of EL."
  (let ((cut (abs cut-width))
	(length-fun (if gnus-use-correct-string-widths
		      'gnus-correct-length
		    'length))
	(substring-fun (if gnus-use-correct-string-widths
		       'gnus-correct-substring
		     'substring)))
    (if (symbolp el)
	`(if (> (,length-fun ,el) ,cut)
	     ,(if (< cut-width 0)
		  `(,substring-fun ,el 0 (- (,length-fun ,el) ,cut))
		`(,substring-fun ,el ,cut))
	   ,el)
      `(let ((val (eval ,el)))
	 (if (> (,length-fun val) ,cut)
	     ,(if (< cut-width 0)
		  `(,substring-fun val 0 (- (,length-fun val) ,cut))
		`(,substring-fun val ,cut))
	   val)))))

(defun gnus-tilde-ignore-form (el ignore-value)
  "Return a form that is blank when EL is IGNORE-VALUE."
  (if (symbolp el)
      `(if (equal ,el ,ignore-value)
	   "" ,el)
    `(let ((val (eval ,el)))
       (if (equal val ,ignore-value)
	   "" val))))

(defun gnus-correct-pad-form (el pad-width)
  "Return a form that pads EL to PAD-WIDTH accounting for multi-column
characters correctly. This is because `format' may pad to columns or to
characters when given a pad value."
  (let ((pad (abs pad-width))
	(side (< 0 pad-width)))
    (if (symbolp el)
	`(let ((need (- ,pad (gnus-correct-length ,el))))
	   (if (> need 0)
	       (concat ,(when side '(make-string need ?\ ))
		       ,el
		       ,(when (not side) '(make-string need ?\ )))
	     ,el))
      `(let* ((val (eval ,el))
	      (need (- ,pad (gnus-correct-length ,el))))
	 (if (> need 0)
	     (concat ,(when side '(make-string need ?\ ))
		     ,el
		     ,(when (not side) '(make-string need ?\ )))
	   ,el)))))

(defun gnus-parse-format (format spec-alist &optional insert)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return the
  ;; string.  If the FORMAT string contains the specifiers %( and %)
  ;; the text between them will have the mouse-face text property.
  ;; If the FORMAT string contains the specifiers %[ and %], the text between
  ;; them will have the balloon-help text property.
  (let ((case-fold-search nil))
    (if (string-match
       "\\`\\(.*\\)%[0-9]?[{(�]\\(.*\\)%[0-9]?[�})]\\(.*\n?\\)\\'"
       format)
      (gnus-parse-complex-format format spec-alist)
      ;; This is a simple format.
      (gnus-parse-simple-format format spec-alist insert))))

(defun gnus-parse-complex-format (format spec-alist)
  (let (found-C)
    (save-excursion
      (gnus-set-work-buffer)
      (insert format)
      (goto-char (point-min))
      (while (re-search-forward "\"" nil t)
	(replace-match "\\\"" nil t))
      (goto-char (point-min))
      (insert "(\"")
      ;; Convert all font specs into font spec lists.
      (while (re-search-forward "%\\([0-9]+\\)?\\([��{}()]\\)" nil t)
	(let ((number (if (match-beginning 1)
			  (match-string 1) "0"))
	      (delim (aref (match-string 2) 0)))
	  (if (or (= delim ?\()
		  (= delim ?\{)
		  (= delim ?\�))
	      (replace-match (concat "\"("
				     (cond ((= delim ?\() "mouse")
					   ((= delim ?\{) "face")
					   (t "balloon"))
				     " " number " \"")
			     t t)
	    (replace-match "\")\""))))
      (goto-char (point-max))
      (insert "\")")
      ;; Convert point position commands.
      (goto-char (point-min))
      (let ((case-fold-search nil))
	(while (re-search-forward "%\\([-0-9]+\\)?C" nil t)
	  (replace-match "\"(point)\"" t t)
	  (setq found-C t)))
      ;; Convert TAB commands.
      (goto-char (point-min))
      (while (re-search-forward "%\\([-0-9]+\\)=" nil t)
	(replace-match (format "\"(tab %s)\"" (match-string 1)) t t))
      ;; Convert the buffer into the spec.
      (goto-char (point-min))
      (let ((form (read (current-buffer))))
	(if found-C
	    `(let (gnus-position)
	       ,@(gnus-complex-form-to-spec form spec-alist)
	       (if gnus-position
		   (gnus-put-text-property gnus-position (1+ gnus-position)
					   'gnus-position t)))
	  `(progn
	     ,@(gnus-complex-form-to-spec form spec-alist)))))))

(defun gnus-complex-form-to-spec (form spec-alist)
  (delq nil
	(mapcar
	 (lambda (sform)
	   (cond
	    ((stringp sform)
	     (gnus-parse-simple-format sform spec-alist t))
	    ((eq (car sform) 'point)
	     '(setq gnus-position (point)))
	    ((eq (car sform) 'tab)
	     (gnus-spec-tab (cadr sform)))
	    (t
	     (funcall (intern (format "gnus-%s-face-function" (car sform)))
		      (gnus-complex-form-to-spec (cddr sform) spec-alist)
		      (nth 1 sform)))))
	 form)))

(defun gnus-parse-simple-format (format spec-alist &optional insert)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return a
  ;; string.
  (let (max-width
	spec flist fstring elem result dontinsert user-defined
	type value pad-width spec-beg cut-width ignore-value
	tilde-form tilde elem-type extended-spec)
    (save-excursion
      (gnus-set-work-buffer)
      (insert format)
      (goto-char (point-min))
      (while (search-forward "%" nil t)
	(setq user-defined nil
	      spec-beg nil
	      pad-width nil
	      max-width nil
	      cut-width nil
	      ignore-value nil
	      tilde-form nil
	      extended-spec nil)
	(setq spec-beg (1- (point)))

	;; Parse this spec fully.
	(while
	    (cond
	     ((looking-at "\\([-.0-9]+\\)\\(,[-0-9]+\\)?")
	      (setq pad-width (string-to-number (match-string 1)))
	      (when (match-beginning 2)
		(setq max-width (string-to-number (buffer-substring
						   (1+ (match-beginning 2))
						   (match-end 2)))))
	      (goto-char (match-end 0)))
	     ((looking-at "~")
	      (forward-char 1)
	      (setq tilde (read (current-buffer))
		    type (car tilde)
		    value (cadr tilde))
	      (cond
	       ((memq type '(pad pad-left))
		(setq pad-width value))
	       ((eq type 'pad-right)
		(setq pad-width (- value)))
	       ((memq type '(max-right max))
		(setq max-width value))
	       ((eq type 'max-left)
		(setq max-width (- value)))
	       ((memq type '(cut cut-left))
		(setq cut-width value))
	       ((eq type 'cut-right)
		(setq cut-width (- value)))
	       ((eq type 'ignore)
		(setq ignore-value
		      (if (stringp value) value (format "%s" value))))
	       ((eq type 'form)
		(setq tilde-form value))
	       (t
		(error "Unknown tilde type: %s" tilde)))
	      t)
	     (t
	      nil)))
	(cond
	 ;; User-defined spec -- find the spec name.
	 ((eq (setq spec (char-after)) ?u)
	  (forward-char 1)
	  (when (and (eq (setq user-defined (char-after)) ?&)
		     (looking-at "&\\([^;]+\\);"))
	    (setq user-defined (match-string 1))
	    (goto-char (match-end 1))))
	 ;; extended spec
	 ((and (eq spec ?&) (looking-at "&\\([^;]+\\);"))
	  (setq extended-spec (intern (match-string 1)))
	  (goto-char (match-end 1))))
	(forward-char 1)
	(delete-region spec-beg (point))

	;; Now we have all the relevant data on this spec, so
	;; we start doing stuff.
	(insert "%")
	(if (eq spec ?%)
	    ;; "%%" just results in a "%".
	    (insert "%")
	  (cond
	   ;; Do tilde forms.
	   ((eq spec ?@)
	    (setq elem (list tilde-form ?s)))
	   ;; Treat user defined format specifiers specially.
	   (user-defined
	    (setq elem
		  (list
		   (list (intern (format
				  (if (stringp user-defined)
				      "gnus-user-format-function-%s"
				    "gnus-user-format-function-%c")
				  user-defined))
			 'gnus-tmp-header)
		   ?s)))
	   ;; Find the specification from `spec-alist'.
	   ((setq elem (cdr (assq (or extended-spec spec) spec-alist))))
	   (t
	    (setq elem '("*" ?s))))
	  (setq elem-type (cadr elem))
	  ;; Insert the new format elements.
	  (when (and pad-width
		     (not (and (featurep 'xemacs)
			       gnus-use-correct-string-widths)))
	    (insert (number-to-string pad-width)))
	  ;; Create the form to be evaled.
	  (if (or max-width cut-width ignore-value
		  (and (featurep 'xemacs)
		       gnus-use-correct-string-widths))
	      (progn
		(insert ?s)
		(let ((el (car elem)))
		  (cond ((= (cadr elem) ?c)
			 (setq el (list 'char-to-string el)))
			((= (cadr elem) ?d)
			 (setq el (list 'int-to-string el))))
		  (when ignore-value
		    (setq el (gnus-tilde-ignore-form el ignore-value)))
		  (when cut-width
		    (setq el (gnus-tilde-cut-form el cut-width)))
		  (when max-width
		    (setq el (gnus-tilde-max-form el max-width)))
		  (when pad-width
		    (setq el (gnus-correct-pad-form el pad-width)))
		  (push el flist)))
	    (insert elem-type)
	    (push (car elem) flist))))
      (setq fstring (buffer-substring-no-properties (point-min) (point-max))))

    ;; Do some postprocessing to increase efficiency.
    (setq
     result
     (cond
      ;; Emptiness.
      ((string= fstring "")
       nil)
      ;; Not a format string.
      ((not (string-match "%" fstring))
       (list fstring))
      ;; A format string with just a single string spec.
      ((string= fstring "%s")
       (list (car flist)))
      ;; A single character.
      ((string= fstring "%c")
       (list (car flist)))
      ;; A single number.
      ((string= fstring "%d")
       (setq dontinsert)
       (if insert
	   (list `(princ ,(car flist)))
	 (list `(int-to-string ,(car flist)))))
      ;; Just lots of chars and strings.
      ((string-match "\\`\\(%[cs]\\)+\\'" fstring)
       (nreverse flist))
      ;; A single string spec at the beginning of the spec.
      ((string-match "\\`%[sc][^%]+\\'" fstring)
       (list (car flist) (substring fstring 2)))
      ;; A single string spec in the middle of the spec.
      ((string-match "\\`\\([^%]+\\)%[sc]\\([^%]+\\)\\'" fstring)
       (list (match-string 1 fstring) (car flist) (match-string 2 fstring)))
      ;; A single string spec in the end of the spec.
      ((string-match "\\`\\([^%]+\\)%[sc]\\'" fstring)
       (list (match-string 1 fstring) (car flist)))
      ;; A more complex spec.
      (t
       (list (cons 'format (cons fstring (nreverse flist)))))))

    (if insert
	(when result
	  (if dontinsert
	      result
	    (cons 'insert result)))
      (cond ((stringp result)
	     result)
	    ((consp result)
	     (cons 'concat result))
	    (t "")))))

(defun gnus-eval-format (format &optional alist props)
  "Eval the format variable FORMAT, using ALIST.
If PROPS, insert the result."
  (let ((form (gnus-parse-format format alist props)))
    (if props
	(gnus-add-text-properties (point) (progn (eval form) (point)) props)
      (eval form))))

(defun gnus-compile ()
  "Byte-compile the user-defined format specs."
  (interactive)
  (require 'bytecomp)
  (let ((entries gnus-format-specs)
	(byte-compile-warnings '(unresolved callargs redefine))
	entry type compiled-function)
    (save-excursion
      (gnus-message 7 "Compiling format specs...")

      (while entries
	(setq entry (pop entries)
	      type (car entry))
	(if (memq type '(gnus-version version))
	    (setq gnus-format-specs (delq entry gnus-format-specs))
	  (let ((form (caddr entry)))
	    (when (and (listp form)
		       ;; Under GNU Emacs, it's (byte-code ...)
		       (not (eq 'byte-code (car form)))
		       ;; Under XEmacs, it's (funcall #<compiled-function ...>)
		       (not (and (eq 'funcall (car form))
				 (byte-code-function-p (cadr form)))))
	      (defalias 'gnus-tmp-func `(lambda () ,form))
	      (byte-compile 'gnus-tmp-func)
	      (setq compiled-function (gnus-byte-code 'gnus-tmp-func))
	      (set (intern (format "gnus-%s-line-format-spec" type))
		   compiled-function)
	      (let ((elem (cdr (assq type gnus-format-specs-compiled))))
		(if elem
		    (set-alist 'elem (cadr entry) compiled-function)
		  (setq elem (list (cadr entry) compiled-function)))
		(set-alist 'gnus-format-specs-compiled type elem))))))

      (push (cons 'version emacs-version) gnus-format-specs)
      (gnus-message 7 "Compiling user specs...done"))))

(defun gnus-set-format (type &optional insertable)
  (set (intern (format "gnus-%s-line-format-spec" type))
       (gnus-parse-format
	(symbol-value (intern (format "gnus-%s-line-format" type)))
	(symbol-value (intern (format "gnus-%s-line-format-alist" type)))
	insertable)))

(gnus-ems-redefine)

(provide 'gnus-spec)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; gnus-spec.el ends here

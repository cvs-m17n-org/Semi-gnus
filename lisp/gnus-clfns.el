;;; gnus-clfns.el --- compiler macros for emulating cl functions
;; Copyright (C) 2000, 2001 Free Software Foundation, Inc.

;; Author: Kastsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: cl, compile

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

;; This module is for mainly avoiding cl runtime functions in FSF
;; Emacsen.  Function should also be defined as an ordinary function
;; if it will not be provided in cl.

;;; Code:

(if (featurep 'xemacs)
    nil
  (eval-when-compile (require 'cl))
  (require 'pym)

  (define-compiler-macro butlast (&whole form x &optional n)
    (if (and (fboundp 'butlast)
	     (subrp (symbol-function 'butlast)))
	form
      (if n
	  `(let ((x ,x)
		 (n ,n))
	     (if (and n (<= n 0))
		 x
	       (let ((m (length x)))
		 (or n (setq n 1))
		 (and (< n m)
		      (progn
			(if (> n 0) (setcdr (nthcdr (- (1- m) n) x) nil))
			x)))))
	`(let* ((x ,x)
		(m (length x)))
	   (and (< 1 m)
		(progn
		  (setcdr (nthcdr (- m 2) x) nil)
		  x))))))

  (define-compiler-macro coerce (&whole form x type)
    (if (and (fboundp 'coerce)
	     (subrp (symbol-function 'coerce)))
	form
      `(let ((x ,x)
	     (type ,type))
	 (cond ((eq type 'list) (if (listp x) x (append x nil)))
	       ((eq type 'vector) (if (vectorp x) x (vconcat x)))
	       ((eq type 'string) (if (stringp x) x (concat x)))
	       ((eq type 'array) (if (arrayp x) x (vconcat x)))
	       ((and (eq type 'character) (stringp x) (= (length x) 1))
		(aref x 0))
	       ((and (eq type 'character) (symbolp x)
		     (= (length (symbol-name x)) 1))
		(aref (symbol-name x) 0))
	       ((eq type 'float) (float x))
	       ((typep x type) x)
	       (t (error "Can't coerce %s to type %s" x type))))))

  (define-compiler-macro last (&whole form x &optional n)
    (if (and (fboundp 'last)
	     (subrp (symbol-function 'last)))
	form
      (if n
	  `(let* ((x ,x)
		  (n ,n)
		  (m 0)
		  (p x))
	     (while (consp p)
	       (incf m)
	       (pop p))
	     (if (<= n 0)
		 p
	       (if (< n m)
		   (nthcdr (- m n) x)
		 x)))
	`(let ((x ,x))
	   (while (consp (cdr x))
	     (pop x))
	   x))))

  (define-compiler-macro merge (&whole form type seq1 seq2 pred &rest keys)
    (if (and (fboundp 'merge)
	     (subrp (symbol-function 'merge)))
	form
      `(let ((type ,type)
	     (seq1 ,seq1)
	     (seq2 ,seq2)
	     (pred ,pred))
	 (or (listp seq1) (setq seq1 (append seq1 nil)))
	 (or (listp seq2) (setq seq2 (append seq2 nil)))
	 (let ((res nil))
	   (while (and seq1 seq2)
	     (if (funcall pred (car seq2) (car seq1))
		 (push (pop seq2) res)
	       (push (pop seq1) res)))
	   (coerce (nconc (nreverse res) seq1 seq2) type)))))

  (define-compiler-macro string (&whole form &rest args)
    (if (and (fboundp 'string)
	     (subrp (symbol-function 'string)))
	form
      (list 'concat (cons 'list args))))

  (defun-maybe string (&rest args)
    "Concatenate all the argument characters and make the result a string."
    (concat args))

  (define-compiler-macro subseq (&whole form seq start &optional end)
    (if (and (fboundp 'subseq)
	     (subrp (symbol-function 'subseq)))
	form
      (if end
	  `(let ((seq ,seq)
		 (start ,start)
		 (end ,end))
	     (if (stringp seq)
		 (substring seq start end)
	       (let (len)
		 (if (< end 0)
		     (setq end (+ end (setq len (length seq)))))
		 (if (< start 0)
		     (setq start (+ start (or len (setq len (length seq))))))
		 (cond ((listp seq)
			(if (> start 0)
			    (setq seq (nthcdr start seq)))
			(let ((res nil))
			  (while (>= (setq end (1- end)) start)
			    (push (pop seq) res))
			  (nreverse res)))
		       (t
			(let ((res (make-vector (max (- end start) 0) nil))
			      (i 0))
			  (while (< start end)
			    (aset res i (aref seq start))
			    (setq i (1+ i)
				  start (1+ start)))
			  res))))))
	`(let ((seq ,seq)
	       (start ,start))
	   (if (stringp seq)
	       (substring seq start)
	     (let (len)
	       (if (< start 0)
		   (setq start (+ start (or len (setq len (length seq))))))
	       (cond ((listp seq)
		      (if (> start 0)
			  (setq seq (nthcdr start seq)))
		      (copy-sequence seq))
		     (t
		      (let* ((end (or len (length seq)))
			     (res (make-vector (max (- end start) 0) nil))
			     (i 0))
			(while (< start end)
			  (aset res i (aref seq start))
			  (setq i (1+ i)
				start (1+ start)))
			res)))))))))
  )

;; A tool for the developers.

(defvar cl-run-time-functions
  '(Values
    Values-list acons assoc-if assoc-if-not build-klist butlast ceiling*
    coerce common-lisp-indent-function compiler-macroexpand concatenate
    copy-list count count-if count-if-not delete* delete-duplicates delete-if
    delete-if-not duplicate-symbols-p elt-satisfies-test-p equalp evenp every
    extract-from-klist fill find find-if find-if-not floatp-safe floor* gcd
    gensym gentemp get-setf-method getf hash-table-count hash-table-p
    intersection isqrt keyword-argument-supplied-p keyword-of keywordp last
    lcm ldiff lisp-indent-259 lisp-indent-do lisp-indent-function-lambda-hack
    lisp-indent-report-bad-format lisp-indent-tagbody list-length
    make-hash-table make-random-state map mapc mapcan mapcar* mapcon mapl
    maplist member-if member-if-not merge mismatch mod* nbutlast nintersection
    notany notevery nreconc nset-difference nset-exclusive-or nsublis nsubst
    nsubst-if nsubst-if-not nsubstitute nsubstitute-if nsubstitute-if-not
    nunion oddp pair-with-newsyms pairlis position position-if position-if-not
    proclaim random* random-state-p rassoc* rassoc-if rassoc-if-not
    reassemble-argslists reduce rem* remove remove* remove-duplicates
    remove-if remove-if-not remq replace revappend round* safe-idiv search
    set-difference set-exclusive-or setelt setnth setnthcdr signum some sort*
    stable-sort sublis subseq subsetp subst subst-if subst-if-not substitute
    substitute-if substitute-if-not tailp tree-equal truncate* union
    unzip-lists zip-lists)
  "A list of CL run-time functions.  Some functions were built-in, nowadays.")

;;;###autoload
(defun find-cl-run-time-functions (file-or-directory in-this-emacs)
  "Find CL run-time functions in the FILE-OR-DIRECTORY.  If the prefix
argument IN-THIS-EMACS is non-nil, the built-in functions in this
Emacs will not be reported."
  (interactive (list (read-file-name "Find CL run-time functions in: "
				     nil default-directory t)
		     current-prefix-arg))
  (unless (interactive-p)
    (error "You should invoke `M-x find-cl-run-time-functions' interactively"))
  (let (files clfns working file lines forms fns pt form fn buffer
	      window height buffer-file-format format-alist
	      insert-file-contents-post-hook insert-file-contents-pre-hook)
    (cond ((file-directory-p file-or-directory)
	   (prog1
	       (setq files (directory-files file-or-directory t "\\.el$"))
	     (unless files
	       (message "No files found in: %s" file-or-directory))))
	  ((file-exists-p file-or-directory)
	   (setq files (list file-or-directory)))
	  (t
	   (message "No such file or directory: %s" file-or-directory)))
    (dolist (file files)
      (unless (file-exists-p file)
	(setq files (delete file files))))
    (if files
	(progn
	  (if in-this-emacs
	      (dolist (fn cl-run-time-functions)
		(unless (and (fboundp fn)
			     (subrp (symbol-function fn)))
		  (push fn clfns)))
	    (setq clfns cl-run-time-functions))
	  (set-buffer (setq working
			    (get-buffer-create
			     " *Searching for CL run-time functions*")))
	  (let (emacs-lisp-mode-hook)
	    (emacs-lisp-mode))
	  (while files
	    (setq file (pop files)
		  lines (list nil 1))
	    (message "Searching for CL run-time functions in: %s..."
		     (file-name-nondirectory file))
	    (insert-file-contents file nil nil nil t)
	    ;; Why is the following needed for FSF Emacsen???
	    (goto-char (point-min))
	    ;;
	    (while (setq forms (condition-case nil
				   (list (read working))
				 (error nil)))
	      (setq fns nil
		    pt (point)
		    lines (list (cadr lines)
				(count-lines (point-min) pt)))
	      (condition-case nil
		  (progn
		    (forward-list -1)
		    (setcar lines (+ (count-lines (point-min) (point))
				     (if (bolp) 1 0))))
		(error))
	      (goto-char pt)
	      (while forms
		(setq form (pop forms))
		(if (consp form)
		    (progn
		      (setq fn (pop form))
		      (cond ((eq fn 'define-compiler-macro)
			     (setq form nil))
			    ((memq fn '(let let*))
			     (setq form
				   (append
				    (delq nil
					  (mapcar
					   (lambda (element)
					     (when (and (consp element)
							(consp (cadr element)))
					       (cadr element)))
					   (car form)))
				    (cdr form))))
			    ((memq fn '(defadvice
					 defmacro defsubst defun
					 defmacro-maybe defmacro-maybe-cond
					 defsubst-maybe defun-maybe
					 defun-maybe-cond))
			     (setq form (cddr form)))
			    ((eq fn 'lambda)
			     (setq form (cdr form)))
			    ((memq fn '(\` backquote quote))
			     (setq form (when (consp (car form))
					  (car form))))
			    ((eq fn 'dolist)
			     (setcar form (cadar form)))
			    ((and (memq fn clfns)
				  (listp form))
			     (push fn fns)))
		      (when (and (consp form)
				 (condition-case nil
				     ;; Ignore a case `(a b . c)'.
				     (length form)
				   (error nil)))
			(setq forms (append (delq nil
						  (mapcar
						   (lambda (element)
						     (when (consp element)
						       element))
						   form))
					    forms))))
		  (goto-char (point-max))
		  (setq lines (list (cadr lines)
				    (count-lines (point-min) (point)))
			fns '("Couldn't parse, check this file manually."))))
	      (when fns
		(if buffer
		    (set-buffer buffer)
		  (display-buffer
		   (setq buffer (get-buffer-create
				 (concat "*CL run-time functions in: "
					 file-or-directory "*"))))
		  (setq window (get-buffer-window buffer t)
			height (window-height window))
		  (set-buffer buffer)
		  (erase-buffer))
		(when file
		  (insert file "\n")
		  (setq file nil))
		(insert (format "%5d - %5d: %s"
				(car lines) (cadr lines)
				(mapconcat (lambda (fn) (format "%s" fn))
					   (nreverse fns) " ")))
		(while (> (current-column) 78)
		  (skip-chars-backward "^ ")
		  (backward-char 1)
		  (insert "\n              ")
		  (end-of-line))
		(insert "\n")
		(when (zerop (forward-line (- 0 height -2)))
		  (set-window-start window (point)))
		(goto-char (point-max))
		(sit-for 0)
		(set-buffer working))))
	  (kill-buffer working)
	  (if buffer
	      (message "Done")
	    (message "No CL run-time functions found in: %s"
		     file-or-directory)))
      (message "No files found"))))

(provide 'gnus-clfns)

;;; gnus-clfns.el ends here

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

;; Avoid cl runtime functions for FSF Emacsen.

;;; Code:

(if (featurep 'xemacs)
    nil
  (require 'cl)

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

(provide 'gnus-clfns)

;;; gnus-clfns.el ends here

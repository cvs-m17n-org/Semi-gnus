;;; gnus-clfns.el --- compiler macros for emulating cl functions
;; Copyright (C) 2000 Free Software Foundation, Inc.

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

  (define-compiler-macro mapc (&whole form fn seq &rest rest)
    (if (and (fboundp 'mapc)
	     (subrp (symbol-function 'mapc)))
	form
      (if rest
	  `(let* ((fn ,fn)
		  (seq ,seq)
		  (args (list seq ,@rest))
		  (m (apply (function min) (mapcar (function length) args)))
		  (n 0))
	     (while (< n m)
	       (apply fn (mapcar (function (lambda (arg) (nth n arg))) args))
	       (setq n (1+ n)))
	     seq)
	`(let ((seq ,seq))
	   (mapcar ,fn seq)
	   seq))))

  (define-compiler-macro mapcon (&whole form fn seq &rest rest)
    (if (and (fboundp 'mapcon)
	     (subrp (symbol-function 'mapcon)))
	form
      (if rest
	  `(let ((fn ,fn)
		 res
		 (args (list ,seq ,@rest))
		 p)
	     (while (not (memq nil args))
	       (push (apply ,fn args) res)
	       (setq p args)
	       (while p
		 (setcar p (cdr (pop p)))
		 ))
	     (apply (function nconc) (nreverse res)))
	`(let ((fn ,fn)
	       res
	       (arg ,seq))
	   (while arg
	     (push (funcall ,fn arg) res)
	     (setq arg (cdr arg)))
	   (apply (function nconc) (nreverse res))))))

;;  (define-compiler-macro member-if (&whole form pred list)
;;    (if (and (fboundp 'member-if)
;;	     (subrp (symbol-function 'member-if)))
;;	form
;;      `(let ((fn ,pred)
;;	     (seq ,list))
;;	 (while (and seq
;;		     (not (funcall fn (car seq))))
;;	   (pop seq))
;;	 seq)))

  (define-compiler-macro union (&whole form list1 list2)
    (if (and (fboundp 'union)
	     (subrp (symbol-function 'union)))
	form
      `(let ((a ,list1)
	     (b ,list2))
	 (cond ((null a) b)
	       ((null b) a)
	       ((equal a b) a)
	       (t
		(or (>= (length a) (length b))
		    (setq a (prog1 b (setq b a))))
		(while b
		  (or (memq (car b) a)
		      (push (car b) a))
		  (pop b))
		a)))))
  )

(provide 'gnus-clfns)

;;; gnus-clfns.el ends here

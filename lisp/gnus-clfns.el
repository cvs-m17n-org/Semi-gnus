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
  )

(provide 'gnus-clfns)

;;; gnus-clfns.el ends here

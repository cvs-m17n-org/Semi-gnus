;;; gnus-i18n.el --- Internationalization for Gnus

;; Copyright (C) 1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1997/11/27
;; Keywords: internationalization, news, mail

;; This file is not part of GNU Emacs yet.

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

;;; @ newsgroup default charset
;;;

(defvar gnus-newsgroup-default-charset-alist
  '(("\\(^\\|:\\)\\(fj\\|tnn\\|japan\\)\\."	. iso-2022-jp-2)
    ("\\(^\\|:\\)han\\."			. euc-kr)
    ("\\(^\\|:\\)relcom\\."			. koi8-r)
    ("\\(^\\|:\\)alt\\.chinese\\.text\\.big5"	. cn-big5)
    ("\\(^\\|:\\)hk\\(star\\)?\\."		. cn-big5)
    ("\\(^\\|:\\)tw\\."				. cn-big5)
    ("\\(^\\|:\\)alt\\.chinese"			. hz-gb-2312)
    )
  "Alist of newsgroup patterns vs. corresponding default MIME charset.
Each element looks like (REGEXP . SYMBOL).  REGEXP is pattern for
newsgroup name.  SYMBOL is MIME charset or coding-system.")

(defun gnus-set-newsgroup-default-charset (newsgroup charset)
  "Set CHARSET for the NEWSGROUP as default MIME charset."
  (let* ((ng-regexp (concat "^" (regexp-quote newsgroup) "\\($\\|\\.\\)"))
	 (pair (assoc ng-regexp gnus-newsgroup-default-charset-alist))
	 )
    (if pair
	(setcdr pair charset)
      (setq gnus-newsgroup-default-charset-alist
	    (cons (cons ng-regexp charset)
		  gnus-newsgroup-default-charset-alist))
      )))


;;; @ localization
;;;

(defun gnus-set-summary-default-charset ()
  "Set up `default-mime-charset' of summary buffer.
It is specified by variable `gnus-newsgroup-default-charset-alist'
\(cf. function `gnus-set-newsgroup-default-charset')."
  (if (buffer-live-p gnus-summary-buffer)
      (let ((charset
	     (catch 'found
	       (let ((group
		      (save-excursion
			(set-buffer gnus-summary-buffer)
			gnus-newsgroup-name))
		     (alist gnus-newsgroup-default-charset-alist))
		 (while alist
		   (let ((pair (car alist)))
		     (if (string-match (car pair) group)
			 (throw 'found (cdr pair))
		       ))
		   (setq alist (cdr alist)))
		 ))))
	(if charset
            (progn
              (save-excursion
                (set-buffer gnus-summary-buffer)
                (make-local-variable 'default-mime-charset)
                (setq default-mime-charset charset))
              (make-local-variable 'default-mime-charset)
              (setq default-mime-charset charset))
	  (kill-local-variable 'default-mime-charset)))))


;;; @ end
;;;

(provide 'gnus-i18n)

;;; gnus-i18n.el ends here

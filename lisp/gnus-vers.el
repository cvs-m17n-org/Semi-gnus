;;; gnus-vers.el --- Declare gnus version.

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Keiichi Suzuki <keiichi@nanap.org>
;;         Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: news, mail, compatibility

;; This file is part of T-gnus.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'poe)
(require 'product)
(provide 'gnus-vers)

(defconst gnus-revision-number "06"
  "Revision number for this version of gnus.")

;; Product information of this gnus.
(product-provide 'gnus-vers
  (product-define "T-gnus" nil
		  (list 6 15 0
			(string-to-number gnus-revision-number))))

(defconst gnus-original-version-number "0.01"
  "Version number for this version of Gnus.")

(provide 'running-pterodactyl-gnus-0_73-or-later)

(defconst gnus-original-product-name "Oort Gnus"
  "Product name of the original version of Gnus.")

(defconst gnus-product-name (product-name (product-find 'gnus-vers))
  "Product name of this version of gnus.")

(defconst gnus-version-number
  (mapconcat
   'number-to-string
   (butlast (product-version (product-find 'gnus-vers)))
   ".")
  "Version number for this version of gnus.")

(defconst gnus-version
  (format "%s %s r%s (based on %s v%s ; for SEMI 1.13, FLIM 1.13)"
	  gnus-product-name gnus-version-number gnus-revision-number
	  gnus-original-product-name gnus-original-version-number)
  "Version string for this version of gnus.")

(defun gnus-version (&optional arg)
  "Version number of this version of Gnus.
If ARG, insert string at point."
  (interactive "P")
  (if arg
      (insert (message "%s" gnus-version))
    (message "%s" gnus-version)))

(defun gnus-extended-version ()
  "Stringified gnus version."
  (concat gnus-product-name "/" gnus-version-number
	  " (based on "
	  gnus-original-product-name " v" gnus-original-version-number ")"
	  (if (zerop (string-to-number gnus-revision-number))
	      ""
	    (concat " (revision " gnus-revision-number ")"))
	  ))

;; gnus-vers.el ends here

;;; gnus-vers.el --- Declare gnus version.

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Keiichi Suzuki <keiichi@nanap.org>
;; Keywords: news, mail, compatibility

;; This file is part of Nana-gnus.

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

(require 'product)
(provide 'gnus-vers)

;; Product information of this gnus.
(product-provide 'gnus-vers
    (product-define "Nana-gnus" nil '(7 1 0 18)))

(defconst gnus-product-name (product-name (product-find 'gnus-vers))
  "Product name of this version of gnus.")

(defconst gnus-version-number (mapconcat
			       'number-to-string
			       (product-version (product-find 'gnus-vers))
			       ".")
  "Version number for this version of gnus.")

;; gnus-vers.el ends here

;;; gnus-vers.el --- Declare gnus version

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006
;; Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'poe)
(require 'product)
(provide 'gnus-vers)

(defconst gnus-revision-number "00"
  "Revision number for this version of gnus.")

;; Product information of this gnus.
(product-provide 'gnus-vers
  (product-define "T-gnus" nil
		  (list 6 17 4
			(string-to-number gnus-revision-number))))

(defconst gnus-original-version-number "0.4"
  "Version number for this version of Gnus.")

(provide 'running-pterodactyl-gnus-0_73-or-later)

(defconst gnus-original-product-name "No Gnus"
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
  (format "%s %s r%s (based on %s v%s ; for SEMI 1.14 FLIM 1.14)"
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

(eval-when-compile
  (defvar mime-user-interface-product)
  (require 'mime-def))

(defun gnus-extended-version ()
  "Stringified Gnus, Emacs, SEMI, FLIM and APEL versions.
See the variable `gnus-user-agent'."
  (if (stringp gnus-user-agent)
      gnus-user-agent
    ;; `gnus-user-agent' is a list:
    (let* ((float-output-format nil)
	   (gnus-v (when (memq 'gnus gnus-user-agent)
		     (concat
		      gnus-product-name "/" gnus-version-number " ("
		      (unless (zerop (string-to-number gnus-revision-number))
			(concat "r" gnus-revision-number ", "))
		      "based on " gnus-original-product-name
		      " v" gnus-original-version-number ")")))
	   (emacs-v (gnus-emacs-version))
	   (mime-v (when (memq 'mime gnus-user-agent)
		     (concat
		      (mime-product-name mime-user-interface-product) "/"
		      (mapconcat
		       #'number-to-string
		       (mime-product-version mime-user-interface-product)
		       ".")
		      " ("
		      (mime-product-code-name mime-user-interface-product)
		      ") "
		      (mime-product-name mime-library-product) "/"
		      (mapconcat
		       #'number-to-string
		       (mime-product-version mime-library-product)
		       ".")
		      " ("
		      (mime-product-code-name mime-library-product)
		      ") " (apel-version)))))
      (mapconcat 'identity (delq nil (list gnus-v mime-v emacs-v)) " "))))

;; gnus-vers.el ends here

;;; base64.el --- Base64 encoding functions using MEL
;; Copyright (C) 2000, 2001 Free Software Foundation, Inc.

;; Author: T-gnus development team
;; Keywords: extensions

;; This file is part of T-gnus.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-and-compile
  (defun base64-autoload-functionp (object)
    (if (functionp object)
	(let ((def object))
	  (while (and (symbolp def) (fboundp def))
	    (setq def (symbol-function def)))
	  (eq (car-safe def) 'autoload))))

  (if (base64-autoload-functionp 'base64-decode-string)
      (fmakunbound 'base64-decode-string))
  (if (base64-autoload-functionp 'base64-decode-region)
      (fmakunbound 'base64-decode-region))
  (if (base64-autoload-functionp 'base64-encode-string)
      (fmakunbound 'base64-encode-string))
  (if (base64-autoload-functionp 'base64-encode-region)
      (fmakunbound 'base64-encode-region))

  (require 'mel)

  (mel-find-function 'mime-decode-string "base64")
  (mel-find-function 'mime-decode-region "base64")
  (mel-find-function 'mime-encode-string "base64")
  (mel-find-function 'mime-encode-region "base64"))

(provide 'base64)

;;; base64.el ends here

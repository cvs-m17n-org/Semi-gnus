;;; mail-parse.el --- Interface functions for parsing mail
;; Copyright (C) 1998,99 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;; This file contains wrapper functions for a wide range of mail
;; parsing functions.  The idea is that there are low-level libraries
;; that impement according to various specs (RFC2231, DRUMS, USEFOR),
;; but that programmers that want to parse some header (say,
;; Content-Type) will want to use the latest spec.
;;
;; So while each low-level library (rfc2231.el, for instance) decodes
;; faithfully according to that (proposed) standard, this library is
;; the interface library.  If some later RFC supersedes RFC2231, one
;; would just have to write a new low-level library, adjust the
;; aliases in this library, and the users and programmers won't notice
;; any changes.

;;; Code:

(require 'ietf-drums)
(require 'rfc2231)
(require 'rfc2047)
(require 'rfc2045)

(defalias 'mail-header-parse-content-type 'rfc2231-parse-string)
(defalias 'mail-header-parse-content-disposition 'rfc2231-parse-string)
(defalias 'mail-content-type-get 'rfc2231-get-value)
(defalias 'mail-header-encode-parameter 'rfc2045-encode-string)

(defalias 'mail-header-remove-comments 'ietf-drums-remove-comments)
(defalias 'mail-header-remove-whitespace 'ietf-drums-remove-whitespace)
(defalias 'mail-header-get-comment 'ietf-drums-get-comment)
(defalias 'mail-header-parse-address 'ietf-drums-parse-address)
(defalias 'mail-header-parse-addresses 'ietf-drums-parse-addresses)
(defalias 'mail-header-parse-date 'ietf-drums-parse-date)
(defalias 'mail-narrow-to-head 'ietf-drums-narrow-to-header)
(defalias 'mail-quote-string 'ietf-drums-quote-string)

(defalias 'mail-header-narrow-to-field 'rfc2047-narrow-to-field)
(defalias 'mail-encode-encoded-word-region 'rfc2047-encode-region)
(defalias 'mail-encode-encoded-word-buffer 'rfc2047-encode-message-header)
(defalias 'mail-encode-encoded-word-string 'rfc2047-encode-string)
(defalias 'mail-decode-encoded-word-region 'rfc2047-decode-region)
(defalias 'mail-decode-encoded-word-string 'rfc2047-decode-string)

(provide 'mail-parse)

;;; mail-parse.el ends here

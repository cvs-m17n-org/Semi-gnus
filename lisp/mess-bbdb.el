;; mess-bbdb.el --- Interface to message (For after Nana-gnus 6.12.1).

;; Copyright (C) 1998 Keiichi Suzuki <kei-suzu@mail.wbs.ne.jp>

;; Author: Keiichi Suzuki <kei-suzu@mail.wbs.ne.jp>
;; Keywords: BBDB, mail, news

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'bbdb)

(defvar message-bbdb/mailing-list-field 'ml-name)

(defun message-bbdb/mailing-list-p (address)
  (let ((record (bbdb-search-simple nil address)))
    (and record
	 (bbdb-record-getprop record message-bbdb/mailing-list-field)
	 )))

(provide 'mess-bbdb)
			  
;; mess-bbdb.el ends here.

;;; mmgnus.el --- MIME entity implementation for gnus-article

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;         Keiichi Suzuki <keiichi@nanp.org>
;; Keywords: MIME, multimedia, mail, news

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

(require 'mmbuffer)

(luna-define-class mime-gnus-handle-entity (mime-buffer-entity)
		   (undisplayer content-description cache content-id))

(luna-define-internal-accessors 'mime-gnus-handle-entity)

(luna-define-method initialize-instance ((entity mime-gnus-handle-entity)
					 &rest init-args)
  (apply (car (luna-class-find-functions
	       (luna-find-class 'standard-object)
	       'initialize-instance))
	 entity init-args))

(luna-define-class mime-gnus-entity (mime-gnus-handle-entity)
		   (number
		    subject from date id references chars lines xref extra))

(luna-define-internal-accessors 'mime-gnus-entity)

(luna-define-method initialize-instance ((entity mime-gnus-entity)
					 &rest init-args)
  (apply (car (luna-class-find-functions
	       (luna-find-class 'standard-object)
	       'initialize-instance))
	 entity init-args))

;;; @ end
;;;

(provide 'mmgnus)

;;; mmgnus.el ends here

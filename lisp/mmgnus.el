;;; mmgnus.el --- MIME entity implementation for gnus-article

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: MIME, multimedia, mail, news

;; This file is part of Chao-gnus.

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

(mm-define-backend gnus (generic))

(mm-define-method entity-buffer ((entity gnus))
  ;; (if (with-current-buffer gnus-summary-buffer
  ;;       (eq gnus-current-article (mail-header-number entity)))
  ;;     ...)
  (unless (mime-entity-header-start-internal entity)
    (set-buffer gnus-original-article-buffer)
    (mime-entity-set-header-start-internal entity (point-min))
    (mime-entity-set-body-end-internal entity (point-max))
    (if (re-search-forward "^$" nil t)
	(progn
	  (mime-entity-set-header-end-internal entity (match-end 0))
	  (mime-entity-set-body-start-internal
	   entity
	   (if (= (mime-entity-header-end-internal entity)
		  (mime-entity-body-end-internal entity))
	       (mime-entity-body-end-internal entity)
	     (1+ (mime-entity-header-end-internal entity))
	     ))
	  )
      (mime-entity-set-header-end-internal entity (point-min))
      (mime-entity-set-body-start-internal entity (point-min))
      ))
  gnus-original-article-buffer)


;;; @ end
;;;

(provide 'mmgnus)

;;; mmgnus.el ends here

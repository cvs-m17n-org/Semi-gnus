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

(luna-define-class mime-gnus-entity (mime-buffer-entity)
		   (number
		    subject from date id references chars lines xref extra))

(luna-define-internal-accessors 'mime-gnus-entity)

(luna-define-method initialize-instance ((entity mime-gnus-entity)
					 &rest init-args)
  (apply (car (luna-class-find-functions
	       (luna-find-class 'standard-object)
	       'initialize-instance))
	 entity init-args)
  )

;; (luna-define-method mime-entity-fetch-field ((entity mime-gnus-entity)
;;                                              field-name)
;;   (or (funcall (car (luna-class-find-functions
;;                      (luna-find-class 'mime-entity)
;;                      'mime-entity-fetch-field))
;;                entity field-name)
;;       (with-current-buffer gnus-original-article-buffer
;;         (let ((ret (std11-field-body field-name)))
;;           (when ret
;;             (or (symbolp field-name)
;;                 (setq field-name
;;                       (intern (capitalize (capitalize field-name)))))
;;             (mime-entity-set-original-header-internal
;;              entity
;;              (put-alist field-name ret
;;                         (mime-entity-original-header-internal entity)))
;;             ret)))))

;; (luna-define-method mime-entity-buffer ((entity mime-gnus-entity))
;;   ;; (if (with-current-buffer gnus-summary-buffer
;;   ;;       (eq gnus-current-article (mail-header-number entity)))
;;   ;;     ...)
;;   (unless (mime-buffer-entity-header-end-internal entity)
;;     (set-buffer gnus-original-article-buffer)
;;     (mime-buffer-entity-set-header-start-internal entity (point-min))
;;     (mime-buffer-entity-set-body-end-internal entity (point-max))
;;     (goto-char (point-min))
;;     (if (re-search-forward "^$" nil t)
;;         (progn
;;           (mime-buffer-entity-set-header-end-internal entity (match-end 0))
;;           (mime-buffer-entity-set-body-start-internal
;;            entity
;;            (if (= (mime-buffer-entity-header-end-internal entity)
;;                   (mime-buffer-entity-body-end-internal entity))
;;                (mime-buffer-entity-body-end-internal entity)
;;              (1+ (mime-buffer-entity-header-end-internal entity))
;;              ))
;;           )
;;       (mime-buffer-entity-set-header-end-internal entity (point-min))
;;       (mime-buffer-entity-set-body-start-internal entity (point-min))
;;       ))
;;   gnus-original-article-buffer)


;;; @ end
;;;

(provide 'mmgnus)

;;; mmgnus.el ends here

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

(condition-case nil
    (require 'mmgeneric)
  (error nil))
(require 'mime)
(require 'eword-decode)

(eval-and-compile
  (luna-define-class mmgnus-entity (mime-entity)
		     (body
		      header undisplayer content-description
		      cache content-id)))

(luna-define-internal-accessors 'mmgnus-entity)

(luna-define-method initialize-instance ((entity mmgnus-entity)
					 &rest init-args)
  (apply (car (luna-class-find-functions
	       (luna-find-class 'standard-object)
	       'initialize-instance))
	 entity init-args))

(defun mmgnus-visible-field-p (field-name visible-fields invisible-fields)
  (or (catch 'found
	(while visible-fields
	  (let ((regexp (car visible-fields)))
	    (if (string-match regexp field-name)
		(throw 'found t)
	      ))
	  (setq visible-fields (cdr visible-fields))
	  ))
      (catch 'found
	(while invisible-fields
	  (let ((regexp (car invisible-fields)))
	    (if (string-match regexp field-name)
		(throw 'found nil)
	      ))
	  (setq invisible-fields (cdr invisible-fields))
	  )
	t)))

(defun mmgnus-insert-header-from-string (string
					 &optional invisible-fields
					 visible-fields)
  (let ((the-buf (current-buffer))
	(mode-obj (mime-find-field-presentation-method 'wide))
	field-decoder
	f-b p f-e field-name len field field-body buffer)
    (with-temp-buffer
      (setq buffer (current-buffer))
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward std11-field-head-regexp nil t)
	(setq f-b (match-beginning 0)
	      p (match-end 0)
	      field-name (buffer-substring f-b p)
	      len (string-width field-name)
	      f-e (std11-field-end))
	(when (mmgnus-visible-field-p field-name
				      visible-fields invisible-fields)
	  (setq field (intern
		       (capitalize (buffer-substring f-b (1- p))))
		field-body (buffer-substring p f-e)
		field-decoder (inline (mime-find-field-decoder-internal
				       field mode-obj)))
	  (with-current-buffer the-buf
	    (insert field-name)
	    (insert (if field-decoder
			(funcall field-decoder field-body len)
		      ;; Don't decode
		      field-body))
	    (insert "\n")
	    ))))))

(defun mmgnus-entity-body (entity)
   (cond
    ((bufferp (mmgnus-entity-body-internal entity))
     (with-current-buffer (mmgnus-entity-body-internal entity)
       (buffer-string)))
    (t
     (error "Invalid body object. %s"
	    (mmgnus-entity-body-internal entity)))))

(luna-define-method mime-insert-header ((entity mmgnus-entity)
					&optional invisible-fields
					visible-fields)
  (mmgnus-insert-header-from-string
   (mmgnus-entity-header-internal entity)
   invisible-fields visible-fields))

(luna-define-method mime-entity-content ((entity mmgnus-entity))
  (mime-decode-string (mmgnus-entity-body entity)
		      (mime-entity-encoding entity)))

(luna-define-method mime-insert-entity-content ((entity mmgnus-entity))
  (insert (mime-entity-content entity)))

(luna-define-method mime-write-entity-content ((entity mmgnus-entity) filename)
  (with-temp-buffer
    (insert (mmgnus-entity-body entity))
    (mime-write-decoded-region (point-min) (point-max)
			       filename
			       (or (mime-entity-encoding entity) "7bit"))))

(luna-define-method mime-insert-entity ((entity mmgnus-entity))
  (insert (mmgnus-entity-header-internal entity)
	  "\n"
	  (mmgnus-entity-body entity)))

(luna-define-method mime-write-entity ((entity mmgnus-entity) filename)
  (with-temp-buffer
    (mime-insert-entity entity)
    (write-region-as-raw-text-CRLF (point-min) (point-max) filename)))

(luna-define-method mime-write-entity-body ((entity mmgnus-entity) filename)
  (with-temp-buffer
    (insert (mmgnus-entity-body entity))
    (write-region-as-binary (point-min) (point-max) filename)))

(eval-and-compile
  (luna-define-class mime-gnus-entity (mmgnus-entity)
		     (number
		      subject from date id references chars lines xref extra)))

(luna-define-internal-accessors 'mime-gnus-entity)

(luna-define-method initialize-instance ((entity mime-gnus-entity)
					 &rest init-args)
  (apply (car (luna-class-find-functions
	       (luna-find-class 'standard-object)
	       'initialize-instance))
	 entity init-args))

(luna-define-method mime-insert-header :around ((entity mime-gnus-entity)
						&optional invisible-fields
						visible-fields)
  (luna-call-next-method))

(luna-define-method mime-entity-content :around ((entity mime-gnus-entity))
  (luna-call-next-method))

;;; @ end
;;;

(provide 'mmgnus)

;;; mmgnus.el ends here

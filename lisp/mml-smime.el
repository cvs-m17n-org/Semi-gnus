;;; mml-smime.el --- S/MIME support for MML
;; Copyright (c) 2000 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: Gnus, MIME, SMIME, MML

;; This file is a part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; todo: move s/mime code from mml-sec.el here.

;;; Code:

(require 'smime)
(require 'mm-decode)

(defun mml-smime-verify (handle ctl)
  (with-current-buffer (mm-handle-multipart-original-buffer ctl)
    ;; xxx modifies buffer -- noone else uses the buffer, so what the heck
    (goto-char (point-min))
    (insert (format "Content-Type: %s; " (mm-handle-media-type ctl)))
    (insert (format "protocol=\"%s\"; " 
		    (mm-handle-multipart-ctl-parameter ctl 'protocol)))
    (insert (format "micalg=\"%s\"; " 
		    (mm-handle-multipart-ctl-parameter ctl 'micalg)))
    (insert (format "boundary=\"%s\"\n\n"
		    (mm-handle-multipart-ctl-parameter ctl 'boundary)))
    (when (get-buffer smime-details-buffer)
      (kill-buffer smime-details-buffer))
    (if (smime-verify-buffer)
	(progn
	  (mm-set-handle-multipart-parameter 
	   mm-security-handle 'gnus-info "OK")
	  (kill-buffer smime-details-buffer))
      (mm-set-handle-multipart-parameter 
       mm-security-handle 'gnus-info "Failed")
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-details (with-current-buffer smime-details-buffer 
					  (buffer-string))))
    handle))

(defun mml-smime-verify-test (handle ctl)
  smime-openssl-program)

(provide 'mml-smime)

;;; mml-smime.el ends here

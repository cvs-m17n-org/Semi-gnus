;;; mml2015.el --- MIME Security with Pretty Good Privacy (PGP)
;; Copyright (C) 2000, 2001 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: PGP MIME MML

;; This file is part of GNU Emacs.

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

;;; Code:

(eval-when-compile (require 'cl))
(require 'mm-decode)

(defvar mml2015-use (or
		     (progn
		       (ignore-errors
			 (require 'gpg))
		       (and (fboundp 'gpg-sign-detached)
			    'gpg))
		     (progn (ignore-errors
			      (load "mc-toplev"))
			    (and (fboundp 'mc-encrypt-generic)
				 (fboundp 'mc-sign-generic)
				 (fboundp 'mc-cleanup-recipient-headers)
				 'mailcrypt)))
  "The package used for PGP/MIME.")

;; Something is not RFC2015.
(defvar mml2015-function-alist
  '((mailcrypt mml2015-mailcrypt-sign
	       mml2015-mailcrypt-encrypt
	       mml2015-mailcrypt-verify
	       mml2015-mailcrypt-decrypt
	       mml2015-mailcrypt-clear-verify
	       mml2015-mailcrypt-clear-decrypt)
    (gpg mml2015-gpg-sign
	 mml2015-gpg-encrypt
	 mml2015-gpg-verify
	 mml2015-gpg-decrypt
	 mml2015-gpg-clear-verify
	 mml2015-gpg-clear-decrypt))
  "Alist of PGP/MIME functions.")

(defvar mml2015-result-buffer nil)

;;; mailcrypt wrapper

(eval-and-compile
  (autoload 'mailcrypt-decrypt "mailcrypt")
  (autoload 'mailcrypt-verify "mailcrypt")
  (autoload 'mc-pgp-always-sign "mailcrypt")
  (autoload 'mc-encrypt-generic "mc-toplev")
  (autoload 'mc-cleanup-recipient-headers "mc-toplev")
  (autoload 'mc-sign-generic "mc-toplev"))

(eval-when-compile
  (defvar mc-default-scheme)
  (defvar mc-schemes))

(defvar mml2015-decrypt-function 'mailcrypt-decrypt)
(defvar mml2015-verify-function 'mailcrypt-verify)

(defun mml2015-mailcrypt-decrypt (handle ctl)
  (catch 'error
    (let (child handles result)
      (unless (setq child (mm-find-part-by-type
			   (cdr handle)
			   "application/octet-stream" nil t))
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "Corrupted")
	(throw 'error handle))
      (with-temp-buffer
	(mm-insert-part child)
	(setq result
	      (condition-case err
		  (funcall mml2015-decrypt-function)
		(error
		 (mm-set-handle-multipart-parameter
		  mm-security-handle 'gnus-details (cadr err))
		 nil)
		(quit
		 (mm-set-handle-multipart-parameter
		  mm-security-handle 'gnus-details "Quit.")
		 nil)))
	(unless (car result)
	  (mm-set-handle-multipart-parameter
	   mm-security-handle 'gnus-info "Failed")
	  (throw 'error handle))
	(setq handles (mm-dissect-buffer t)))
      (mm-destroy-parts handle)
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info "OK")
      (if (listp (car handles))
	  handles
	(list handles)))))

(defun mml2015-mailcrypt-clear-decrypt ()
  (let (result)
    (setq result
	  (condition-case err
	      (funcall mml2015-decrypt-function)
	    (error
	     (mm-set-handle-multipart-parameter
	      mm-security-handle 'gnus-details (cadr err))
	     nil)
	    (quit
	     (mm-set-handle-multipart-parameter
	      mm-security-handle 'gnus-details "Quit.")
	     nil)))
    (if (car result)
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "OK")
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info "Failed"))))

(defun mml2015-fix-micalg (alg)
  (and alg
       (upcase (if (string-match "^pgp-" alg)
		   (substring alg (match-end 0))
		 alg))))

(defun mml2015-mailcrypt-verify (handle ctl)
  (catch 'error
    (let (part)
      (unless (setq part (mm-find-raw-part-by-type
			  ctl (or (mm-handle-multipart-ctl-parameter
				   ctl 'protocol)
				  "application/pgp-signature")
			  t))
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "Corrupted")
	(throw 'error handle))
      (with-temp-buffer
	(insert "-----BEGIN PGP SIGNED MESSAGE-----\n")
	(insert (format "Hash: %s\n\n"
			(or (mml2015-fix-micalg
			     (mm-handle-multipart-ctl-parameter
			      ctl 'micalg))
			    "SHA1")))
	(save-restriction
	  (narrow-to-region (point) (point))
	  (insert part "\n")
	  (goto-char (point-min))
	  (while (not (eobp))
	    (if (looking-at "^-")
		(insert "- "))
	    (forward-line)))
	(unless (setq part (mm-find-part-by-type
			    (cdr handle) "application/pgp-signature" nil t))
	  (mm-set-handle-multipart-parameter
	   mm-security-handle 'gnus-info "Corrupted")
	  (throw 'error handle))
	(save-restriction
	  (narrow-to-region (point) (point))
	  (mm-insert-part part)
	  (goto-char (point-min))
	  (if (re-search-forward "^-----BEGIN PGP [^-]+-----\r?$" nil t)
	      (replace-match "-----BEGIN PGP SIGNATURE-----" t t))
	  (if (re-search-forward "^-----END PGP [^-]+-----\r?$" nil t)
	      (replace-match "-----END PGP SIGNATURE-----" t t)))
	(unless (condition-case err
		    (funcall mml2015-verify-function)
		  (error
		   (mm-set-handle-multipart-parameter
		    mm-security-handle 'gnus-details (cadr err))
		   nil)
		  (quit
		   (mm-set-handle-multipart-parameter
		    mm-security-handle 'gnus-details "Quit.")
		   nil))
	  (mm-set-handle-multipart-parameter
	   mm-security-handle 'gnus-info "Failed")
	  (throw 'error handle)))
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info "OK")
      handle)))

(defun mml2015-mailcrypt-clear-verify ()
  (if (condition-case err
	  (funcall mml2015-verify-function)
	(error
	 (mm-set-handle-multipart-parameter
	  mm-security-handle 'gnus-details (cadr err))
	 nil)
	(quit
	 (mm-set-handle-multipart-parameter
	  mm-security-handle 'gnus-details "Quit.")
	 nil))
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info "OK")
    (mm-set-handle-multipart-parameter
     mm-security-handle 'gnus-info "Failed")))

(defun mml2015-mailcrypt-sign (cont)
  (mc-sign-generic (message-options-get 'message-sender)
		   nil nil nil nil)
  (let ((boundary
	 (funcall mml-boundary-function (incf mml-multipart-number)))
	hash point)
    (goto-char (point-min))
    (unless (re-search-forward "^-----BEGIN PGP SIGNED MESSAGE-----\r?$" nil t)
      (error "Cannot find signed begin line." ))
    (goto-char (match-beginning 0))
    (forward-line 1)
    (unless (looking-at "Hash:[ \t]*\\([a-zA-Z0-9]+\\)")
      (error "Cannot not find PGP hash." ))
    (setq hash (match-string 1))
    (unless (re-search-forward "^$" nil t)
      (error "Cannot not find PGP message." ))
    (forward-line 1)
    (delete-region (point-min) (point))
    (insert (format "Content-Type: multipart/signed; boundary=\"%s\";\n"
		    boundary))
    (insert (format "\tmicalg=pgp-%s; protocol=\"application/pgp-signature\"\n"
		    (downcase hash)))
    (insert (format "\n--%s\n" boundary))
    (setq point (point))
    (goto-char (point-max))
    (unless (re-search-backward "^-----END PGP SIGNATURE-----\r?$" nil t)
      (error "Cannot find signature part." ))
    (replace-match "-----END PGP MESSAGE-----" t t)
    (goto-char (match-beginning 0))
    (unless (re-search-backward "^-----BEGIN PGP SIGNATURE-----\r?$"
				nil t)
      (error "Cannot find signature part." ))
    (replace-match "-----BEGIN PGP MESSAGE-----" t t)
    (goto-char (match-beginning 0))
    (save-restriction
      (narrow-to-region point (point))
      (goto-char point)
      (while (re-search-forward "^- -" nil t)
	(replace-match "-" t t))
      (goto-char (point-max)))
    (insert (format "--%s\n" boundary))
    (insert "Content-Type: application/pgp-signature\n\n")
    (goto-char (point-max))
    (insert (format "--%s--\n" boundary))
    (goto-char (point-max))))

(defun mml2015-mailcrypt-encrypt (cont)
  (let ((mc-pgp-always-sign
	 (or mc-pgp-always-sign
	     (eq t (or (message-options-get 'message-sign-encrypt)
		       (message-options-set
			'message-sign-encrypt
			(or (y-or-n-p "Sign the message? ")
			    'not))))
	     'never)))
    (mm-with-unibyte-current-buffer-mule4
      (mc-encrypt-generic
       (or (message-options-get 'message-recipients)
	   (message-options-set 'message-recipients
			      (mc-cleanup-recipient-headers
			       (read-string "Recipients: "))))
       nil nil nil
       (message-options-get 'message-sender))))
  (goto-char (point-min))
  (unless (looking-at "-----BEGIN PGP MESSAGE-----")
    (error "Fail to encrypt the message."))
  (let ((boundary
	 (funcall mml-boundary-function (incf mml-multipart-number))))
    (insert (format "Content-Type: multipart/encrypted; boundary=\"%s\";\n"
		    boundary))
    (insert "\tprotocol=\"application/pgp-encrypted\"\n\n")
    (insert (format "--%s\n" boundary))
    (insert "Content-Type: application/pgp-encrypted\n\n")
    (insert "Version: 1\n\n")
    (insert (format "--%s\n" boundary))
    (insert "Content-Type: application/octet-stream\n\n")
    (goto-char (point-max))
    (insert (format "--%s--\n" boundary))
    (goto-char (point-max))))

;;; gpg wrapper

(eval-and-compile
  (autoload 'gpg-decrypt "gpg")
  (autoload 'gpg-verify "gpg")
  (autoload 'gpg-verify-cleartext "gpg")
  (autoload 'gpg-sign-detached "gpg")
  (autoload 'gpg-sign-encrypt "gpg")
  (autoload 'gpg-passphrase-read "gpg"))

(defun mml2015-gpg-passphrase ()
  (or (message-options-get 'gpg-passphrase)
      (message-options-set 'gpg-passphrase (gpg-passphrase-read))))

(defun mml2015-gpg-decrypt-1 ()
  (let ((cipher (current-buffer)) plain result)
    (if (with-temp-buffer
	  (prog1
	      (gpg-decrypt cipher (setq plain (current-buffer))
			   mml2015-result-buffer nil)
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-details
	     (with-current-buffer mml2015-result-buffer
	       (buffer-string)))
	    (set-buffer cipher)
	    (erase-buffer)
	    (insert-buffer plain)))
	'(t)
      ;; Some wrong with the return value, check plain text buffer.
      (if (> (point-max) (point-min))
	  '(t)
	nil))))

(defun mml2015-gpg-decrypt (handle ctl)
  (let ((mml2015-decrypt-function 'mml2015-gpg-decrypt-1))
    (mml2015-mailcrypt-decrypt handle ctl)))

(defun mml2015-gpg-clear-decrypt ()
  (let (result)
    (setq result (mml2015-gpg-decrypt-1))
    (if (car result)
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "OK")
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info "Failed"))))

(defun mml2015-gpg-verify (handle ctl)
  (catch 'error
    (let (part message signature)
      (unless (setq part (mm-find-raw-part-by-type
			  ctl (or (mm-handle-multipart-ctl-parameter
				   ctl 'protocol)
				  "application/pgp-signature")
			  t))
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "Corrupted")
	(throw 'error handle))
      (with-temp-buffer
	(setq message (current-buffer))
	(insert part)
	(with-temp-buffer
	  (setq signature (current-buffer))
	  (unless (setq part (mm-find-part-by-type
			      (cdr handle) "application/pgp-signature" nil t))
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-info "Corrupted")
	    (throw 'error handle))
	  (mm-insert-part part)
	  (unless (condition-case err
		      (prog1
			  (gpg-verify message signature mml2015-result-buffer)
			(mm-set-handle-multipart-parameter
			 mm-security-handle 'gnus-details
			 (with-current-buffer mml2015-result-buffer
			   (buffer-string))))
		    (error
		     (mm-set-handle-multipart-parameter
		      mm-security-handle 'gnus-details (cadr err))
		     nil)
		    (quit
		     (mm-set-handle-multipart-parameter
		      mm-security-handle 'gnus-details "Quit.")
		     nil))
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-info "Failed")
	    (throw 'error handle)))
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "OK"))
      handle)))

(defun mml2015-gpg-clear-verify ()
  (if (condition-case err
	  (prog1
	      (gpg-verify-cleartext (current-buffer) mml2015-result-buffer)
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-details
	     (with-current-buffer mml2015-result-buffer
	       (buffer-string))))
	(error
	 (mm-set-handle-multipart-parameter
	  mm-security-handle 'gnus-details (cadr err))
	 nil)
	(quit
	 (mm-set-handle-multipart-parameter
	  mm-security-handle 'gnus-details "Quit.")
	 nil))
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info "OK")
    (mm-set-handle-multipart-parameter
     mm-security-handle 'gnus-info "Failed")))

(defun mml2015-gpg-sign (cont)
  (let ((boundary
	 (funcall mml-boundary-function (incf mml-multipart-number)))
	(text (current-buffer)) signature)
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (with-temp-buffer
      (unless (gpg-sign-detached text (setq signature (current-buffer))
				 mml2015-result-buffer
				 nil
				 (message-options-get 'message-sender)
				 t t) ; armor & textmode
	(unless (> (point-max) (point-min))
	  (pop-to-buffer mml2015-result-buffer)
	  (error "Sign error.")))
      (goto-char (point-min))
      (while (re-search-forward "\r+$" nil t)
	(replace-match "" t t))
      (set-buffer text)
      (goto-char (point-min))
      (insert (format "Content-Type: multipart/signed; boundary=\"%s\";\n"
		      boundary))
      ;;; FIXME: what is the micalg?
      (insert "\tmicalg=pgp-sha1; protocol=\"application/pgp-signature\"\n")
      (insert (format "\n--%s\n" boundary))
      (goto-char (point-max))
      (insert (format "\n--%s\n" boundary))
      (insert "Content-Type: application/pgp-signature\n\n")
      (insert-buffer signature)
      (goto-char (point-max))
      (insert (format "--%s--\n" boundary))
      (goto-char (point-max)))))

(defun mml2015-gpg-encrypt (cont)
  (let ((boundary
	 (funcall mml-boundary-function (incf mml-multipart-number)))
	(text (current-buffer))
	cipher)
    (mm-with-unibyte-current-buffer-mule4
      (with-temp-buffer
	(unless (gpg-sign-encrypt
		 text (setq cipher (current-buffer))
		 mml2015-result-buffer
		 (split-string
		  (or
		   (message-options-get 'message-recipients)
		   (message-options-set 'message-recipients
					(read-string "Recipients: ")))
		  "[ \f\t\n\r\v,]+")
		 nil
		 (message-options-get 'message-sender)
		 t t) ; armor & textmode
	  (unless (> (point-max) (point-min))
	    (pop-to-buffer mml2015-result-buffer)
	    (error "Encrypt error.")))
	(goto-char (point-min))
	(while (re-search-forward "\r+$" nil t)
	  (replace-match "" t t))
	(set-buffer text)
	(delete-region (point-min) (point-max))
	(insert (format "Content-Type: multipart/encrypted; boundary=\"%s\";\n"
			boundary))
	(insert "\tprotocol=\"application/pgp-encrypted\"\n\n")
	(insert (format "--%s\n" boundary))
	(insert "Content-Type: application/pgp-encrypted\n\n")
	(insert "Version: 1\n\n")
	(insert (format "--%s\n" boundary))
	(insert "Content-Type: application/octet-stream\n\n")
	(insert-buffer cipher)
	(goto-char (point-max))
	(insert (format "--%s--\n" boundary))
	(goto-char (point-max))))))

;;; General wrapper

(defun mml2015-clean-buffer ()
  (if (gnus-buffer-live-p mml2015-result-buffer)
      (with-current-buffer mml2015-result-buffer
	(erase-buffer)
	t)
    (setq mml2015-result-buffer
	  (gnus-get-buffer-create "*MML2015 Result*"))
    nil))

(defsubst mml2015-clear-decrypt-function ()
  (nth 6 (assq mml2015-use mml2015-function-alist)))

(defsubst mml2015-clear-verify-function ()
  (nth 5 (assq mml2015-use mml2015-function-alist)))

;;;###autoload
(defun mml2015-decrypt (handle ctl)
  (mml2015-clean-buffer)
  (let ((func (nth 4 (assq mml2015-use mml2015-function-alist))))
    (if func
	(funcall func handle ctl)
      handle)))

;;;###autoload
(defun mml2015-decrypt-test (handle ctl)
  mml2015-use)

;;;###autoload
(defun mml2015-verify (handle ctl)
  (mml2015-clean-buffer)
  (let ((func (nth 3 (assq mml2015-use mml2015-function-alist))))
    (if func
	(funcall func handle ctl)
      handle)))

;;;###autoload
(defun mml2015-verify-test (handle ctl)
  mml2015-use)

;;;###autoload
(defun mml2015-encrypt (cont)
  (mml2015-clean-buffer)
  (let ((func (nth 2 (assq mml2015-use mml2015-function-alist))))
    (if func
	(funcall func cont)
      (error "Cannot find encrypt function."))))

;;;###autoload
(defun mml2015-sign (cont)
  (mml2015-clean-buffer)
  (let ((func (nth 1 (assq mml2015-use mml2015-function-alist))))
    (if func
	(funcall func cont)
      (error "Cannot find sign function."))))

;;;###autoload
(defun mml2015-self-encrypt ()
  (mml2015-encrypt nil))

(provide 'mml2015)

;;; mml2015.el ends here

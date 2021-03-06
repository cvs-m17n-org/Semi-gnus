;;; gnus-dired.el --- utility functions where gnus and dired meet

;; Copyright (C) 1996, 1997, 1998, 1999, 2001, 2002, 2003, 2004,
;;   2005, 2006 Free Software Foundation, Inc.

;; Authors: Benjamin Rutt <brutt@bloomington.in.us>,
;;          Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: mail, news, extensions

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides utility functions for intersections of gnus
;; and dired.  To enable the gnus-dired-mode minor mode which will
;; have the effect of installing keybindings in dired-mode, place the
;; following in your ~/.gnus:

;; (require 'gnus-dired) ;, isn't needed due to autoload cookies
;; (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; Note that if you visit dired buffers before your ~/.gnus file has
;; been read, those dired buffers won't have the keybindings in
;; effect.  To get around that problem, you may want to add the above
;; statements to your ~/.emacs instead.

;;; Code:

(require 'dired)
(require 'gnus-ems)
(require 'gnus-msg)
(require 'gnus-util)
(require 'message)
(require 'mm-encode)
(require 'mml)

(defvar gnus-dired-mode nil
  "Minor mode for intersections of gnus and dired.")

(defvar gnus-dired-mode-map nil)

(unless gnus-dired-mode-map
  (setq gnus-dired-mode-map (make-sparse-keymap))

  (gnus-define-keys gnus-dired-mode-map
    "\C-c\C-m\C-a" gnus-dired-attach
    "\C-c\C-m\C-l" gnus-dired-find-file-mailcap
    "\C-c\C-m\C-p" gnus-dired-print))

(defun gnus-dired-mode (&optional arg)
  "Minor mode for intersections of gnus and dired.

\\{gnus-dired-mode-map}"
  (interactive "P")
  (when (eq major-mode 'dired-mode)
    (set (make-local-variable 'gnus-dired-mode)
	 (if (null arg) (not gnus-dired-mode)
	   (> (prefix-numeric-value arg) 0)))
    (when gnus-dired-mode
      (add-minor-mode 'gnus-dired-mode "" gnus-dired-mode-map)
      (gnus-run-hooks 'gnus-dired-mode-hook))))

;;;###autoload
(defun turn-on-gnus-dired-mode ()
  "Convenience method to turn on gnus-dired-mode."
  (gnus-dired-mode 1))

;; Method to attach files to a gnus composition.
(defun gnus-dired-attach (files-to-attach)
  "Attach dired's marked files to a gnus message composition.
If called non-interactively, FILES-TO-ATTACH should be a list of
filenames."
  (interactive
   (list
    (delq nil
	  (mapcar
	   ;; don't attach directories
	   (lambda (f) (if (file-directory-p f) nil f))
	   (nreverse (dired-map-over-marks (dired-get-filename) nil))))))
  (let ((destination nil)
	(files-str nil)
	(bufs nil))
    ;; warn if user tries to attach without any files marked
    (if (null files-to-attach)
	(error "No files to attach")
      (setq files-str
	    (mapconcat
	     (lambda (f) (file-name-nondirectory f))
	     files-to-attach ", "))
      (setq bufs (message-buffers))

      ;; set up destination message buffer
      (if (and bufs
	       (y-or-n-p "Attach files to existing message buffer? "))
	  (setq destination
		(if (= (length bufs) 1)
		    (get-buffer (car bufs))
		  (completing-read "Attach to which message buffer: "
				   (mapcar
				    (lambda (b)
				      (cons b (get-buffer b)))
				    bufs)
				   nil t)))
	;; setup a new gnus message buffer
	(gnus-setup-message 'message (message-mail))
	(setq destination (current-buffer)))

      ;; set buffer to destination buffer, and attach files
      (set-buffer destination)
      (goto-char (point-max))		;attach at end of buffer
      (while files-to-attach
	(mml-attach-file (car files-to-attach)
			 (or (mm-default-file-encoding (car files-to-attach))
			     "application/octet-stream") nil)
	(setq files-to-attach (cdr files-to-attach)))
      (message "Attached file(s) %s" files-str))))

(autoload 'mailcap-parse-mailcaps "mailcap" "" t)

(defun gnus-dired-find-file-mailcap (&optional file-name arg)
  "In dired, visit FILE-NAME according to the mailcap file.
If ARG is non-nil, open it in a new buffer."
  (interactive (list
		(file-name-sans-versions (dired-get-filename) t)
		current-prefix-arg))
  (mailcap-parse-mailcaps)
  (if (file-exists-p file-name)
      (let (mime-type method)
	(if (and (not arg)
		 (not (file-directory-p file-name))
		 (string-match "\\.[^\\.]+$" file-name)
		 (setq mime-type
		       (mailcap-extension-to-mime
			(match-string 0 file-name)))
		 (stringp
		  (setq method
			(cdr (assoc 'viewer
				    (car (mailcap-mime-info mime-type
							    'all)))))))
	    (let ((view-command (mm-mailcap-command method file-name nil)))
	      (message "viewing via %s" view-command)
	      (start-process "*display*"
			     nil
			     shell-file-name
			     shell-command-switch
			     view-command))
	  (find-file file-name)))
    (if (file-symlink-p file-name)
	(error "File is a symlink to a nonexistent target")
      (error "File no longer exists; type `g' to update Dired buffer"))))

(defun gnus-dired-print (&optional file-name print-to)
  "In dired, print FILE-NAME according to the mailcap file.

If there is no print command, print in a PostScript image. If the
optional argument PRINT-TO is nil, send the image to the printer. If
PRINT-TO is a string, save the PostScript image in a file with that
name.  If PRINT-TO is a number, prompt the user for the name of the
file to save in."
  (interactive (list
		(file-name-sans-versions (dired-get-filename) t)
		(ps-print-preprint current-prefix-arg)))
  (mailcap-parse-mailcaps)
  (cond
   ((file-directory-p file-name)
    (error "Can't print a directory"))
   ((file-exists-p file-name)
    (let (mime-type method)
      (if (and (string-match "\\.[^\\.]+$" file-name)
	       (setq mime-type
		     (mailcap-extension-to-mime
		      (match-string 0 file-name)))
	       (stringp
		(setq method (mailcap-mime-info mime-type "print"))))
	  (call-process shell-file-name nil
			(generate-new-buffer " *mm*")
			nil
			shell-command-switch
			(mm-mailcap-command method file-name mime-type))
	(with-temp-buffer
	  (insert-file-contents file-name)
	  (gnus-print-buffer))
	(ps-despool print-to))))
   ((file-symlink-p file-name)
     (error "File is a symlink to a nonexistent target"))
    (t
     (error "File no longer exists; type `g' to update Dired buffer"))))

(provide 'gnus-dired)

;;; gnus-dired.el ends here

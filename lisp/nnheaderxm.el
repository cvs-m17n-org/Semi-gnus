;;; nnheaderxm.el --- making Gnus backends work under XEmacs

;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;;      Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	Katsumi Yamaoka  <yamaoka@jpl.org>
;; Keywords: news

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defun nnheader-xmas-run-at-time (time repeat function &rest args)
  (start-itimer
   "nnheader-run-at-time"
   `(lambda ()
      (,function ,@args))
   time repeat))

(defun nnheader-xmas-Y-or-n-p (prompt)
  "Ask user a \"Y/n\" question. Return t if answer is neither \"n\", \"N\" nor \"C-g\"."
  (if (should-use-dialog-box-p)
      (yes-or-no-p-dialog-box prompt)
    (let ((cursor-in-echo-area t)
	  (echo-keystrokes 0)
	  (inhibit-quit t)
	  event)
      (message "%s(Y/n) " prompt)
      (while (or (not (key-press-event-p (setq event (next-command-event))))
		 (not (or (eq (event-key event) 'escape)
			  (memq (event-to-character event)
				'(?\  ?N ?Y ?\C-g ?\e ?\n ?\r ?n ?y))))))
      (if (memq (event-key event) '(?\C-g ?N ?n))
	  (progn
	    (message "%s(Y/n) No" prompt)
	    nil)
	(message "%s(Y/n) Yes" prompt)
	t))))

(defalias 'nnheader-run-at-time 'nnheader-xmas-run-at-time)
(defalias 'nnheader-cancel-timer 'delete-itimer)
(defalias 'nnheader-cancel-function-timers 'ignore)
(defalias 'nnheader-Y-or-n-p 'nnheader-xmas-Y-or-n-p)

(provide 'nnheaderxm)

;;; nnheaderxm.el ends here.

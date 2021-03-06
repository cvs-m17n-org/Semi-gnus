;;; smiley.el --- displaying smiley faces

;; Copyright (C) 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: news mail multimedia

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A re-written, simplified version of Wes Hardaker's XEmacs smiley.el
;; which might be merged back to smiley.el if we get an assignment for
;; that.  We don't have assignments for the images smiley.el uses, but
;; I'm not sure we need that degree of rococoness and defaults like a
;; yellow background.  Also, using PBM means we can display the images
;; more generally.  -- fx
;; `smiley.el' was replaced by `smiley-ems.el' on 2002-01-26 (after fx'
;; comment).

;; Test smileys:
;; smile             ^:-) ^:)
;; blink             ;-)  ;)
;; forced            :-]
;; braindamaged      8-)
;; indifferent       :-|
;; wry               :-/  :-\
;; sad               :-(
;; frown             :-{
;; evil              >:-)
;; cry               ;-(
;; dead              X-)
;; grin              :-D

;;; Code:

(eval-when-compile (require 'cl))
(require 'nnheader)
(require 'gnus-art)

(defgroup smiley nil
  "Turn :-)'s into real images."
  :group 'gnus-visual)

;; Maybe this should go.
(defcustom smiley-data-directory
  (nnheader-find-etc-directory "images/smilies")
  "Location of the smiley faces files."
  :type 'directory
  :group 'smiley)

;; The XEmacs version has a baroque, if not rococo, set of these.
(defcustom smiley-regexp-alist
  (if (file-exists-p (expand-file-name "WideFaceSmile.xbm"
				       smiley-data-directory))
      ;; Use faces in ftp://ftp.gnus.org/pub/gnus/etc-0.27.tar.gz
      '(;; ^_^ ^^
	("\\(\\^_?\\^\\)\\W" 1 "WideFaceSmile")
	;; ;-> ;-) ;-} ;> ;) :}
	("\\(;-?[>)}]+\\)\\W" 1 "FaceWinking")
	;; ^_^; ^^;
	("\\(\\^_?\\^;\\)\\W" 1 "WideFaceAse1")
	;; ^_^;; ^^;;
	("\\(\\^_?\\^;;\\)\\W" 1 "WideFaceAse2")
	;; ^_^;;; ^^;;;
	("\\(\\^_?\\^;;;\\)\\W" 1 "WideFaceAse3")
	;; ;_;
	("\\(;_;\\)\\W" 1 "WideFaceWeep")
	;; T_T
	("\\(T_T\\)\\W" 1 "WideFaceWeep")
	;; >_<
	("\\(>_<\\)\\W" 1 "WideFaceWeep")
	;; :-< :<
	("\\(:-?<\\)\\W" 1 "FaceAngry")
	;; :-] :]
	("\\(:-?\\]+\\)\\W" 1 "FaceGoofy")
	;; :-D :D
	("\\(:-?D\\)\\W" 1 "FaceGrinning")
	;; :-) :-> :-} :) :> :}
	("\\(:-?[)>}]+\\)\\W" 1 "FaceHappy")
	;; =)
	("\\(=)\\)\\W" 1 "FaceHappy")
	;; :-/ :-\ :/ :\  excludes urls etc.
	("\\(:-[/\\]\\)\\W" 1 "FaceIronic")
	("\\(:/\\)\\([\t\n ]\\|[^/]\\W\\)" 1 "FaceIronic")
	("\\(:\\\\\\)\\([\t\n ]\\|[^\\]\\W\\)" 1 "FaceIronic")
	;; 8-| 8-O 8-%
	;; excludes just numbers
	("[^.0-9]\\(8-[|O%]\\)\\W" 1 "FaceKOed")
	;; :-# :#
	("\\(:-?#\\)\\W" 1 "FaceNyah")
	;; :-( :-{ :( :{
	("\\(:-?[({]+\\)\\W" 1 "FaceSad")
	;; =( ={
	("\\(=[({]+\\)\\W" 1 "FaceSad")
	;; :-O :-o :O :o
	("\\(:-?[Oo]\\)\\W" 1 "FaceStartled")
	;; :-| :|
	("\\(:-?|\\)\\W" 1 "FaceStraight")
	;; :-p :p
	("\\(:-?p\\)\\W" 1 "FaceTalking")
	;; :-d
	("\\(:-d\\)\\W" 1 "FaceTasty")
	;; :-V :-v :V :v
	("\\(:-?[Vv]\\)\\W" 1 "FaceWry")
	;; :-P :P
	("\\(:-?P\\)\\W" 1 "FaceYukky")
	;; ]:-) ]:-> ]:-} ]8-) ]8-> ]8-} ]B-) ]B-> ]B-}
	;; ]:) ]:> ]:} ]8) ]8> ]8} ]B) ]B> ]B}
	("\\(\\][:8B]-?[)>}]\\)\\W" 1 "FaceDevilish"))
    '(("\\(:-?)\\)\\W" 1 "smile")
      ("\\(;-?)\\)\\W" 1 "blink")
      ("\\(:-]\\)\\W" 1 "forced")
      ("\\(8-)\\)\\W" 1 "braindamaged")
      ("\\(:-|\\)\\W" 1 "indifferent")
      ("\\(:-[/\\]\\)\\W" 1 "wry")
      ("\\(:-(\\)\\W" 1 "sad")
      ("\\(:-{\\)\\W" 1 "frown")))
  "*A list of regexps to map smilies to images.
The elements are (REGEXP MATCH IMAGE), where MATCH is the submatch in
regexp to replace with IMAGE.  IMAGE is the name of an image file in
`smiley-data-directory'."
  :type '(repeat (list regexp
		       (integer :tag "Regexp match number")
		       (string :tag "Image name")))
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (smiley-update-cache))
  :initialize 'custom-initialize-default
  :group 'smiley)

(defcustom gnus-smiley-file-types
  (let ((types (list "pbm")))
    (when (gnus-image-type-available-p 'xpm)
      (push "xpm" types))
    (when (gnus-image-type-available-p 'xbm)
      (push "xbm" types))
    types)
  "*List of suffixes on smiley file names to try."
  :version "22.1"
  :type '(repeat string)
  :group 'smiley)

(defvar smiley-cached-regexp-alist nil)

(defun smiley-update-cache ()
  (setq smiley-cached-regexp-alist nil)
  (dolist (elt (if (symbolp smiley-regexp-alist)
		   (symbol-value smiley-regexp-alist)
		 smiley-regexp-alist))
    (let ((types gnus-smiley-file-types)
	  file type)
      (while (and (not file)
		  (setq type (pop types)))
	(unless (file-exists-p
		 (setq file (expand-file-name (concat (nth 2 elt) "." type)
					      smiley-data-directory)))
	  (setq file nil)))
      (when type
	(let ((image (gnus-create-image file (intern type) nil
					:ascent 'center)))
	  (when image
	    (push (list (car elt) (cadr elt) image)
		  smiley-cached-regexp-alist)))))))

;; Not implemented:
;; (defvar smiley-mouse-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [down-mouse-2] 'ignore) ; override widget
;;     (define-key map [mouse-2]
;;       'smiley-mouse-toggle-buffer)
;;     map))

;;;###autoload
(defun smiley-region (start end)
  "Replace in the region `smiley-regexp-alist' matches with corresponding images.
A list of images is returned."
  (interactive "r")
  (when (gnus-graphic-display-p)
    (unless smiley-cached-regexp-alist
      (smiley-update-cache))
    (save-excursion
      (let ((beg (or start (point-min)))
	    group image images string)
	(dolist (entry smiley-cached-regexp-alist)
	  (setq group (nth 1 entry)
		image (nth 2 entry))
	  (goto-char beg)
	  (while (re-search-forward (car entry) end t)
	    (goto-char (match-end group))
	    (unless (text-property-any (match-beginning group) (point)
				       'smilified t)
	      (setq string (match-string group))
	      (delete-region (match-beginning group) (match-end group))
	      (when image
		(push image images)
		(gnus-add-wash-type 'smiley)
		(gnus-add-image 'smiley image)
		(put-text-property (point)
				   (progn
				     (gnus-put-image image string 'smiley)
				     (point))
				   'smilified t)))))
	(put-text-property beg (or end (point-max)) 'smilified nil)
	images))))

;;;###autoload
(defun smiley-buffer (&optional buffer)
  "Run `smiley-region' at the buffer, specified in the argument or
interactively. If there's no argument, do it at the current buffer"
  (interactive "bBuffer to run smiley-region: ")
  (save-excursion
    (if buffer
	(set-buffer (get-buffer buffer)))
    (smiley-region (point-min) (point-max))))

(defun smiley-toggle-buffer (&optional arg)
  "Toggle displaying smiley faces in article buffer.
With arg, turn displaying on if and only if arg is positive."
  (interactive "P")
  (gnus-with-article-buffer
    (if (if (numberp arg)
	    (> arg 0)
	  (not (memq 'smiley gnus-article-wash-types)))
	(smiley-region (point-min) (point-max))
      (gnus-delete-images 'smiley))))

(defun smiley-mouse-toggle-buffer (event)
  "Toggle displaying smiley faces.
With arg, turn displaying on if and only if arg is positive."
  (interactive "e")
  (save-excursion
    (save-window-excursion
      (mouse-set-point event)
      (smiley-toggle-buffer))))

(provide 'smiley)

;;; smiley.el ends here

;;; mm-url.el --- a wrapper of url functions/commands for Gnus
;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>

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

;; Some codes are stolen from w3 and url packages. Some are moved from
;; nnweb.

;; TODO: Support POST, cookie.

;;; Code:

(eval-when-compile (require 'cl))

(require 'mm-util)

(eval-and-compile
  (autoload 'exec-installed-p "path-util")
  (autoload 'url-insert-file-contents "url-handlers"))

(defgroup mm-url nil
  "A wrapper of url package and external url command for Gnus."
  :group 'gnus)

(defcustom mm-url-use-external (not
				(condition-case nil
				    (require 'url-handlers)
				  (error nil)))
  "*If not-nil, use external grab program `mm-url-program'."
  :type 'boolean
  :group 'mm-url)

(defvar mm-url-predefined-programs
  '((wget "wget" "-q" "-O" "-")
    (lynx "lynx" "-source")
    (curl "curl")))

(defcustom mm-url-program
  (cond
   ((exec-installed-p "wget") 'wget)
   ((exec-installed-p "lynx") 'lynx)
   ((exec-installed-p "curl") 'curl)
   (t "GET"))
  "The url grab program."
  :type '(choice
	  (symbol :tag "wget" wget)
	  (symbol :tag "lynx" lynx)
	  (symbol :tag "curl" curl)
	  (string :tag "other"))
  :group 'mm-url)

(defcustom mm-url-arguments nil
  "The arguments for `mm-url-program'."
  :type '(repeat string)
  :group 'mm-url)

;; Stolen from w3.
(defvar mm-url-html-entities
  '(
    ;;(excl        .  33)
    (quot        .  34)
    ;;(num         .  35)
    ;;(dollar      .  36)
    ;;(percent     .  37)
    (amp         .  38)
    (rsquo       .  39)			; should be U+8217
    ;;(apos        .  39)
    ;;(lpar        .  40)
    ;;(rpar        .  41)
    ;;(ast         .  42)
    ;;(plus        .  43)
    ;;(comma       .  44)
    ;;(period      .  46)
    ;;(colon       .  58)
    ;;(semi        .  59)
    (lt          .  60)
    ;;(equals      .  61)
    (gt          .  62)
    ;;(quest       .  63)
    ;;(commat      .  64)
    ;;(lsqb        .  91)
    ;;(rsqb        .  93)
    (uarr        .  94)			; should be U+8593
    ;;(lowbar      .  95)
    (lsquo       .  96)			; should be U+8216
    (lcub        . 123)
    ;;(verbar      . 124)
    (rcub        . 125)
    (tilde       . 126)
    (nbsp        . 160)
    (iexcl       . 161)
    (cent        . 162)
    (pound       . 163)
    (curren      . 164)
    (yen         . 165)
    (brvbar      . 166)
    (sect        . 167)
    (uml         . 168)
    (copy        . 169)
    (ordf        . 170)
    (laquo       . 171)
    (not         . 172)
    (shy         . 173)
    (reg         . 174)
    (macr        . 175)
    (deg         . 176)
    (plusmn      . 177)
    (sup2        . 178)
    (sup3        . 179)
    (acute       . 180)
    (micro       . 181)
    (para        . 182)
    (middot      . 183)
    (cedil       . 184)
    (sup1        . 185)
    (ordm        . 186)
    (raquo       . 187)
    (frac14      . 188)
    (frac12      . 189)
    (frac34      . 190)
    (iquest      . 191)
    (Agrave      . 192)
    (Aacute      . 193)
    (Acirc       . 194)
    (Atilde      . 195)
    (Auml        . 196)
    (Aring       . 197)
    (AElig       . 198)
    (Ccedil      . 199)
    (Egrave      . 200)
    (Eacute      . 201)
    (Ecirc       . 202)
    (Euml        . 203)
    (Igrave      . 204)
    (Iacute      . 205)
    (Icirc       . 206)
    (Iuml        . 207)
    (ETH         . 208)
    (Ntilde      . 209)
    (Ograve      . 210)
    (Oacute      . 211)
    (Ocirc       . 212)
    (Otilde      . 213)
    (Ouml        . 214)
    (times       . 215)
    (Oslash      . 216)
    (Ugrave      . 217)
    (Uacute      . 218)
    (Ucirc       . 219)
    (Uuml        . 220)
    (Yacute      . 221)
    (THORN       . 222)
    (szlig       . 223)
    (agrave      . 224)
    (aacute      . 225)
    (acirc       . 226)
    (atilde      . 227)
    (auml        . 228)
    (aring       . 229)
    (aelig       . 230)
    (ccedil      . 231)
    (egrave      . 232)
    (eacute      . 233)
    (ecirc       . 234)
    (euml        . 235)
    (igrave      . 236)
    (iacute      . 237)
    (icirc       . 238)
    (iuml        . 239)
    (eth         . 240)
    (ntilde      . 241)
    (ograve      . 242)
    (oacute      . 243)
    (ocirc       . 244)
    (otilde      . 245)
    (ouml        . 246)
    (divide      . 247)
    (oslash      . 248)
    (ugrave      . 249)
    (uacute      . 250)
    (ucirc       . 251)
    (uuml        . 252)
    (yacute      . 253)
    (thorn       . 254)
    (yuml        . 255)

    ;; Special handling of these
    (frac56      . "5/6")
    (frac16      . "1/6")
    (frac45      . "4/5")
    (frac35      . "3/5")
    (frac25      . "2/5")
    (frac15      . "1/5")
    (frac23      . "2/3")
    (frac13      . "1/3")
    (frac78      . "7/8")
    (frac58      . "5/8")
    (frac38      . "3/8")
    (frac18      . "1/8")

    ;; The following 5 entities are not mentioned in the HTML 2.0
    ;; standard, nor in any other HTML proposed standard of which I
    ;; am aware.  I am not even sure they are ISO entity names.  ***
    ;; Hence, some arrangement should be made to give a bad HTML
    ;; message when they are seen.
    (ndash       .  45)
    (mdash       .  45)
    (emsp        .  32)
    (ensp        .  32)
    (sim         . 126)
    (le          . "<=")
    (agr         . "alpha")
    (rdquo       . "''")
    (ldquo       . "``")
    (trade       . "(TM)")
    ;; To be done
    ;; (shy      . ????) ; soft hyphen
    )
  "*An assoc list of entity names and how to actually display them.")

(defconst mm-url-unreserved-chars
  '(
    ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
    ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
    ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\))
  "A list of characters that are _NOT_ reserved in the URL spec.
This is taken from RFC 2396.")

(defun mm-url-insert-file-contents (url)
  (if mm-url-use-external
      (if (string-match "^file:/+" url)
	  (insert-file-contents (substring url (1- (match-end 0))))
	(mm-url-insert-file-contents-external url))
    (require 'url-handlers)
    (let ((name buffer-file-name))
      (prog1
	  (url-insert-file-contents url)
	(setq buffer-file-name name)))))

(defun mm-url-insert-file-contents-external (url)
  (let (program args)
    (if (symbolp mm-url-program)
	(let ((item (cdr (assq mm-url-program mm-url-predefined-programs))))
	  (setq program (car item)
		args (append (cdr item) (list url))))
      (setq program mm-url-program
	    args (append mm-url-arguments (list url))))
    (apply 'call-process program nil t nil args)))

(defun mm-url-insert (url &optional follow-refresh)
  "Insert the contents from an URL in the current buffer.
If FOLLOW-REFRESH is non-nil, redirect refresh url in META."
  (if follow-refresh
      (save-restriction
	(narrow-to-region (point) (point))
	(mm-url-insert-file-contents url)
	(goto-char (point-min))
	(when (re-search-forward
	       "<meta[ \t\r\n]*http-equiv=\"Refresh\"[^>]*URL=\\([^\"]+\\)\"" nil t)
	  (let ((url (match-string 1)))
	    (delete-region (point-min) (point-max))
	    (mm-url-insert url t))))
    (mm-url-insert-file-contents url)))

(defun mm-url-decode-entities ()
  "Decode all HTML entities."
  (goto-char (point-min))
  (while (re-search-forward "&\\(#[0-9]+\\|[a-z]+\\);" nil t)
    (let ((elem (if (eq (aref (match-string 1) 0) ?\#)
			(let ((c
			       (string-to-number (substring
						  (match-string 1) 1))))
			  (if (mm-char-or-char-int-p c) c 32))
		      (or (cdr (assq (intern (match-string 1))
				     mm-url-html-entities))
			  ?#))))
      (unless (stringp elem)
	(setq elem (char-to-string elem)))
      (replace-match elem t t))))

(defun mm-url-decode-entities-nbsp ()
  "Decode all HTML entities and &nbsp; to a space."
  (let ((mm-url-html-entities (cons '(nbsp . 32) mm-url-html-entities)))
    (mm-url-decode-entities)))

(defun mm-url-decode-entities-string (string)
  (with-temp-buffer
    (insert string)
    (mm-url-decode-entities)
    (buffer-substring (point-min) (point-max))))

(defun mm-url-form-encode-xwfu (chunk)
  "Escape characters in a string for application/x-www-form-urlencoded.
Blasphemous crap because someone didn't think %20 was good enough for encoding
spaces.  Die Die Die."
  ;; This will get rid of the 'attributes' specified by the file type,
  ;; which are useless for an application/x-www-form-urlencoded form.
  (if (consp chunk)
      (setq chunk (cdr chunk)))

  (mapconcat
   (lambda (char)
     (cond
      ((= char ?  ) "+")
      ((memq char mm-url-unreserved-chars) (char-to-string char))
      (t (upcase (format "%%%02x" char)))))
   ;; Fixme: Should this actually be accepting multibyte?  Is there a
   ;; better way in XEmacs?
   (if (featurep 'mule)
       (encode-coding-string chunk
			     (if (fboundp 'find-coding-systems-string)
				 (car (find-coding-systems-string chunk))
				 buffer-file-coding-system))
     chunk)
   ""))

(defun mm-url-encode-www-form-urlencoded (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat
   (lambda (data)
     (concat (mm-url-form-encode-xwfu (car data)) "="
	     (mm-url-form-encode-xwfu (cdr data))))
   pairs "&"))

(defun mm-url-fetch-form (url pairs)
  "Fetch a form from URL with PAIRS as the data using the POST method."
  (require 'url-handlers)
  (let ((url-request-data (mm-url-encode-www-form-urlencoded pairs))
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents url)
    (setq buffer-file-name nil))
  t)

(defun mm-url-fetch-simple (url content)
  (require 'url-handlers)
  (let ((url-request-data content)
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents url)
    (setq buffer-file-name nil))
  t)

(defun mm-url-remove-markup ()
  "Remove all HTML markup, leaving just plain text."
  (goto-char (point-min))
  (while (search-forward "<!--" nil t)
    (delete-region (match-beginning 0)
		   (or (search-forward "-->" nil t)
		       (point-max))))
  (goto-char (point-min))
  (while (re-search-forward "<[^>]+>" nil t)
    (replace-match "" t t)))

(provide 'mm-url)

;;; mm-url.el ends here
;;; gnus-art.el --- article mode commands for Semi-gnus
;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;	Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: mail, news, MIME

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'static))

(require 'path-util)
(require 'gnus)
(require 'gnus-sum)
(require 'gnus-spec)
(require 'gnus-int)
(require 'gnus-win)
(require 'alist)
(require 'mime-view)
(require 'wid-edit)

;; Avoid byte-compile warnings.
(eval-when-compile
  (require 'mm-bodies)
  (require 'mail-parse)
  (require 'mm-decode)
  (require 'mm-view)
  (require 'mm-uu)
  )

(autoload 'gnus-msg-mail "gnus-msg" nil t)
(autoload 'gnus-button-mailto "gnus-msg")
(autoload 'gnus-button-reply "gnus-msg" nil t)

(defgroup gnus-article nil
  "Article display."
  :link '(custom-manual "(gnus)The Article Buffer")
  :group 'gnus)

(defgroup gnus-article-treat nil
  "Treating article parts."
  :link '(custom-manual "(gnus)Article Hiding")
  :group 'gnus-article)

(defgroup gnus-article-hiding nil
  "Hiding article parts."
  :link '(custom-manual "(gnus)Article Hiding")
  :group 'gnus-article)

(defgroup gnus-article-highlight nil
  "Article highlighting."
  :link '(custom-manual "(gnus)Article Highlighting")
  :group 'gnus-article
  :group 'gnus-visual)

(defgroup gnus-article-signature nil
  "Article signatures."
  :link '(custom-manual "(gnus)Article Signature")
  :group 'gnus-article)

(defgroup gnus-article-headers nil
  "Article headers."
  :link '(custom-manual "(gnus)Hiding Headers")
  :group 'gnus-article)

(defgroup gnus-article-washing nil
  "Special commands on articles."
  :link '(custom-manual "(gnus)Article Washing")
  :group 'gnus-article)

(defgroup gnus-article-emphasis nil
  "Fontisizing articles."
  :link '(custom-manual "(gnus)Article Fontisizing")
  :group 'gnus-article)

(defgroup gnus-article-saving nil
  "Saving articles."
  :link '(custom-manual "(gnus)Saving Articles")
  :group 'gnus-article)

(defgroup gnus-article-mime nil
  "Worshiping the MIME wonder."
  :link '(custom-manual "(gnus)Using MIME")
  :group 'gnus-article)

(defgroup gnus-article-buttons nil
  "Pushable buttons in the article buffer."
  :link '(custom-manual "(gnus)Article Buttons")
  :group 'gnus-article)

(defgroup gnus-article-various nil
  "Other article options."
  :link '(custom-manual "(gnus)Misc Article")
  :group 'gnus-article)

(defcustom gnus-ignored-headers
  '("^Path:" "^Expires:" "^Date-Received:" "^References:" "^Xref:" "^Lines:"
    "^Relay-Version:" "^Message-ID:" "^Approved:" "^Sender:" "^Received:"
    "^X-UIDL:" "^MIME-Version:" "^Return-Path:" "^In-Reply-To:"
    "^Content-Type:" "^Content-Transfer-Encoding:" "^X-WebTV-Signature:"
    "^X-MimeOLE:" "^X-MSMail-Priority:" "^X-Priority:" "^X-Loop:"
    "^X-Authentication-Warning:" "^X-MIME-Autoconverted:" "^X-Face"
    "^X-Attribution:" "^X-Originating-IP:" "^Delivered-To:"
    "^NNTP-[-A-Za-z]+:" "^Distribution:" "^X-no-archive:" "^X-Trace:"
    "^X-Complaints-To:" "^X-NNTP-Posting-Host:" "^X-Orig.*:"
    "^Abuse-Reports-To:" "^Cache-Post-Path:" "^X-Article-Creation-Date:"
    "^X-Poster:" "^X-Mail2News-Path:" "^X-Server-Date:" "^X-Cache:"
    "^Originator:" "^X-Problems-To:" "^X-Auth-User:" "^X-Post-Time:"
    "^X-Admin:" "^X-UID:" "^Resent-[-A-Za-z]+:" "^X-Mailing-List:"
    "^Precedence:" "^Original-[-A-Za-z]+:" "^X-filename:" "^X-Orcpt:"
    "^Old-Received:" "^X-Pgp" "^X-Auth:" "^X-From-Line:"
    "^X-Gnus-Article-Number:" "^X-Majordomo:" "^X-Url:" "^X-Sender:"
    "^MBOX-Line" "^Priority:" "^X400-[-A-Za-z]+:"
    "^Status:" "^X-Gnus-Mail-Source:" "^Cancel-Lock:"
    "^X-FTN" "^X-EXP32-SerialNo:" "^Encoding:" "^Importance:"
    "^Autoforwarded:" "^Original-Encoded-Information-Types:" "^X-Ya-Pop3:"
    "^X-Face-Version:" "^X-Vms-To:" "^X-ML-NAME:" "^X-ML-COUNT:"
    "^Mailing-List:" "^X-finfo:" "^X-md5sum:" "^X-md5sum-Origin:"
    "^X-Sun-Charset:" "^X-Accept-Language:" "^X-Envelope-Sender:"
    "^List-[A-Za-z]+:" "^X-Listprocessor-Version:"
    "^X-Received:" "^X-Distribute:" "^X-Sequence:" "^X-Juno-Line-Breaks:"
    "^X-Notes-Item:" "^X-MS-TNEF-Correlator:" "^x-uunet-gateway:"
    "^X-Received:" "^Content-length:" "X-precedence:"
    "^X-Authenticated-User:" "^X-Comment" "^X-Report:" "^X-Abuse-Info:"
    "^X-HTTP-Proxy:" "^X-Mydeja-Info:" "^X-Copyright" "^X-No-Markup:"
    "^X-Abuse-Info:" "^X-From_:" "^X-Accept-Language:" "^Errors-To:"
    "^X-BeenThere:" "^X-Mailman-Version:" "^List-Help:" "^List-Post:"
    "^List-Subscribe:" "^List-Id:" "^List-Unsubscribe:" "^List-Archive:"
     "^X-Content-length:" "^X-Posting-Agent:" "^Original-Received:"
     "^X-Request-PGP:" "^X-Fingerprint:" "^X-WRIEnvto:" "^X-WRIEnvfrom:"
     "^X-Virus-Scanned:" "^X-Delivery-Agent:" "^Posted-Date:" "^X-Gateway:"
     "^X-Local-Origin:" "^X-Local-Destination:" "^X-UserInfo1:"
     "^X-Received-Date:")
  "*All headers that start with this regexp will be hidden.
This variable can also be a list of regexps of headers to be ignored.
If `gnus-visible-headers' is non-nil, this variable will be ignored."
  :type '(choice :custom-show nil
		 regexp
		 (repeat regexp))
  :group 'gnus-article-hiding)

(defcustom gnus-visible-headers
  "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:"
  "*All headers that do not match this regexp will be hidden.
This variable can also be a list of regexp of headers to remain visible.
If this variable is non-nil, `gnus-ignored-headers' will be ignored."
  :type '(repeat :value-to-internal (lambda (widget value)
				      (custom-split-regexp-maybe value))
		 :match (lambda (widget value)
			  (or (stringp value)
			      (widget-editable-list-match widget value)))
		 regexp)
  :group 'gnus-article-hiding)

(defcustom gnus-sorted-header-list
  '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:"
    "^Followup-To:" "^To:" "^Cc:" "^Date:" "^Organization:")
  "*This variable is a list of regular expressions.
If it is non-nil, headers that match the regular expressions will
be placed first in the article buffer in the sequence specified by
this list."
  :type '(repeat regexp)
  :group 'gnus-article-hiding)

(defcustom gnus-boring-article-headers '(empty followup-to reply-to)
  "Headers that are only to be displayed if they have interesting data.
Possible values in this list are:

  'empty       Headers with no content.
  'newsgroups  Newsgroup identical to Gnus group.
  'to-address  To identical to To-address.
  'followup-to Followup-to identical to Newsgroups.
  'reply-to    Reply-to identical to From.
  'date        Date less than four days old.
  'long-to     To and/or Cc longer than 1024 characters.
  'many-to     Multiple To and/or Cc."
  :type '(set (const :tag "Headers with no content." empty)
	      (const :tag "Newsgroups identical to Gnus group." newsgroups)
	      (const :tag "To identical to To-address." to-address)
	      (const :tag "Followup-to identical to Newsgroups." followup-to)
	      (const :tag "Reply-to identical to From." reply-to)
	      (const :tag "Date less than four days old." date)
	      (const :tag "To and/or Cc longer than 1024 characters." long-to)
	      (const :tag "Multiple To and/or Cc headers." many-to))
  :group 'gnus-article-hiding)

(defcustom gnus-signature-separator '("^-- $" "^-- *$")
  "Regexp matching signature separator.
This can also be a list of regexps.  In that case, it will be checked
from head to tail looking for a separator.  Searches will be done from
the end of the buffer."
  :type '(repeat string)
  :group 'gnus-article-signature)

(defcustom gnus-signature-limit nil
  "Provide a limit to what is considered a signature.
If it is a number, no signature may not be longer (in characters) than
that number.  If it is a floating point number, no signature may be
longer (in lines) than that number.  If it is a function, the function
will be called without any parameters, and if it returns nil, there is
no signature in the buffer.  If it is a string, it will be used as a
regexp.  If it matches, the text in question is not a signature."
  :type '(choice (integer :value 200)
		 (number :value 4.0)
		 (function :value fun)
		 (regexp :value ".*"))
  :group 'gnus-article-signature)

(defcustom gnus-hidden-properties '(invisible t intangible t)
  "Property list to use for hiding text."
  :type 'sexp
  :group 'gnus-article-hiding)

;; Fixme: This isn't the right thing for mixed graphical and and
;; non-graphical frames in a session.
(defcustom gnus-article-x-face-command
  (cond
   ((featurep 'xemacs)
    (if (or (gnus-image-type-available-p 'xface)
	    (gnus-image-type-available-p 'pbm))
	'gnus-display-x-face-in-from
      "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | ee -"))
   ((and (fboundp 'image-type-available-p)
	 (module-installed-p 'x-face-e21))
    'x-face-decode-message-header)
   ((gnus-image-type-available-p 'pbm)
    'gnus-display-x-face-in-from)
   ((and window-system
	 (module-installed-p 'x-face-mule))
    'x-face-mule-gnus-article-display-x-face)
   (t
    "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | \
display -"))
  "*String or function to be executed to display an X-Face header.
If it is a string, the command will be executed in a sub-shell
asynchronously.	 The compressed face will be piped to this command."
  :type `(choice
	  ,@(let (x-face-e21 x-face-mule)
	      (if (featurep 'xemacs)
		  nil
		(setq x-face-e21 (module-installed-p 'x-face-e21)
		      x-face-mule (module-installed-p 'x-face-mule)))
	      (delq nil
		    (list
		     'string
		     (if (or (gnus-image-type-available-p 'xface)
			     (gnus-image-type-available-p 'pbm))
			 '(function-item gnus-display-x-face-in-from))
		     (if (and x-face-e21
			      (fboundp 'image-type-available-p))
			 '(function-item
			   :tag "x-face-decode-message-header (x-face-e21)"
			   x-face-decode-message-header))
		     (if x-face-mule
			 '(function-item
			   x-face-mule-gnus-article-display-x-face))
		     'function))))
  ;;:version "21.1"
  :group 'gnus-article-washing)

(defcustom gnus-article-x-face-too-ugly nil
  "Regexp matching posters whose face shouldn't be shown automatically."
  :type '(choice regexp (const nil))
  :group 'gnus-article-washing)

(defcustom gnus-article-banner-alist nil
  "Banner alist for stripping.
For example,
     ((egroups . \"^[ \\t\\n]*-------------------+\\\\( \\\\(e\\\\|Yahoo! \\\\)Groups Sponsor -+\\\\)?....\\n\\\\(.+\\n\\\\)+\"))"
  :version "21.1"
  :type '(repeat (cons symbol regexp))
  :group 'gnus-article-washing)

(gnus-define-group-parameter
 banner
 :variable-document
 "Alist of regexps (to match group names) and banner."
 :variable-group gnus-article-washing
 :parameter-type
 '(choice :tag "Banner"
	  :value nil
	  (const :tag "Remove signature" signature)
	  (symbol :tag "Item in `gnus-article-banner-alist'" none)
	  regexp
	  (const :tag "None" nil))
 :parameter-document
 "If non-nil, specify how to remove `banners' from articles.

Symbol `signature' means to remove signatures delimited by
`gnus-signature-separator'.  Any other symbol is used to look up a
regular expression to match the banner in `gnus-article-banner-alist'.
A string is used as a regular expression to match the banner
directly.")

(defcustom gnus-emphasis-alist
  (let ((format
	 "\\(\\s-\\|^\\|\\=\\|[-\"]\\|\\s(\\)\\(%s\\(\\w+\\(\\s-+\\w+\\)*[.,]?\\)%s\\)\\(\\([-,.;:!?\"]\\|\\s)\\)+\\s-\\|[?!.]\\s-\\|\\s)\\|\\s-\\)")
	(types
	 '(("\\*" "\\*" bold)
	   ("_" "_" underline)
	   ("/" "/" italic)
	   ("_/" "/_" underline-italic)
	   ("_\\*" "\\*_" underline-bold)
	   ("\\*/" "/\\*" bold-italic)
	   ("_\\*/" "/\\*_" underline-bold-italic))))
    `(,@(mapcar
	 (lambda (spec)
	   (list
	    (format format (car spec) (car (cdr spec)))
	    2 3 (intern (format "gnus-emphasis-%s" (nth 2 spec)))))
	 types)
	("\\(\\s-\\|^\\)\\(_\\(\\(\\w\\|_[^_]\\)+\\)_\\)\\(\\s-\\|[?!.,;]\\)"
	 2 3 gnus-emphasis-underline)))
  "*Alist that says how to fontify certain phrases.
Each item looks like this:

  (\"_\\\\(\\\\w+\\\\)_\" 0 1 'underline)

The first element is a regular expression to be matched.  The second
is a number that says what regular expression grouping used to find
the entire emphasized word.  The third is a number that says what
regexp grouping should be displayed and highlighted.  The fourth
is the face used for highlighting."
  :type '(repeat (list :value ("" 0 0 default)
		       regexp
		       (integer :tag "Match group")
		       (integer :tag "Emphasize group")
		       face))
  :group 'gnus-article-emphasis)

(defcustom gnus-emphasize-whitespace-regexp "^[ \t]+\\|[ \t]*\n"
  "A regexp to describe whitespace which should not be emphasized.
Typical values are \"^[ \\t]+\\\\|[ \\t]*\\n\" and \"[ \\t]+\\\\|[ \\t]*\\n\".
The former avoids underlining of leading and trailing whitespace,
and the latter avoids underlining any whitespace at all."
  :version "21.1"
  :group 'gnus-article-emphasis
  :type 'regexp)

(defface gnus-emphasis-bold '((t (:bold t)))
  "Face used for displaying strong emphasized text (*word*)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-italic '((t (:italic t)))
  "Face used for displaying italic emphasized text (/word/)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-underline '((t (:underline t)))
  "Face used for displaying underlined emphasized text (_word_)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-underline-bold '((t (:bold t :underline t)))
  "Face used for displaying underlined bold emphasized text (_*word*_)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-underline-italic '((t (:italic t :underline t)))
  "Face used for displaying underlined italic emphasized text (_/word/_)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-bold-italic '((t (:bold t :italic t)))
  "Face used for displaying bold italic emphasized text (/*word*/)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-underline-bold-italic
  '((t (:bold t :italic t :underline t)))
  "Face used for displaying underlined bold italic emphasized text.
Esample: (_/*word*/_)."
  :group 'gnus-article-emphasis)

(defface gnus-emphasis-highlight-words
  '((t (:background "black" :foreground "yellow")))
  "Face used for displaying highlighted words."
  :group 'gnus-article-emphasis)

(defcustom gnus-article-time-format "%a, %b %d %Y %T %Z"
  "Format for display of Date headers in article bodies.
See `format-time-string' for the possible values.

The variable can also be function, which should return a complete Date
header.  The function is called with one argument, the time, which can
be fed to `format-time-string'."
  :type '(choice string symbol)
  :link '(custom-manual "(gnus)Article Date")
  :group 'gnus-article-washing)

(eval-and-compile
  (autoload 'mail-extract-address-components "mail-extr"))

(defcustom gnus-save-all-headers t
  "*If non-nil, don't remove any headers before saving."
  :group 'gnus-article-saving
  :type 'boolean)

(defcustom gnus-prompt-before-saving 'always
  "*This variable says how much prompting is to be done when saving articles.
If it is nil, no prompting will be done, and the articles will be
saved to the default files.  If this variable is `always', each and
every article that is saved will be preceded by a prompt, even when
saving large batches of articles.  If this variable is neither nil not
`always', there the user will be prompted once for a file name for
each invocation of the saving commands."
  :group 'gnus-article-saving
  :type '(choice (item always)
		 (item :tag "never" nil)
		 (sexp :tag "once" :format "%t\n" :value t)))

(defcustom gnus-saved-headers gnus-visible-headers
  "Headers to keep if `gnus-save-all-headers' is nil.
If `gnus-save-all-headers' is non-nil, this variable will be ignored.
If that variable is nil, however, all headers that match this regexp
will be kept while the rest will be deleted before saving."
  :group 'gnus-article-saving
  :type 'regexp)

(defcustom gnus-default-article-saver 'gnus-summary-save-in-rmail
  "A function to save articles in your favourite format.
The function must be interactively callable (in other words, it must
be an Emacs command).

Gnus provides the following functions:

* gnus-summary-save-in-rmail (Rmail format)
* gnus-summary-save-in-mail (Unix mail format)
* gnus-summary-save-in-folder (MH folder)
* gnus-summary-save-in-file (article format)
* gnus-summary-save-body-in-file (article body)
* gnus-summary-save-in-vm (use VM's folder format)
* gnus-summary-write-to-file (article format -- overwrite)."
  :group 'gnus-article-saving
  :type '(radio (function-item gnus-summary-save-in-rmail)
		(function-item gnus-summary-save-in-mail)
		(function-item gnus-summary-save-in-folder)
		(function-item gnus-summary-save-in-file)
		(function-item gnus-summary-save-body-in-file)
		(function-item gnus-summary-save-in-vm)
		(function-item gnus-summary-write-to-file)))

(defcustom gnus-rmail-save-name 'gnus-plain-save-name
  "A function generating a file name to save articles in Rmail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE."
  :group 'gnus-article-saving
  :type 'function)

(defcustom gnus-mail-save-name 'gnus-plain-save-name
  "A function generating a file name to save articles in Unix mail format.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FILE."
  :group 'gnus-article-saving
  :type 'function)

(defcustom gnus-folder-save-name 'gnus-folder-save-name
  "A function generating a file name to save articles in MH folder.
The function is called with NEWSGROUP, HEADERS, and optional LAST-FOLDER."
  :group 'gnus-article-saving
  :type 'function)

(defcustom gnus-file-save-name 'gnus-numeric-save-name
  "A function generating a file name to save articles in article format.
The function is called with NEWSGROUP, HEADERS, and optional
LAST-FILE."
  :group 'gnus-article-saving
  :type 'function)

(defcustom gnus-split-methods
  '((gnus-article-archive-name)
    (gnus-article-nndoc-name))
  "*Variable used to suggest where articles are to be saved.
For instance, if you would like to save articles related to Gnus in
the file \"gnus-stuff\", and articles related to VM in \"vm-stuff\",
you could set this variable to something like:

 '((\"^Subject:.*gnus\\|^Newsgroups:.*gnus\" \"gnus-stuff\")
   (\"^Subject:.*vm\\|^Xref:.*vm\" \"vm-stuff\"))

This variable is an alist where the where the key is the match and the
value is a list of possible files to save in if the match is non-nil.

If the match is a string, it is used as a regexp match on the
article.  If the match is a symbol, that symbol will be funcalled
from the buffer of the article to be saved with the newsgroup as the
parameter.  If it is a list, it will be evaled in the same buffer.

If this form or function returns a string, this string will be used as
a possible file name; and if it returns a non-nil list, that list will
be used as possible file names."
  :group 'gnus-article-saving
  :type '(repeat (choice (list :value (fun) function)
			 (cons :value ("" "") regexp (repeat string))
			 (sexp :value nil))))

(defcustom gnus-article-display-method-for-mime
  'gnus-article-display-mime-message
  "Function to display a MIME message.
The function is called from the article buffer."
  :group 'gnus-article-mime
  :type 'function)

(defcustom gnus-article-display-method-for-traditional
  'gnus-article-display-traditional-message
  "*Function to display a traditional message.
The function is called from the article buffer."
  :group 'gnus-article-mime
  :type 'function)

(defcustom gnus-page-delimiter "^\^L"
  "*Regexp describing what to use as article page delimiters.
The default value is \"^\^L\", which is a form linefeed at the
beginning of a line."
  :type 'regexp
  :group 'gnus-article-various)

(defcustom gnus-article-mode-line-format "Gnus: %g [%w] %S%m"
  "*The format specification for the article mode line.
See `gnus-summary-mode-line-format' for a closer description.

The following additional specs are available:

%w  The article washing status.
%m  The number of MIME parts in the article."
  :type 'string
  :group 'gnus-article-various)

(defcustom gnus-article-mode-hook nil
  "*A hook for Gnus article mode."
  :type 'hook
  :group 'gnus-article-various)

(when (featurep 'xemacs)
  ;; Extracted from gnus-xmas-define in order to preserve user settings
  (when (fboundp 'turn-off-scroll-in-place)
    (add-hook 'gnus-article-mode-hook 'turn-off-scroll-in-place))
  ;; Extracted from gnus-xmas-redefine in order to preserve user settings
  (add-hook 'gnus-article-mode-hook 'gnus-xmas-article-menu-add))

(defcustom gnus-article-menu-hook nil
  "*Hook run after the creation of the article mode menu."
  :type 'hook
  :group 'gnus-article-various)

(defcustom gnus-article-prepare-hook nil
  "*A hook called after an article has been prepared in the article buffer."
  :type 'hook
  :group 'gnus-article-various)

(defcustom gnus-article-hide-pgp-hook nil
  "*A hook called after successfully hiding a PGP signature."
  :type 'hook
  :group 'gnus-article-various)

(defcustom gnus-article-button-face 'bold
  "Face used for highlighting buttons in the article buffer.

An article button is a piece of text that you can activate by pressing
`RET' or `mouse-2' above it."
  :type 'face
  :group 'gnus-article-buttons)

(defcustom gnus-article-mouse-face 'highlight
  "Face used for mouse highlighting in the article buffer.

Article buttons will be displayed in this face when the cursor is
above them."
  :type 'face
  :group 'gnus-article-buttons)

(defcustom gnus-signature-face 'gnus-signature-face
  "Face used for highlighting a signature in the article buffer.
Obsolete; use the face `gnus-signature-face' for customizations instead."
  :type 'face
  :group 'gnus-article-highlight
  :group 'gnus-article-signature)

(defface gnus-signature-face
  '((t
     (:italic t)))
  "Face used for highlighting a signature in the article buffer."
  :group 'gnus-article-highlight
  :group 'gnus-article-signature)

(defface gnus-header-from-face
  '((((class color)
      (background dark))
     (:foreground "spring green"))
    (((class color)
      (background light))
     (:foreground "red3"))
    (t
     (:italic t)))
  "Face used for displaying from headers."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)

(defface gnus-header-subject-face
  '((((class color)
      (background dark))
     (:foreground "SeaGreen3"))
    (((class color)
      (background light))
     (:foreground "red4"))
    (t
     (:bold t :italic t)))
  "Face used for displaying subject headers."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)

(defface gnus-header-newsgroups-face
  '((((class color)
      (background dark))
     (:foreground "yellow" :italic t))
    (((class color)
      (background light))
     (:foreground "MidnightBlue" :italic t))
    (t
     (:italic t)))
  "Face used for displaying newsgroups headers."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)

(defface gnus-header-name-face
  '((((class color)
      (background dark))
     (:foreground "SeaGreen"))
    (((class color)
      (background light))
     (:foreground "maroon"))
    (t
     (:bold t)))
  "Face used for displaying header names."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)

(defface gnus-header-content-face
  '((((class color)
      (background dark))
     (:foreground "forest green" :italic t))
    (((class color)
      (background light))
     (:foreground "indianred4" :italic t))
    (t
     (:italic t)))
  "Face used for displaying header content."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight)

(defcustom gnus-header-face-alist
  '(("From" nil gnus-header-from-face)
    ("Subject" nil gnus-header-subject-face)
    ("Newsgroups:.*," nil gnus-header-newsgroups-face)
    ("" gnus-header-name-face gnus-header-content-face))
  "*Controls highlighting of article header.

An alist of the form (HEADER NAME CONTENT).

HEADER is a regular expression which should match the name of an
header header and NAME and CONTENT are either face names or nil.

The name of each header field will be displayed using the face
specified by the first element in the list where HEADER match the
header name and NAME is non-nil.  Similarly, the content will be
displayed by the first non-nil matching CONTENT face."
  :group 'gnus-article-headers
  :group 'gnus-article-highlight
  :type '(repeat (list (regexp :tag "Header")
		       (choice :tag "Name"
			       (item :tag "skip" nil)
			       (face :value default))
		       (choice :tag "Content"
			       (item :tag "skip" nil)
			       (face :value default)))))

(defcustom gnus-article-decode-hook nil
  "*Hook run to decode charsets in articles."
  :group 'gnus-article-headers
  :type 'hook)

(defcustom gnus-display-mime-function 'gnus-display-mime
  "Function to display MIME articles."
  :group 'gnus-article-mime
  :type 'function)

(defvar gnus-decode-header-function 'mail-decode-encoded-word-region
  "Function used to decode headers.")

(defvar gnus-article-dumbquotes-map
  '(("\200" "EUR")
    ("\202" ",")
    ("\203" "f")
    ("\204" ",,")
    ("\205" "...")
    ("\213" "<")
    ("\214" "OE")
    ("\221" "`")
    ("\222" "'")
    ("\223" "``")
    ("\224" "\"")
    ("\225" "*")
    ("\226" "-")
    ("\227" "--")
    ("\231" "(TM)")
    ("\233" ">")
    ("\234" "oe")
    ("\264" "'"))
  "Table for MS-to-Latin1 translation.")

(defcustom gnus-ignored-mime-types nil
  "List of MIME types that should be ignored by Gnus."
  :version "21.1"
  :group 'gnus-article-mime
  :type '(repeat regexp))

(defcustom gnus-unbuttonized-mime-types '(".*/.*")
  "List of MIME types that should not be given buttons when rendered inline.
See also `gnus-buttonized-mime-types' which may override this variable."
  :version "21.1"
  :group 'gnus-article-mime
  :type '(repeat regexp))

(defcustom gnus-buttonized-mime-types nil
  "List of MIME types that should be given buttons when rendered inline.
If set, this variable overrides `gnus-unbuttonized-mime-types'.
To see e.g. security buttons you could set this to
`(\"multipart/signed\")'."
  :version "21.1"
  :group 'gnus-article-mime
  :type '(repeat regexp))

(defcustom gnus-body-boundary-delimiter "_"
  "String used to delimit header and body.
This variable is used by `gnus-article-treat-body-boundary' which can
be controlled by `gnus-treat-body-boundary'."
  :group 'gnus-article-various
  :type '(choice (item :tag "None" :value nil)
		 string))

(defcustom gnus-picon-databases '("/usr/lib/picon" "/usr/local/faces")
  "*Defines the location of the faces database.
For information on obtaining this database of pretty pictures, please
see http://www.cs.indiana.edu/picons/ftp/index.html"
  :type 'directory
  :group 'gnus-picon)

(defun gnus-picons-installed-p ()
  "Say whether picons are installed on your machine."
  (let ((installed nil))
    (dolist (database gnus-picon-databases)
      (when (file-exists-p database)
	(setq installed t)))
    installed))

(defcustom gnus-article-mime-part-function nil
  "Function called with a MIME handle as the argument.
This is meant for people who want to do something automatic based
on parts -- for instance, adding Vcard info to a database."
  :group 'gnus-article-mime
  :type 'function)

(defcustom gnus-mime-multipart-functions nil
  "An alist of MIME types to functions to display them."
  :version "21.1"
  :group 'gnus-article-mime
  :type 'alist)

(defcustom gnus-article-date-lapsed-new-header nil
  "Whether the X-Sent and Date headers can coexist.
When using `gnus-treat-date-lapsed', the \"X-Sent:\" header will
either replace the old \"Date:\" header (if this variable is nil), or
be added below it (otherwise)."
  :version "21.1"
  :group 'gnus-article-headers
  :type 'boolean)

(defcustom gnus-article-mime-match-handle-function 'undisplayed-alternative
  "Function called with a MIME handle as the argument.
This is meant for people who want to view first matched part.
For `undisplayed-alternative' (default), the first undisplayed
part or alternative part is used.  For `undisplayed', the first
undisplayed part is used.  For a function, the first part which
the function return `t' is used.  For `nil', the first part is
used."
  :version "21.1"
  :group 'gnus-article-mime
  :type '(choice
	  (item :tag "first" :value nil)
	  (item :tag "undisplayed" :value undisplayed)
	  (item :tag "undisplayed or alternative"
		:value undisplayed-alternative)
	  (function)))

(defcustom gnus-mime-action-alist
  '(("save to file" . gnus-mime-save-part)
    ("save and strip" . gnus-mime-save-part-and-strip)
    ("display as text" . gnus-mime-inline-part)
    ("view the part" . gnus-mime-view-part)
    ("pipe to command" . gnus-mime-pipe-part)
    ("toggle display" . gnus-article-press-button)
    ("toggle display" . gnus-article-view-part-as-charset)
    ("view as type" . gnus-mime-view-part-as-type)
    ("view internally" . gnus-mime-view-part-internally)
    ("view externally" . gnus-mime-view-part-externally))
  "An alist of actions that run on the MIME attachment."
  :group 'gnus-article-mime
  :type '(repeat (cons (string :tag "name")
		       (function))))

;;;
;;; The treatment variables
;;;

(defvar gnus-part-display-hook nil
  "Hook called on parts that are to receive treatment.")

(defvar gnus-article-treat-custom
  '(choice (const :tag "Off" nil)
	   (const :tag "On" t)
	   (const :tag "Header" head)
	   (const :tag "Last" last)
	   (const :tag "Mime" mime)
	   (integer :tag "Less")
	   (repeat :tag "Groups" regexp)
	   (sexp :tag "Predicate")))

(defvar gnus-article-treat-head-custom
  '(choice (const :tag "Off" nil)
	   (const :tag "Header" head)))

(defvar gnus-article-treat-types '("text/plain")
  "Parts to treat.")

(defvar gnus-inhibit-treatment nil
  "Whether to inhibit treatment.")

(defcustom gnus-treat-highlight-signature '(or last (typep "text/x-vcard"))
  "Highlight the signature.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)
(put 'gnus-treat-highlight-signature 'highlight t)

(defcustom gnus-treat-buttonize 100000
  "Add buttons.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles'."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)
(put 'gnus-treat-buttonize 'highlight t)

(defcustom gnus-treat-buttonize-head 'head
  "Add buttons to the head.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-buttonize-head 'highlight t)

(defcustom gnus-treat-emphasize
  (and (or window-system
	   (featurep 'xemacs)
	   (>= (string-to-number emacs-version) 21))
       50000)
  "Emphasize text.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)
(put 'gnus-treat-emphasize 'highlight t)

(defcustom gnus-treat-strip-cr nil
  "Remove carriage returns.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-unsplit-urls nil
  "Remove newlines from within URLs.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-leading-whitespace nil
  "Remove leading whitespace in headers.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-hide-headers 'head
  "Hide headers.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)

(defcustom gnus-treat-hide-boring-headers nil
  "Hide boring headers.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)

(defcustom gnus-treat-hide-signature nil
  "Hide the signature.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-fill-article nil
  "Fill the article.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-hide-citation nil
  "Hide cited text.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-hide-citation-maybe nil
  "Hide cited text.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-strip-list-identifiers 'head
  "Strip list identifiers from `gnus-list-identifiers`.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :version "21.1"
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-strip-pgp t
  "Strip PGP signatures.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-strip-pem nil
  "Strip PEM signatures.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-strip-banner t
  "Strip banners from articles.
The banner to be stripped is specified in the `banner' group parameter.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-highlight-headers 'head
  "Highlight the headers.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-highlight-headers 'highlight t)

(defcustom gnus-treat-highlight-citation t
  "Highlight cited text.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)
(put 'gnus-treat-highlight-citation 'highlight t)

(defcustom gnus-treat-date-ut nil
  "Display the Date in UT (GMT).
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)

(defcustom gnus-treat-date-local nil
  "Display the Date in the local timezone.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)

(defcustom gnus-treat-date-english nil
  "Display the Date in a format that can be read aloud in English.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)

(defcustom gnus-treat-date-lapsed nil
  "Display the Date header in a way that says how much time has elapsed.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)

(defcustom gnus-treat-date-original nil
  "Display the date in the original timezone.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)

(defcustom gnus-treat-date-iso8601 nil
  "Display the date in the ISO8601 format.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :version "21.1"
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)

(defcustom gnus-treat-date-user-defined nil
  "Display the date in a user-defined format.
The format is defined by the `gnus-article-time-format' variable.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)

(defcustom gnus-treat-strip-headers-in-body t
  "Strip the X-No-Archive header line from the beginning of the body.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :version "21.1"
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-strip-trailing-blank-lines nil
  "Strip trailing blank lines.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-strip-leading-blank-lines nil
  "Strip leading blank lines.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-strip-multiple-blank-lines nil
  "Strip multiple blank lines.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-unfold-headers 'head
  "Unfold folded header lines.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-fold-headers nil
  "Fold headers.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-fold-newsgroups 'head
  "Fold the Newsgroups and Followup-To headers.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-overstrike t
  "Treat overstrike highlighting.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)
(put 'gnus-treat-overstrike 'highlight t)

(defcustom gnus-treat-display-xface
  (and (not noninteractive)
       (or (and (fboundp 'image-type-available-p)
		(image-type-available-p 'xbm)
		(string-match "^0x" (shell-command-to-string "uncompface")))
	   (and (featurep 'xemacs)
		(featurep 'xface))
	   (eq 'x-face-mule-gnus-article-display-x-face
	       gnus-article-x-face-command))
       'head)
  "Display X-Face headers.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' and Info node
`(gnus)X-Face' for details."
  :group 'gnus-article-treat
  ;;:version "21.1"
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-display-xface 'highlight t)

(defcustom gnus-article-should-use-smiley-mule
  (not (or (featurep 'xemacs)
	   (gnus-image-type-available-p 'xpm)
	   (gnus-image-type-available-p 'pbm)))
  "If non-nil, gnus uses `smiley-mule' for displaying smileys rather than
`smiley'.  It defaults to t when Emacs 20 or earlier is running.
`smiley-mule' is boundled in BITMAP-MULE package.  You can set it to t
even if you are using Emacs 21+.  It has no effect on XEmacs."
  :group 'gnus-article-various
  :type 'boolean
  :get (lambda (symbol)
	 (and (default-value symbol)
	      (not (featurep 'xemacs))
	      (module-installed-p 'smiley-mule)
	      t))
  :set (lambda (symbol value)
	 (set-default symbol (and value
				  (not (featurep 'xemacs))
				  (module-installed-p 'smiley-mule)
				  t))))

(defvar gnus-article-smiley-mule-loaded-p nil
  "Internal variable used to say whether `smiley-mule' is loaded (whether
smiley functions are not overridden by `smiley').")

(defcustom gnus-treat-display-grey-xface
  (and (not noninteractive)
       (or (featurep 'xemacs)
	   (and (fboundp 'display-images-p)
		(display-images-p)))
       (string-match "^0x" (shell-command-to-string "uncompface"))
       t)
  "Display grey X-Face headers.
Valid values are nil, t."
  :group 'gnus-article-treat
  :version "21.3"
  :type 'boolean)
(put 'gnus-treat-display-grey-xface 'highlight t)

(defcustom gnus-treat-display-smileys
  (if (or (and (featurep 'xemacs)
	       (featurep 'xpm))
	  (gnus-image-type-available-p 'xpm)
	  (gnus-image-type-available-p 'pbm)
	  (and (not (featurep 'xemacs))
	       window-system
	       (module-installed-p 'smiley-mule)))
      t
    nil)
  "Display smileys.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' and Info node
`(gnus)Smileys' for details."
  :group 'gnus-article-treat
  ;;:version "21.1"
  :type gnus-article-treat-custom)
(put 'gnus-treat-display-smileys 'highlight t)

(defcustom gnus-treat-from-picon
  (if (and (gnus-image-type-available-p 'xpm)
	   (gnus-picons-installed-p))
      'head nil)
  "Display picons in the From header.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' and Info node
`(gnus)Picons' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-from-picon 'highlight t)

(defcustom gnus-treat-mail-picon
  (if (and (gnus-image-type-available-p 'xpm)
	   (gnus-picons-installed-p))
      'head nil)
  "Display picons in To and Cc headers.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' and Info node
`(gnus)Picons' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-mail-picon 'highlight t)

(defcustom gnus-treat-newsgroups-picon
  (if (and (gnus-image-type-available-p 'xpm)
	   (gnus-picons-installed-p))
      'head nil)
  "Display picons in the Newsgroups and Followup-To headers.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' and Info node
`(gnus)Picons' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-head-custom)
(put 'gnus-treat-newsgroups-picon 'highlight t)

(defcustom gnus-treat-body-boundary
  (if (or gnus-treat-newsgroups-picon
	  gnus-treat-mail-picon
	  gnus-treat-from-picon)
      'head nil)
  "Draw a boundary at the end of the headers.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :version "21.1"
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-capitalize-sentences nil
  "Capitalize sentence-starting words.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :version "21.1"
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-fill-long-lines nil
  "Fill long lines.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-play-sounds nil
  "Play sounds.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :version "21.1"
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-decode-article-as-default-mime-charset nil
  "Decode an article as `default-mime-charset'.  For instance, if you want to
attempt to decode an article even if the value of `gnus-show-mime' is nil,
you could set this variable to something like: nil for don't decode, t for
decode the body, '(or header t) for the whole article, etc."
  :group 'gnus-article-treat
  :type '(radio (const :tag "Off" nil)
		(const :tag "Decode body" t)
		(const :tag "Decode all" (or head t))))

(defcustom gnus-treat-translate nil
  "Translate articles from one language to another.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :version "21.1"
  :group 'gnus-article-treat
  :type gnus-article-treat-custom)

(defcustom gnus-treat-x-pgp-sig nil
  "Verify X-PGP-Sig.
To automatically treat X-PGP-Sig, set it to head.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :group 'mime-security
  :type gnus-article-treat-custom)

(defcustom gnus-treat-monafy nil
  "Display body part with mona font.
Valid values are nil, t, `head', `last', an integer or a predicate.
See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :group 'mime-security
  :type gnus-article-treat-custom)

(defvar gnus-article-encrypt-protocol-alist
  '(("PGP" . mml2015-self-encrypt)))

;; Set to nil if more than one protocol added to
;; gnus-article-encrypt-protocol-alist.
(defcustom gnus-article-encrypt-protocol "PGP"
  "The protocol used for encrypt articles.
It is a string, such as \"PGP\". If nil, ask user."
  :type 'string
  :group 'mime-security)

(defvar gnus-article-wash-function nil
  "Function used for converting HTML into text.")

;;; Internal variables

(defvar gnus-english-month-names
  '("January" "February" "March" "April" "May" "June" "July" "August"
    "September" "October" "November" "December"))

(defvar article-goto-body-goes-to-point-min-p nil)
(defvar gnus-article-wash-types nil)
(defvar gnus-article-emphasis-alist nil)
(defvar gnus-article-image-alist nil)

(defvar gnus-article-mime-handle-alist-1 nil)
(defvar gnus-treatment-function-alist
  '((gnus-treat-decode-article-as-default-mime-charset
     gnus-article-decode-article-as-default-mime-charset)
    (gnus-treat-x-pgp-sig gnus-article-verify-x-pgp-sig)
    (gnus-treat-monafy gnus-article-monafy)
    (gnus-treat-strip-banner gnus-article-strip-banner)
    (gnus-treat-strip-headers-in-body gnus-article-strip-headers-in-body)
    (gnus-treat-buttonize gnus-article-add-buttons)
    (gnus-treat-fill-article gnus-article-fill-cited-article)
    (gnus-treat-fill-long-lines gnus-article-fill-long-lines)
    (gnus-treat-strip-cr gnus-article-remove-cr)
    (gnus-treat-unsplit-urls gnus-article-unsplit-urls)
    (gnus-treat-date-ut gnus-article-date-ut)
    (gnus-treat-date-local gnus-article-date-local)
    (gnus-treat-date-english gnus-article-date-english)
    (gnus-treat-date-lapsed gnus-article-date-lapsed)
    (gnus-treat-date-original gnus-article-date-original)
    (gnus-treat-date-user-defined gnus-article-date-user)
    (gnus-treat-date-iso8601 gnus-article-date-iso8601)
    (gnus-treat-hide-headers gnus-article-maybe-hide-headers)
    (gnus-treat-hide-boring-headers gnus-article-hide-boring-headers)
    (gnus-treat-hide-signature gnus-article-hide-signature)
    (gnus-treat-strip-list-identifiers gnus-article-hide-list-identifiers)
    (gnus-treat-leading-whitespace gnus-article-remove-leading-whitespace)
    (gnus-treat-strip-pgp gnus-article-hide-pgp)
    (gnus-treat-strip-pem gnus-article-hide-pem)
    (gnus-treat-from-picon gnus-treat-from-picon)
    (gnus-treat-mail-picon gnus-treat-mail-picon)
    (gnus-treat-newsgroups-picon gnus-treat-newsgroups-picon)
    (gnus-treat-highlight-headers gnus-article-highlight-headers)
    (gnus-treat-highlight-signature gnus-article-highlight-signature)
    (gnus-treat-strip-trailing-blank-lines
     gnus-article-remove-trailing-blank-lines)
    (gnus-treat-strip-leading-blank-lines
     gnus-article-strip-leading-blank-lines)
    (gnus-treat-strip-multiple-blank-lines
     gnus-article-strip-multiple-blank-lines)
    (gnus-treat-overstrike gnus-article-treat-overstrike)
    (gnus-treat-unfold-headers gnus-article-treat-unfold-headers)
    (gnus-treat-fold-headers gnus-article-treat-fold-headers)
    ;; Displaying X-Face should be done after unfolding headers
    ;; to protect bitmap lines.
    (gnus-treat-display-xface gnus-article-display-x-face)
    (gnus-treat-fold-newsgroups gnus-article-treat-fold-newsgroups)
    (gnus-treat-buttonize-head gnus-article-add-buttons-to-head)
    (gnus-treat-display-smileys gnus-treat-smiley)
    (gnus-treat-capitalize-sentences gnus-article-capitalize-sentences)
    (gnus-treat-emphasize gnus-article-emphasize)
    (gnus-treat-hide-citation gnus-article-hide-citation)
    (gnus-treat-hide-citation-maybe gnus-article-hide-citation-maybe)
    (gnus-treat-highlight-citation gnus-article-highlight-citation)
    (gnus-treat-body-boundary gnus-article-treat-body-boundary)
    (gnus-treat-play-sounds gnus-earcon-display)))

(defvar gnus-article-mime-handle-alist nil)
(defvar article-lapsed-timer nil)
(defvar gnus-article-current-summary nil)

(defvar gnus-article-mode-syntax-table
  (let ((table (copy-syntax-table text-mode-syntax-table)))
    ;; This causes the citation match run O(2^n).
    ;; (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?> ")" table)
    (modify-syntax-entry ?< "(" table)
    table)
  "Syntax table used in article mode buffers.
Initialized from `text-mode-syntax-table.")

(defvar gnus-save-article-buffer nil)

(defvar gnus-article-mode-line-format-alist
  (nconc '((?w (gnus-article-wash-status) ?s)
	   (?m (gnus-article-mime-part-status) ?s))
	 gnus-summary-mode-line-format-alist))

(defvar gnus-number-of-articles-to-be-saved nil)

(defvar gnus-inhibit-hiding nil)

;;; Macros for dealing with the article buffer.

(defmacro gnus-with-article-headers (&rest forms)
  `(save-excursion
     (set-buffer gnus-article-buffer)
     (save-restriction
       (let ((buffer-read-only nil)
	     (inhibit-point-motion-hooks t)
	     (case-fold-search t))
	 (article-narrow-to-head)
	 ,@forms))))

(put 'gnus-with-article-headers 'lisp-indent-function 0)
(put 'gnus-with-article-headers 'edebug-form-spec '(body))

(defmacro gnus-with-article-buffer (&rest forms)
  `(save-excursion
     (set-buffer gnus-article-buffer)
     (let ((buffer-read-only nil))
       ,@forms)))

(put 'gnus-with-article-buffer 'lisp-indent-function 0)
(put 'gnus-with-article-buffer 'edebug-form-spec '(body))

(defun gnus-article-goto-header (header)
  "Go to HEADER, which is a regular expression."
  (re-search-forward (concat "^\\(" header "\\):") nil t))

(defsubst gnus-article-hide-text (b e props)
  "Set text PROPS on the B to E region, extending `intangible' 1 past B."
  (gnus-add-text-properties-when 'article-type nil b e props)
  (when (memq 'intangible props)
    (put-text-property
     (max (1- b) (point-min))
     b 'intangible (cddr (memq 'intangible props)))))

(defsubst gnus-article-unhide-text (b e)
  "Remove hidden text properties from region between B and E."
  (remove-text-properties b e gnus-hidden-properties)
  (when (memq 'intangible gnus-hidden-properties)
    (put-text-property (max (1- b) (point-min))
		       b 'intangible nil)))

(defun gnus-article-hide-text-type (b e type)
  "Hide text of TYPE between B and E."
  (gnus-add-wash-type type)
  (gnus-article-hide-text
   b e (cons 'article-type (cons type gnus-hidden-properties))))

(defun gnus-article-unhide-text-type (b e type)
  "Unhide text of TYPE between B and E."
  (gnus-delete-wash-type type)
  (remove-text-properties
   b e (cons 'article-type (cons type gnus-hidden-properties)))
  (when (memq 'intangible gnus-hidden-properties)
    (put-text-property (max (1- b) (point-min))
		       b 'intangible nil)))

(defun gnus-article-hide-text-of-type (type)
  "Hide text of TYPE in the current buffer."
  (save-excursion
    (let ((b (point-min))
	  (e (point-max)))
      (while (setq b (text-property-any b e 'article-type type))
	(add-text-properties b (incf b) gnus-hidden-properties)))))

(defun gnus-article-delete-text-of-type (type)
  "Delete text of TYPE in the current buffer."
  (save-excursion
    (let ((b (point-min)))
      (while (setq b (text-property-any b (point-max) 'article-type type))
	(delete-region
	 b (or (text-property-not-all b (point-max) 'article-type type)
	       (point-max)))))))

(defun gnus-article-delete-invisible-text ()
  "Delete all invisible text in the current buffer."
  (save-excursion
    (let ((b (point-min)))
      (while (setq b (text-property-any b (point-max) 'invisible t))
	(delete-region
	 b (or (text-property-not-all b (point-max) 'invisible t)
	       (point-max)))))))

(defun gnus-article-text-type-exists-p (type)
  "Say whether any text of type TYPE exists in the buffer."
  (text-property-any (point-min) (point-max) 'article-type type))

(defsubst gnus-article-header-rank ()
  "Give the rank of the string HEADER as given by `gnus-sorted-header-list'."
  (let ((list gnus-sorted-header-list)
	(i 0))
    (while list
      (when (looking-at (car list))
	(setq list nil))
      (setq list (cdr list))
      (incf i))
    i))

(defun article-hide-headers (&optional arg delete)
  "Hide unwanted headers and possibly sort them as well."
  (interactive (gnus-article-hidden-arg))
  ;; Lars said that this function might be inhibited.
  (if (gnus-article-check-hidden-text 'headers arg)
      (progn
	;; Show boring headers as well.
	(gnus-article-show-hidden-text 'boring-headers)
	(when (eq 1 (point-min))
	  (set-window-start (get-buffer-window (current-buffer)) 1)))
    (unless gnus-inhibit-hiding
      (save-excursion
	(save-restriction
	  (let ((inhibit-read-only t)
		(case-fold-search t)
		(max (1+ (length gnus-sorted-header-list)))
		(ignored (when (not gnus-visible-headers)
			   (cond ((stringp gnus-ignored-headers)
				  gnus-ignored-headers)
				 ((listp gnus-ignored-headers)
				  (mapconcat 'identity gnus-ignored-headers
					     "\\|")))))
		(visible
		 (cond ((stringp gnus-visible-headers)
			gnus-visible-headers)
		       ((and gnus-visible-headers
			     (listp gnus-visible-headers))
			(mapconcat 'identity gnus-visible-headers "\\|"))))
		(inhibit-point-motion-hooks t)
		beg)
	    ;; First we narrow to just the headers.
	    (article-narrow-to-head)
	    ;; Hide any "From " lines at the beginning of (mail) articles.
	    (while (looking-at "From ")
	      (forward-line 1))
	    (unless (bobp)
	      (if delete
		  (delete-region (point-min) (point))
		(gnus-article-hide-text (point-min) (point)
					(nconc (list 'article-type 'headers)
					       gnus-hidden-properties))))
	    ;; Then treat the rest of the header lines.
	    ;; Then we use the two regular expressions
	    ;; `gnus-ignored-headers' and `gnus-visible-headers' to
	    ;; select which header lines is to remain visible in the
	    ;; article buffer.
	    (while (re-search-forward "^[^ \t:]*:" nil t)
	      (beginning-of-line)
	      ;; Mark the rank of the header.
	      (put-text-property
	       (point) (1+ (point)) 'message-rank
	       (if (or (and visible (looking-at visible))
		       (and ignored
			    (not (looking-at ignored))))
		   (gnus-article-header-rank)
		 (+ 2 max)))
	      (forward-line 1))
	    (message-sort-headers-1)
	    (when (setq beg (text-property-any
			     (point-min) (point-max) 'message-rank (+ 2 max)))
	      ;; We delete or make invisible the unwanted headers.
	      (gnus-add-wash-type 'headers)
	      (if delete
		  (progn
		    (add-text-properties
		     (point-min) (+ 5 (point-min))
		     '(article-type headers dummy-invisible t))
		    (delete-region beg (point-max)))
		(gnus-article-hide-text-type beg (point-max) 'headers)))))))))

(defun article-hide-boring-headers (&optional arg)
  "Toggle hiding of headers that aren't very interesting.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-article-hidden-arg))
  (when (and (not (gnus-article-check-hidden-text 'boring-headers arg))
	     (not gnus-show-all-headers))
    (save-excursion
      (save-restriction
	(let ((buffer-read-only nil)
	      (list gnus-boring-article-headers)
	      (inhibit-point-motion-hooks t)
	      elem)
	  (article-narrow-to-head)
	  (while list
	    (setq elem (pop list))
	    (goto-char (point-min))
	    (cond
	     ;; Hide empty headers.
	     ((eq elem 'empty)
	      (while (re-search-forward "^[^: \t]+:[ \t]*\n[^ \t]" nil t)
		(forward-line -1)
		(gnus-article-hide-text-type
		 (progn (beginning-of-line) (point))
		 (progn
		   (end-of-line)
		   (if (re-search-forward "^[^ \t]" nil t)
		       (match-beginning 0)
		     (point-max)))
		 'boring-headers)))
	     ;; Hide boring Newsgroups header.
	     ((eq elem 'newsgroups)
	      (when (gnus-string-equal
		     (gnus-fetch-field "newsgroups")
		     (gnus-group-real-name
		      (if (boundp 'gnus-newsgroup-name)
			  gnus-newsgroup-name
			"")))
		(gnus-article-hide-header "newsgroups")))
	     ((eq elem 'to-address)
	      (let ((to (message-fetch-field "to"))
		    (to-address
		     (gnus-parameter-to-address
		      (if (boundp 'gnus-newsgroup-name)
			  gnus-newsgroup-name ""))))
		(when (and to to-address
			   (ignore-errors
			     (gnus-string-equal
			      ;; only one address in To
			      (nth 1 (mail-extract-address-components to))
			      to-address)))
		  (gnus-article-hide-header "to"))))
	     ((eq elem 'followup-to)
	      (when (gnus-string-equal
		     (message-fetch-field "followup-to")
		     (message-fetch-field "newsgroups"))
		(gnus-article-hide-header "followup-to")))
	     ((eq elem 'reply-to)
	      (let ((from (message-fetch-field "from"))
		    (reply-to (message-fetch-field "reply-to")))
		(when (and
		       from reply-to
		       (ignore-errors
			 (gnus-string-equal
			  (nth 1 (mail-extract-address-components from))
			  (nth 1 (mail-extract-address-components reply-to)))))
		  (gnus-article-hide-header "reply-to"))))
	     ((eq elem 'date)
	      (let ((date (message-fetch-field "date")))
		(when (and date
			   (< (days-between (current-time-string) date)
			      4))
		  (gnus-article-hide-header "date"))))
	     ((eq elem 'long-to)
	      (let ((to (message-fetch-field "to"))
		    (cc (message-fetch-field "cc")))
		(when (> (length to) 1024)
		  (gnus-article-hide-header "to"))
		(when (> (length cc) 1024)
		  (gnus-article-hide-header "cc"))))
	     ((eq elem 'many-to)
	      (let ((to-count 0)
		    (cc-count 0))
		(goto-char (point-min))
		(while (re-search-forward "^to:" nil t)
		  (setq to-count (1+ to-count)))
		(when (> to-count 1)
		  (while (> to-count 0)
		    (goto-char (point-min))
		    (save-restriction
		      (re-search-forward "^to:" nil nil to-count)
		      (forward-line -1)
		      (narrow-to-region (point) (point-max))
		      (gnus-article-hide-header "to"))
		    (setq to-count (1- to-count))))
		(goto-char (point-min))
		(while (re-search-forward "^cc:" nil t)
		  (setq cc-count (1+ cc-count)))
		(when (> cc-count 1)
		  (while (> cc-count 0)
		    (goto-char (point-min))
		    (save-restriction
		      (re-search-forward "^cc:" nil nil cc-count)
		      (forward-line -1)
		      (narrow-to-region (point) (point-max))
		      (gnus-article-hide-header "cc"))
		    (setq cc-count (1- cc-count)))))))))))))

(defun gnus-article-hide-header (header)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^" header ":") nil t)
      (gnus-article-hide-text-type
       (progn (beginning-of-line) (point))
       (progn
	 (end-of-line)
	 (if (re-search-forward "^[^ \t]" nil t)
	     (match-beginning 0)
	   (point-max)))
       'boring-headers))))

(defun article-toggle-headers (&optional arg)
  "Toggle hiding of headers.  If given a negative prefix, always show;
if given a positive prefix, always hide."
  (interactive (gnus-article-hidden-arg))
  (let ((force (when (numberp arg)
		 (cond ((> arg 0) 'always-hide)
		       ((< arg 0) 'always-show))))
	(window (get-buffer-window gnus-article-buffer))
	(header-end (point-min))
	header-start field-end field-start
	(inhibit-point-motion-hooks t)
	(inhibit-read-only t))
    (save-restriction
      (widen)
      (while (and (setq header-start
			(text-property-any header-end (point-max)
					   'article-treated-header t))
		  (setq header-end
			(text-property-not-all header-start (point-max)
					       'article-treated-header t)))
	(setq field-end header-start)
	(cond
	 (;; Hide exposed invisible fields.
	  (and (not (eq 'always-show force))
	       (setq field-start
		     (text-property-any field-end header-end
					'exposed-invisible-field t)))
	  (while (and field-start
		      (setq field-end (text-property-not-all
				       field-start header-end
				       'exposed-invisible-field t)))
	    (add-text-properties field-start field-end gnus-hidden-properties)
	    (setq field-start (text-property-any field-end header-end
						 'exposed-invisible-field t)))
	  (put-text-property header-start header-end
			     'exposed-invisible-field nil))
	 (;; Expose invisible fields.
	  (and (not (eq 'always-hide force))
	       (setq field-start
		     (text-property-any field-end header-end 'invisible t)))
	  (while (and field-start
		      (setq field-end (text-property-not-all
				       field-start header-end
				       'invisible t)))
	    ;; If the invisible text is not terminated with newline, we
	    ;; won't expose it.  Because it may be created by x-face-mule.
	    ;; BTW, XEmacs sometimes fail in putting an invisible text
	    ;; property with `gnus-article-hide-text' (really?).  In that
	    ;; case, the invisible text might be started from the middle of
	    ;; a line, so we will expose the sort of thing.
	    (when (or (not (or (eq header-start field-start)
			       (eq ?\n (char-before field-start))))
		      (eq ?\n (char-before field-end))
		      ;; Expose a boundary line anyway.
		      (string-equal
		       "\nX-Boundary: "
		       (buffer-substring (max (- field-end 13) header-start)
					 field-end)))
	      (remove-text-properties field-start field-end
				      gnus-hidden-properties)
	      (put-text-property field-start field-end
				 'exposed-invisible-field t))
	    (setq field-start (text-property-any field-end header-end
						 'invisible t))))
	 (;; Hide fields.
	  (not (eq 'always-show force))
	  (narrow-to-region header-start header-end)
	  (article-hide-headers)
	  ;; Re-display X-Face image under XEmacs.
	  (when (and (featurep 'xemacs)
		     (gnus-functionp gnus-article-x-face-command))
	    (let ((func (cadr (assq 'gnus-treat-display-xface
				    gnus-treatment-function-alist)))
		  (condition 'head))
	      (when (and (not gnus-inhibit-treatment)
			 func
			 (gnus-treat-predicate gnus-treat-display-xface))
		(funcall func)
		(put-text-property header-start header-end 'read-only nil))))
	  (widen))
	 ))
      (goto-char (point-min))
      (when window
	(set-window-start window (point-min))))))

(defvar gnus-article-normalized-header-length 40
  "Length of normalized headers.")

(defun article-normalize-headers ()
  "Make all header lines 40 characters long."
  (interactive)
  (let ((buffer-read-only nil)
	column)
    (save-excursion
      (save-restriction
	(article-narrow-to-head)
	(while (not (eobp))
	  (cond
	   ((< (setq column (- (gnus-point-at-eol) (point)))
	       gnus-article-normalized-header-length)
	    (end-of-line)
	    (insert (make-string
		     (- gnus-article-normalized-header-length column)
		     ? )))
	   ((> column gnus-article-normalized-header-length)
	    (gnus-put-text-property
	     (progn
	       (forward-char gnus-article-normalized-header-length)
	       (point))
	     (gnus-point-at-eol)
	     'invisible t))
	   (t
	    ;; Do nothing.
	    ))
	  (forward-line 1))))))

(defun article-treat-dumbquotes ()
  "Translate M****s*** sm*rtq**t*s into proper text.
Note that this function guesses whether a character is a sm*rtq**t* or
not, so it should only be used interactively.

Sm*rtq**t*s are M****s***'s unilateral extension to the character map
in an attempt to provide more quoting characters.  If you see
something like \\222 or \\264 where you're expecting some kind of
apostrophe or quotation mark, then try this wash."
  (interactive)
  (article-translate-strings gnus-article-dumbquotes-map))

(defun article-translate-characters (from to)
  "Translate all characters in the body of the article according to FROM and TO.
FROM is a string of characters to translate from; to is a string of
characters to translate to."
  (save-excursion
    (when (article-goto-body)
      (let ((buffer-read-only nil)
	    (x (make-string 225 ?x))
	    (i -1))
	(while (< (incf i) (length x))
	  (aset x i i))
	(setq i 0)
	(while (< i (length from))
	  (aset x (aref from i) (aref to i))
	  (incf i))
	(translate-region (point) (point-max) x)))))

(defun article-translate-strings (map)
  "Translate all string in the body of the article according to MAP.
MAP is an alist where the elements are on the form (\"from\" \"to\")."
  (save-excursion
    (when (article-goto-body)
      (let ((buffer-read-only nil)
	    elem)
	(while (setq elem (pop map))
	  (save-excursion
	    (while (search-forward (car elem) nil t)
	      (replace-match (cadr elem)))))))))

(defun article-treat-overstrike ()
  "Translate overstrikes into bold text."
  (interactive)
  (save-excursion
    (when (article-goto-body)
      (let ((buffer-read-only nil))
	(while (search-forward "\b" nil t)
	  (let ((next (char-after))
		start end previous)
	    (backward-char 2)
	    (setq start (point)
		  previous (char-after))
	    (forward-char 3)
	    (setq end (point))
	    (backward-char)
	    ;; We do the boldification/underlining by hiding the
	    ;; overstrikes and putting the proper text property
	    ;; on the letters.
	    (cond
	     ((eq next previous)
	      (gnus-article-hide-text-type start (point) 'overstrike)
	      (put-text-property (point) end 'face 'bold))
	     ((eq next ?_)
	      (gnus-article-hide-text-type
	       (1- (point)) (1+ (point)) 'overstrike)
	      (put-text-property
	       start (1- (point)) 'face 'underline))
	     ((eq previous ?_)
	      (gnus-article-hide-text-type start (point) 'overstrike)
	      (put-text-property
	       (point) end 'face 'underline)))))))))

(defun gnus-article-treat-unfold-headers ()
  "Unfold folded message headers.
Only the headers that fit into the current window width will be
unfolded."
  (interactive)
  (gnus-with-article-headers
    (let (length)
      (while (not (eobp))
	(save-restriction
	  (mail-header-narrow-to-field)
	  (let ((header (buffer-substring (point-min) (point-max))))
	    (with-temp-buffer
	      (insert header)
	      (goto-char (point-min))
	      (while (re-search-forward "\n[\t ]" nil t)
		(replace-match " " t t)))
	    (setq length (- (point-max) (point-min) 1)))
	  (when (< length (window-width))
	    (while (re-search-forward "\n[\t ]" nil t)
	      (replace-match " " t t)))
	  (goto-char (point-max)))))))

(defun gnus-article-treat-fold-headers ()
  "Fold message headers."
  (interactive)
  (gnus-with-article-headers
    (while (not (eobp))
      (save-restriction
	(mail-header-narrow-to-field)
	(mail-header-fold-field)
	(goto-char (point-max))))))

(defun gnus-treat-smiley ()
  "Toggle display of textual emoticons (\"smileys\") as small graphical icons."
  (interactive)
  (unless (featurep 'xemacs)
    (when (and (>= emacs-major-version 21)
	       (not gnus-article-should-use-smiley-mule)
	       gnus-article-smiley-mule-loaded-p)
      (load "smiley" nil t)
      (setq gnus-article-smiley-mule-loaded-p nil))
    (when (and gnus-article-should-use-smiley-mule
	       (not gnus-article-smiley-mule-loaded-p))
      (load "smiley-mule" nil t)
      (setq gnus-article-smiley-mule-loaded-p t)))
  (gnus-with-article-buffer
    (if (memq 'smiley gnus-article-wash-types)
	(gnus-delete-images 'smiley)
      (article-goto-body)
      (let ((images (smiley-region (point) (point-max))))
	(when images
	  (gnus-add-wash-type 'smiley)
	  (dolist (image images)
	    (gnus-add-image 'smiley image)))))))

(defun gnus-article-remove-images ()
  "Remove all images from the article buffer."
  (interactive)
  (gnus-with-article-buffer
    (dolist (elem gnus-article-image-alist)
      (gnus-delete-images (car elem)))))

(defun gnus-article-treat-fold-newsgroups ()
  "Unfold folded message headers.
Only the headers that fit into the current window width will be
unfolded."
  (interactive)
  (gnus-with-article-headers
    (while (gnus-article-goto-header "newsgroups\\|followup-to")
      (save-restriction
	(mail-header-narrow-to-field)
	(while (re-search-forward ", *" nil t)
	  (replace-match ", " t t))
	(mail-header-fold-field)
	(goto-char (point-max))))))

(defun gnus-article-treat-body-boundary ()
  "Place a boundary line at the end of the headers."
  (interactive)
  (when (and gnus-body-boundary-delimiter
	     (> (length gnus-body-boundary-delimiter) 0))
    (gnus-with-article-headers
      (goto-char (point-max))
      (let ((start (point)))
	(insert "X-Boundary: ")
	(gnus-add-text-properties start (point) '(invisible t intangible t))
	(insert (let (str)
		  (while (>= (1- (window-width)) (length str))
		    (setq str (concat str gnus-body-boundary-delimiter)))
		  (substring str 0 (1- (window-width))))
		"\n")
	(gnus-add-text-properties start (point) '(gnus-decoration 'header))))))

(defun article-fill-long-lines ()
  "Fill lines that are wider than the window width."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil)
	  (width (window-width (get-buffer-window (current-buffer)))))
      (save-restriction
	(article-goto-body)
	(let ((adaptive-fill-mode nil)) ;Why?  -sm
	  (while (not (eobp))
	    (end-of-line)
	    (when (>= (current-column) (min fill-column width))
	      (narrow-to-region (point) (gnus-point-at-bol))
	      (fill-paragraph nil)
	      (goto-char (point-max))
	      (widen))
	    (forward-line 1)))))))

(defun article-capitalize-sentences ()
  "Capitalize the first word in each sentence."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil)
	  (paragraph-start "^[\n\^L]"))
      (article-goto-body)
      (while (not (eobp))
	(capitalize-word 1)
	(forward-sentence)))))

(defun article-remove-cr ()
  "Remove trailing CRs and then translate remaining CRs into LFs."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (re-search-forward "\r+$" nil t)
	(replace-match "" t t))
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "\n" t t)))))

(defun article-remove-trailing-blank-lines ()
  "Remove all trailing blank lines from the article."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (delete-region
       (point)
       (progn
	 (while (and (not (bobp))
		     (looking-at "^[ \t]*$")
		     (not (gnus-annotation-in-region-p
			   (point) (gnus-point-at-eol))))
	   (forward-line -1))
	 (forward-line 1)
	 (point))))))

(defun article-display-x-face (&optional force)
  "Look for an X-Face header and display it if present."
  (interactive (list 'force))
  (let ((wash-face-p buffer-read-only))	;; When type `W f'
    (gnus-with-article-headers
      ;; Delete the old process, if any.
      (when (process-status "article-x-face")
	(delete-process "article-x-face"))
      (if (memq 'xface gnus-article-wash-types)
	  ;; We have already displayed X-Faces, so we remove them
	  ;; instead.
	  (gnus-delete-images 'xface)
	;; Display X-Faces.
	(let (x-faces from face grey)
	  (save-excursion
	    (when (and wash-face-p
		       (progn
			 (goto-char (point-min))
			 (not (re-search-forward
			       "^X-Face\\(-[0-9]+\\)?:[\t ]*" nil t)))
		       (gnus-buffer-live-p gnus-original-article-buffer))
	      ;; If type `W f', use gnus-original-article-buffer,
	      ;; otherwise use the current buffer because displaying
	      ;; RFC822 parts calls this function too.
	      (set-buffer gnus-original-article-buffer))
	    (save-restriction
	      (mail-narrow-to-head)
	      (if gnus-treat-display-grey-xface
		  (progn
		    (while (gnus-article-goto-header "X-Face\\(-[0-9]+\\)?")
		      (if (match-beginning 2)
			  (progn
			    (setq grey t)
			    (push (cons (- (string-to-number (match-string 2)))
					(mail-header-field-value))
				  x-faces))
			(push (cons 0 (mail-header-field-value)) x-faces)))
		    (dolist (x-face (prog1
					(if grey
					    (sort x-faces 'car-less-than-car)
					  (nreverse x-faces))
				      (setq x-faces nil)))
		      (push (cdr x-face) x-faces)))
		(while (gnus-article-goto-header "X-Face")
		  (push (mail-header-field-value) x-faces)))
	      (setq from (message-fetch-field "from"))))
	  (if grey
	      (let ((xpm (gnus-convert-gray-x-face-to-xpm x-faces))
		    image)
		(when xpm
		  (setq image (gnus-create-image xpm 'xpm t))
		  (gnus-article-goto-header "from")
		  (when (bobp)
		    (insert "From: [no `from' set]\n")
		    (forward-char -17))
		  (gnus-add-wash-type 'xface)
		  (gnus-add-image 'xface image)
		  (gnus-put-image image)))
	    ;; Sending multiple EOFs to xv doesn't work, so we only do a
	    ;; single external face.
	    (when (stringp gnus-article-x-face-command)
	      (setq x-faces (list (car x-faces))))
	    (while (and (setq face (pop x-faces))
			gnus-article-x-face-command
			(or force
			    ;; Check whether this face is censored.
			    (not gnus-article-x-face-too-ugly)
			    (and gnus-article-x-face-too-ugly from
				 (not (string-match gnus-article-x-face-too-ugly
						    from)))))
	      ;; We display the face.
	      (if (symbolp gnus-article-x-face-command)
		  ;; The command is a lisp function, so we call it.
		  (if (gnus-functionp gnus-article-x-face-command)
		      (funcall gnus-article-x-face-command face)
		    (error "%s is not a function" gnus-article-x-face-command))
		;; The command is a string, so we interpret the command
		;; as a, well, command, and fork it off.
		(let ((process-connection-type nil))
		  (process-kill-without-query
		   (start-process
		    "article-x-face" nil shell-file-name shell-command-switch
		    gnus-article-x-face-command))
		  (with-temp-buffer
		    (insert face)
		    (process-send-region "article-x-face"
					 (point-min) (point-max)))
		  (process-send-eof "article-x-face"))))))))))

(defun article-decode-mime-words ()
  "Decode all MIME-encoded words in the article."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((inhibit-point-motion-hooks t)
	  buffer-read-only
	  (mail-parse-charset gnus-newsgroup-charset)
	  (mail-parse-ignored-charsets
	   (save-excursion (set-buffer gnus-summary-buffer)
			   gnus-newsgroup-ignored-charsets)))
      (mail-decode-encoded-word-region (point-min) (point-max)))))

(defun article-decode-charset (&optional prompt)
  "Decode charset-encoded text in the article.
If PROMPT (the prefix), prompt for a coding system to use."
  (interactive "P")
  (let ((inhibit-point-motion-hooks t) (case-fold-search t)
	buffer-read-only
	(mail-parse-charset gnus-newsgroup-charset)
	(mail-parse-ignored-charsets
	 (save-excursion (condition-case nil
			     (set-buffer gnus-summary-buffer)
			   (error))
			 gnus-newsgroup-ignored-charsets))
	ct cte ctl charset format)
    (save-excursion
      (save-restriction
	(article-narrow-to-head)
	(setq ct (message-fetch-field "Content-Type" t)
	      cte (message-fetch-field "Content-Transfer-Encoding" t)
	      ctl (and ct (ignore-errors
			    (mail-header-parse-content-type ct)))
	      charset (cond
		       (prompt
			(mm-read-coding-system "Charset to decode: "))
		       (ctl
			(mail-content-type-get ctl 'charset)))
	      format (and ctl (mail-content-type-get ctl 'format)))
	(when cte
	  (setq cte (mail-header-strip cte)))
	(if (and ctl (not (string-match "/" (car ctl))))
	    (setq ctl nil))
	(goto-char (point-max)))
      (forward-line 1)
      (save-restriction
	(narrow-to-region (point) (point-max))
	(when (and (eq mail-parse-charset 'gnus-decoded)
		   (eq (mm-body-7-or-8) '8bit))
	  ;; The text code could have been decoded.
	  (setq charset mail-parse-charset))
	(when (and (or (not ctl)
		       (equal (car ctl) "text/plain"))
		   (not format)) ;; article with format will decode later.
	  (mm-decode-body
	   charset (and cte (intern (downcase
				     (gnus-strip-whitespace cte))))
	   (car ctl)))))))

(defun article-decode-encoded-words ()
  "Remove encoded-word encoding from headers."
  (let (buffer-read-only)
    (let ((charset (save-excursion
		     (set-buffer gnus-summary-buffer)
		     default-mime-charset)))
      (mime-decode-header-in-buffer charset))))

(defun article-decode-group-name ()
  "Decode group names in `Newsgroups:'."
  (let ((inhibit-point-motion-hooks t)
	buffer-read-only
	(method (gnus-find-method-for-group gnus-newsgroup-name)))
    (when (and (or gnus-group-name-charset-method-alist
		   gnus-group-name-charset-group-alist)
	       (gnus-buffer-live-p gnus-original-article-buffer))
      (when (nnmail-fetch-field "Newsgroups")
	(nnheader-replace-header "Newsgroups"
				 (gnus-decode-newsgroups
				  (with-current-buffer
				      gnus-original-article-buffer
				    (nnmail-fetch-field "Newsgroups"))
				  gnus-newsgroup-name method)))
      (when (nnmail-fetch-field "Followup-To")
	(nnheader-replace-header "Followup-To"
				 (gnus-decode-newsgroups
				  (with-current-buffer
				      gnus-original-article-buffer
				    (nnmail-fetch-field "Followup-To"))
				  gnus-newsgroup-name method))))))

(defun article-de-quoted-unreadable (&optional force read-charset)
  "Translate a quoted-printable-encoded article.
If FORCE, decode the article whether it is marked as quoted-printable
or not.
If READ-CHARSET, ask for a coding system."
  (interactive (list 'force current-prefix-arg))
  (save-excursion
    (let ((buffer-read-only nil) type charset)
      (if (gnus-buffer-live-p gnus-original-article-buffer)
	  (with-current-buffer gnus-original-article-buffer
	    (setq type
		  (gnus-fetch-field "content-transfer-encoding"))
	    (let* ((ct (gnus-fetch-field "content-type"))
		   (ctl (and ct
			     (ignore-errors
			       (mail-header-parse-content-type ct)))))
	      (setq charset (and ctl
				 (mail-content-type-get ctl 'charset)))
	      (if (stringp charset)
		  (setq charset (intern (downcase charset)))))))
      (if read-charset
	  (setq charset (mm-read-coding-system "Charset: " charset)))
      (unless charset
	(setq charset gnus-newsgroup-charset))
      (when (or force
		(and type (let ((case-fold-search t))
			    (string-match "quoted-printable" type))))
	(article-goto-body)
	(quoted-printable-decode-region
	 (point) (point-max) (mm-charset-to-coding-system charset))))))

(defun article-de-base64-unreadable (&optional force read-charset)
  "Translate a base64 article.
If FORCE, decode the article whether it is marked as base64 not.
If READ-CHARSET, ask for a coding system."
  (interactive (list 'force current-prefix-arg))
  (save-excursion
    (let ((buffer-read-only nil) type charset)
      (if (gnus-buffer-live-p gnus-original-article-buffer)
	  (with-current-buffer gnus-original-article-buffer
	    (setq type
		  (gnus-fetch-field "content-transfer-encoding"))
	    (let* ((ct (gnus-fetch-field "content-type"))
		   (ctl (and ct
			     (ignore-errors
			       (mail-header-parse-content-type ct)))))
	      (setq charset (and ctl
				 (mail-content-type-get ctl 'charset)))
	      (if (stringp charset)
		  (setq charset (intern (downcase charset)))))))
      (if read-charset
	  (setq charset (mm-read-coding-system "Charset: " charset)))
      (unless charset
	(setq charset gnus-newsgroup-charset))
      (when (or force
		(and type (let ((case-fold-search t))
			    (string-match "base64" type))))
	(article-goto-body)
	(save-restriction
	  (narrow-to-region (point) (point-max))
	  (base64-decode-region (point-min) (point-max))
	  (mm-decode-coding-region
	   (point-min) (point-max) (mm-charset-to-coding-system charset)))))))

(eval-when-compile
  (require 'rfc1843))

(defun article-decode-HZ ()
  "Translate a HZ-encoded article."
  (interactive)
  (require 'rfc1843)
  (save-excursion
    (let ((buffer-read-only nil))
      (rfc1843-decode-region (point-min) (point-max)))))

(defun article-unsplit-urls ()
  "Remove the newlines that some other mailers insert into URLs."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\(\\(https?\\|ftp\\)://\\S-+\\)\n\\(\\S-+\\)" nil t)
	(replace-match "\\1\\3" t)))))

(defun article-wash-html (&optional read-charset)
  "Format an html article.
If READ-CHARSET, ask for a coding system."
  (interactive "P")
  (save-excursion
    (let ((buffer-read-only nil)
	  charset)
      (if (gnus-buffer-live-p gnus-original-article-buffer)
	  (with-current-buffer gnus-original-article-buffer
	    (let* ((ct (gnus-fetch-field "content-type"))
		   (ctl (and ct
			     (ignore-errors
			       (mail-header-parse-content-type ct)))))
	      (setq charset (and ctl
				 (mail-content-type-get ctl 'charset)))
	      (if (stringp charset)
		  (setq charset (intern (downcase charset)))))))
      (if read-charset
	  (setq charset (mm-read-coding-system "Charset: " charset)))
      (unless charset
	(setq charset gnus-newsgroup-charset))
      (article-goto-body)
      (save-window-excursion
	(save-restriction
	  (narrow-to-region (point) (point-max))
	  (let* ((func (or gnus-article-wash-function mm-text-html-renderer))
		 (entry (assq func mm-text-html-washer-alist)))
	    (if entry
		(setq func (cdr entry)))
	    (cond
	     ((gnus-functionp func)
	      (funcall func))
	     (t
	      (apply (car func) (cdr func))))))))))

(defun gnus-article-wash-html-with-w3 ()
  "Wash the current buffer with w3."
  (mm-setup-w3)
  (let ((w3-strict-width (window-width))
	(url-standalone-mode t)
	(url-gateway-unplugged t)
	(w3-honor-stylesheets nil)
	(w3-delay-image-loads t))
    (condition-case var
	(w3-region (point-min) (point-max))
      (error))))

(defun gnus-article-wash-html-with-w3m ()
  "Wash the current buffer with emacs-w3m."
  (mm-setup-w3m)
  (save-restriction
    (narrow-to-region (point) (point-max))
    (let ((w3m-safe-url-regexp (if mm-inline-text-html-with-images
				   nil
				 "\\`cid:"))
	  (w3m-display-inline-images mm-inline-text-html-with-images)
	  w3m-force-redisplay)
      (w3m-region (point-min) (point-max)))
    (when mm-inline-text-html-with-w3m-keymap
      (add-text-properties
       (point-min) (point-max)
       (append '(mm-inline-text-html-with-w3m t)
	       (gnus-local-map-property mm-w3m-mode-map))))))

(defun article-hide-list-identifiers ()
  "Remove list identifies from the Subject header.
The `gnus-list-identifiers' variable specifies what to do."
  (interactive)
  (let ((inhibit-point-motion-hooks t)
	(regexp (if (consp gnus-list-identifiers)
		    (mapconcat 'identity gnus-list-identifiers " *\\|")
		  gnus-list-identifiers))
	buffer-read-only)
    (when regexp
      (save-excursion
	(save-restriction
	  (article-narrow-to-head)
	  (goto-char (point-min))
	  (while (re-search-forward
		  (concat "^Subject: +\\(R[Ee]: +\\)*\\(" regexp " *\\)")
		  nil t)
	    (delete-region (match-beginning 2) (match-end 0))
	    (beginning-of-line))
	  (when (re-search-forward
		 "^Subject: +\\(\\(R[Ee]: +\\)+\\)R[Ee]: +" nil t)
	    (delete-region (match-beginning 1) (match-end 1))))))))

(defun article-hide-pgp ()
  "Remove any PGP headers and signatures in the current article."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((inhibit-point-motion-hooks t)
	    buffer-read-only beg end)
	(article-goto-body)
	;; Hide the "header".
	(when (re-search-forward "^-----BEGIN PGP SIGNED MESSAGE-----\n" nil t)
	  (gnus-add-wash-type 'pgp)
	  (delete-region (match-beginning 0) (match-end 0))
	  ;; Remove armor headers (rfc2440 6.2)
	  (delete-region (point) (or (re-search-forward "^[ \t]*\n" nil t)
				     (point)))
	  (setq beg (point))
	  ;; Hide the actual signature.
	  (and (search-forward "\n-----BEGIN PGP SIGNATURE-----\n" nil t)
	       (setq end (1+ (match-beginning 0)))
	       (delete-region
		end
		(if (search-forward "\n-----END PGP SIGNATURE-----\n" nil t)
		    (match-end 0)
		  ;; Perhaps we shouldn't hide to the end of the buffer
		  ;; if there is no end to the signature?
		  (point-max))))
	  ;; Hide "- " PGP quotation markers.
	  (when (and beg end)
	    (narrow-to-region beg end)
	    (goto-char (point-min))
	    (while (re-search-forward "^- " nil t)
	      (delete-region
	       (match-beginning 0) (match-end 0)))
	    (widen))
	  (gnus-run-hooks 'gnus-article-hide-pgp-hook))))))

(defun article-hide-pem (&optional arg)
  "Toggle hiding of any PEM headers and signatures in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-article-hidden-arg))
  (unless (gnus-article-check-hidden-text 'pem arg)
    (save-excursion
      (let (buffer-read-only end)
	(goto-char (point-min))
	;; Hide the horrendously ugly "header".
	(when (and (search-forward
		    "\n-----BEGIN PRIVACY-ENHANCED MESSAGE-----\n"
		    nil t)
		   (setq end (1+ (match-beginning 0))))
	  (gnus-add-wash-type 'pem)
	  (gnus-article-hide-text-type
	   end
	   (if (search-forward "\n\n" nil t)
	       (match-end 0)
	     (point-max))
	   'pem)
	  ;; Hide the trailer as well
	  (when (search-forward "\n-----END PRIVACY-ENHANCED MESSAGE-----\n"
				nil t)
	    (gnus-article-hide-text-type
	     (match-beginning 0) (match-end 0) 'pem)))))))

(defun article-strip-banner ()
  "Strip the banner specified by the `banner' group parameter."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((inhibit-point-motion-hooks t)
	    (banner (gnus-parameter-banner gnus-newsgroup-name))
	    (gnus-signature-limit nil)
	    buffer-read-only beg end)
	(when banner
	  (article-goto-body)
	  (cond
	   ((eq banner 'signature)
	    (when (gnus-article-narrow-to-signature)
	      (widen)
	      (forward-line -1)
	      (delete-region (point) (point-max))))
	   ((symbolp banner)
	    (if (setq banner (cdr (assq banner gnus-article-banner-alist)))
		(while (re-search-forward banner nil t)
		  (delete-region (match-beginning 0) (match-end 0)))))
	   ((stringp banner)
	    (while (re-search-forward banner nil t)
	      (delete-region (match-beginning 0) (match-end 0))))))))))

(defun article-babel ()
  "Translate article using an online translation service."
  (interactive)
  (require 'babel)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (when (article-goto-body)
      (let* ((buffer-read-only nil)
	     (start (point))
	     (end (point-max))
	     (orig (buffer-substring start end))
	     (trans (babel-as-string orig)))
	(save-restriction
	  (narrow-to-region start end)
	  (delete-region start end)
	  (insert trans))))))

(defun article-hide-signature (&optional arg)
  "Hide the signature in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-article-hidden-arg))
  (save-excursion
    (save-restriction
      (if (interactive-p)
	  (progn
	    (widen)
	    (article-goto-body))
	(goto-char (point-min)))
      (unless (gnus-article-check-hidden-text 'signature arg)
	(let ((buffer-read-only nil)
	      (button (point)))
	  (while (setq button (text-property-any button (point-max)
						 'gnus-callback
						 'gnus-signature-toggle))
	    (setq button (text-property-not-all button (point-max)
						'gnus-callback
						'gnus-signature-toggle))
	    (when (and button (not (eobp)))
	      (gnus-article-hide-text-type
	       (1+ button)
	       (next-single-property-change (1+ button) 'mime-view-entity
					    nil (point-max))
	       'signature)))))))
  (gnus-set-mode-line 'article))

(defun article-strip-headers-in-body ()
  "Strip offensive headers from bodies."
  (interactive)
  (save-excursion
    (article-goto-body)
    (let ((case-fold-search t))
      (when (looking-at "x-no-archive:")
	(gnus-delete-line)))))

(defun article-strip-leading-blank-lines ()
  "Remove all blank lines from the beginning of the article."
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  buffer-read-only)
      (when (article-goto-body)
	(while (and (not (eobp))
		    (looking-at "[ \t]*$"))
	  (gnus-delete-line))))))

(defun article-narrow-to-head ()
  "Narrow the buffer to the head of the message.
Point is left at the beginning of the narrowed-to region."
  (narrow-to-region
   (goto-char (point-min))
   (if (search-forward "\n\n" nil 1)
       (1- (point))
     (point-max)))
  (goto-char (point-min)))

(defun article-goto-body ()
  "Place point at the start of the body."
  (goto-char (point-min))
  (cond
   ;; This variable is only bound when dealing with separate
   ;; MIME body parts.
   (article-goto-body-goes-to-point-min-p
    t)
   ((search-forward "\n\n" nil t)
    t)
   (t
    (goto-char (point-max))
    nil)))

(defun article-strip-multiple-blank-lines ()
  "Replace consecutive blank lines with one empty line."
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  buffer-read-only)
      ;; First make all blank lines empty.
      (article-goto-body)
      (while (re-search-forward "^[ \t]+$" nil t)
	(unless (gnus-annotation-in-region-p
		 (match-beginning 0) (match-end 0))
	  (replace-match "" nil t)))
      ;; Then replace multiple empty lines with a single empty line.
      (article-goto-body)
      (while (re-search-forward "\n\n\\(\n+\\)" nil t)
	(unless (gnus-annotation-in-region-p
		 (match-beginning 0) (match-end 0))
	  (delete-region (match-beginning 1) (match-end 1)))))))

(defun article-strip-leading-space ()
  "Remove all white space from the beginning of the lines in the article."
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  buffer-read-only)
      (article-goto-body)
      (while (re-search-forward "^[ \t]+" nil t)
	(replace-match "" t t)))))

(defun article-strip-trailing-space ()
  "Remove all white space from the end of the lines in the article."
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  buffer-read-only)
      (article-goto-body)
      (while (re-search-forward "[ \t]+$" nil t)
	(replace-match "" t t)))))

(defun article-strip-blank-lines ()
  "Strip leading, trailing and multiple blank lines."
  (interactive)
  (article-strip-leading-blank-lines)
  (article-remove-trailing-blank-lines)
  (article-strip-multiple-blank-lines))

(defun article-strip-all-blank-lines ()
  "Strip all blank lines."
  (interactive)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
	  buffer-read-only)
      (article-goto-body)
      (while (re-search-forward "^[ \t]*\n" nil t)
	(replace-match "" t t)))))

(defun gnus-article-narrow-to-signature ()
  "Narrow to the signature; return t if a signature is found, else nil."
  (let ((inhibit-point-motion-hooks t))
    (when (gnus-article-search-signature)
      (forward-line 1)
      ;; Check whether we have some limits to what we consider
      ;; to be a signature.
      (let ((limits (if (listp gnus-signature-limit) gnus-signature-limit
		      (list gnus-signature-limit)))
	    limit limited)
	(while (setq limit (pop limits))
	  (if (or (and (integerp limit)
		       (< (- (point-max) (point)) limit))
		  (and (floatp limit)
		       (< (count-lines (point) (point-max)) limit))
		  (and (gnus-functionp limit)
		       (funcall limit))
		  (and (stringp limit)
		       (not (re-search-forward limit nil t))))
	      ()			; This limit did not succeed.
	    (setq limited t
		  limits nil)))
	(unless limited
	  (narrow-to-region (point) (point-max))
	  t)))))

(defun gnus-article-search-signature ()
  "Search the current buffer for the signature separator.
Put point at the beginning of the signature separator."
  (let ((cur (point)))
    (goto-char (point-max))
    (if (if (stringp gnus-signature-separator)
	    (re-search-backward gnus-signature-separator nil t)
	  (let ((seps gnus-signature-separator))
	    (while (and seps
			(not (re-search-backward (car seps) nil t)))
	      (pop seps))
	    seps))
	t
      (goto-char cur)
      nil)))

(defun gnus-article-hidden-arg ()
  "Return the current prefix arg as a number, or 0 if no prefix."
  (list (if current-prefix-arg
	    (prefix-numeric-value current-prefix-arg)
	  0)))

(defun gnus-article-check-hidden-text (type arg)
  "Return nil if hiding is necessary.
Arg can be nil or a number.  nil and positive means hide, negative
means show, 0 means toggle."
  (save-excursion
    (save-restriction
      (let ((hide (gnus-article-hidden-text-p type)))
	(cond
	 ((or (null arg)
	      (> arg 0))
	  nil)
	 ((< arg 0)
	  (gnus-article-show-hidden-text type)
	  t)
	 (t
	  (if (eq hide 'hidden)
	      (progn
		(gnus-article-show-hidden-text type)
		t)
	    nil)))))))

(defun gnus-article-hidden-text-p (type)
  "Say whether the current buffer contains hidden text of type TYPE."
  (let ((pos (text-property-any (point-min) (point-max) 'article-type type)))
    (while (and pos
		(not (get-text-property pos 'invisible))
		(not (get-text-property pos 'dummy-invisible)))
      (setq pos
	    (text-property-any (1+ pos) (point-max) 'article-type type)))
    (if pos
	'hidden
      nil)))

(defun gnus-article-show-hidden-text (type &optional dummy)
  "Show all hidden text of type TYPE.
Originally it is hide instead of DUMMY."
  (let ((buffer-read-only nil)
	(inhibit-point-motion-hooks t))
    (gnus-remove-text-properties-when
     'article-type type
     (point-min) (point-max)
     (cons 'article-type (cons type
			       gnus-hidden-properties)))
    (gnus-delete-wash-type type)))

(defconst article-time-units
  `((year . ,(* 365.25 24 60 60))
    (week . ,(* 7 24 60 60))
    (day . ,(* 24 60 60))
    (hour . ,(* 60 60))
    (minute . 60)
    (second . 1))
  "Mapping from time units to seconds.")

(defun article-date-ut (&optional type highlight header)
  "Convert DATE date to universal time in the current article.
If TYPE is `local', convert to local time; if it is `lapsed', output
how much time has lapsed since DATE.  For `lapsed', the value of
`gnus-article-date-lapsed-new-header' says whether the \"X-Sent:\" header
should replace the \"Date:\" one, or should be added below it."
  (interactive (list 'ut t))
  (let* ((header (or header
		     (and (eq 1 (point-min))
			  (mail-header-date (save-excursion
					      (set-buffer gnus-summary-buffer)
					      gnus-current-headers)))
		     (message-fetch-field "date")
		     ""))
	 (date (if (vectorp header) (mail-header-date header)
		 header))
	 (inhibit-point-motion-hooks t)
	 bface eface date-pos)
    (when (and date (not (string= date "")))
      (save-excursion
	(save-restriction
	  (article-narrow-to-head)
	  (when (or (and (eq type 'lapsed)
			 gnus-article-date-lapsed-new-header
			 ;; Attempt to get the face of X-Sent first.
			 (re-search-forward "^X-Sent:[ \t]" nil t))
		    (re-search-forward "^Date:[ \t]" nil t)
		    ;; If Date is missing, try again for X-Sent.
		    (re-search-forward "^X-Sent:[ \t]" nil t))
	    (setq bface (get-text-property (gnus-point-at-bol) 'face)
		  date (or (get-text-property (gnus-point-at-bol)
					      'original-date)
			   date)
		  eface (get-text-property (1- (gnus-point-at-eol))
					   'face)))
	  (let ((buffer-read-only nil))
	    ;; Delete any old X-Sent headers.
	    (when (setq date-pos
			(text-property-any (point-min) (point-max)
					   'article-date-lapsed t))
	      (goto-char (setq date-pos (set-marker (make-marker) date-pos)))
	      (delete-region (match-beginning 0)
			     (progn (forward-line 1) (point))))
	    (goto-char (point-min))
	    ;; Delete any old Date headers.
	    (while (re-search-forward "^Date:[ \t]" nil t)
	      (unless date-pos
		(setq date-pos (match-beginning 0)))
	      (unless (and (eq type 'lapsed)
			   gnus-article-date-lapsed-new-header)
		(delete-region (match-beginning 0)
			       (progn (message-next-header) (point)))))
	    (if date-pos
		(progn
		  (goto-char date-pos)
		  (unless (bolp)
		    ;; Possibly, Date has been deleted.
		    (insert "\n"))
		  (when (and (eq type 'lapsed)
			     gnus-article-date-lapsed-new-header
			     (looking-at "Date:"))
		    (forward-line 1)))
	      (goto-char (point-min)))
	    (insert (article-make-date-line date type))
	    (when (eq type 'lapsed)
	      (put-text-property (gnus-point-at-bol) (point)
				 'article-date-lapsed t))
	    (insert "\n")
	    (forward-line -1)
	    ;; Do highlighting.
	    (when (looking-at "\\([^:]+\\): *\\(.*\\)$")
	      (put-text-property (match-beginning 1) (1+ (match-end 1))
				 'original-date date)
	      (put-text-property (match-beginning 1) (1+ (match-end 1))
				 'face bface)
	      (put-text-property (match-beginning 2) (match-end 2)
				 'face eface))))))))

(defun article-make-date-line (date type)
  "Return a DATE line of TYPE."
  (unless (memq type '(local ut original user iso8601 lapsed english))
    (error "Unknown conversion type: %s" type))
  (condition-case ()
      (let ((time (date-to-time date)))
	(cond
	 ;; Convert to the local timezone.
	 ((eq type 'local)
	  (let ((tz (car (current-time-zone time))))
	    (format "Date: %s %s%02d%02d" (current-time-string time)
		    (if (> tz 0) "+" "-") (/ (abs tz) 3600)
		    (/ (% (abs tz) 3600) 60))))
	 ;; Convert to Universal Time.
	 ((eq type 'ut)
	  (concat "Date: "
		  (current-time-string
		   (let* ((e (parse-time-string date))
			  (tm (apply 'encode-time e))
			  (ms (car tm))
			  (ls (- (cadr tm) (car (current-time-zone time)))))
		     (cond ((< ls 0) (list (1- ms) (+ ls 65536)))
			   ((> ls 65535) (list (1+ ms) (- ls 65536)))
			   (t (list ms ls)))))
		  " UT"))
	 ;; Get the original date from the article.
	 ((eq type 'original)
	  (concat "Date: " (if (string-match "\n+$" date)
			       (substring date 0 (match-beginning 0))
			     date)))
	 ;; Let the user define the format.
	 ((eq type 'user)
	  (if (gnus-functionp gnus-article-time-format)
	      (funcall gnus-article-time-format time)
	    (concat
	     "Date: "
	     (format-time-string gnus-article-time-format time))))
	 ;; ISO 8601.
	 ((eq type 'iso8601)
	  (let ((tz (car (current-time-zone time))))
	    (concat
	     "Date: "
	     (format-time-string "%Y%m%dT%H%M%S" time)
	     (format "%s%02d%02d"
		     (if (> tz 0) "+" "-") (/ (abs tz) 3600)
		     (/ (% (abs tz) 3600) 60)))))
	 ;; Do an X-Sent lapsed format.
	 ((eq type 'lapsed)
	  ;; If the date is seriously mangled, the timezone functions are
	  ;; liable to bug out, so we ignore all errors.
	  (let* ((now (current-time))
		 (real-time (subtract-time now time))
		 (real-sec (and real-time
				(+ (* (float (car real-time)) 65536)
				   (cadr real-time))))
		 (sec (and real-time (abs real-sec)))
		 num prev)
	    (cond
	     ((null real-time)
	      "X-Sent: Unknown")
	     ((zerop sec)
	      "X-Sent: Now")
	     (t
	      (concat
	       "X-Sent: "
	       ;; This is a bit convoluted, but basically we go
	       ;; through the time units for years, weeks, etc,
	       ;; and divide things to see whether that results
	       ;; in positive answers.
	       (mapconcat
		(lambda (unit)
		  (if (zerop (setq num (ffloor (/ sec (cdr unit)))))
		      ;; The (remaining) seconds are too few to
		      ;; be divided into this time unit.
		      ""
		    ;; It's big enough, so we output it.
		    (setq sec (- sec (* num (cdr unit))))
		    (prog1
			(concat (if prev ", " "") (int-to-string
						   (floor num))
				" " (symbol-name (car unit))
				(if (> num 1) "s" ""))
		      (setq prev t))))
		article-time-units "")
	       ;; If dates are odd, then it might appear like the
	       ;; article was sent in the future.
	       (if (> real-sec 0)
		   " ago"
		 " in the future"))))))
	 ;; Display the date in proper English
	 ((eq type 'english)
	  (let ((dtime (decode-time time)))
	    (concat
	     "Date: the "
	     (number-to-string (nth 3 dtime))
	     (let ((digit (% (nth 3 dtime) 10)))
	       (cond
		((memq (nth 3 dtime) '(11 12 13)) "th")
		((= digit 1) "st")
		((= digit 2) "nd")
		((= digit 3) "rd")
		(t "th")))
	     " of "
	     (nth (1- (nth 4 dtime)) gnus-english-month-names)
	     " "
	     (number-to-string (nth 5 dtime))
	     " at "
	     (format "%02d" (nth 2 dtime))
	     ":"
	     (format "%02d" (nth 1 dtime)))))))
    (error
     (format "Date: %s (from Oort)" date))))

(defun article-date-local (&optional highlight)
  "Convert the current article date to the local timezone."
  (interactive (list t))
  (article-date-ut 'local highlight))

(defun article-date-english (&optional highlight)
  "Convert the current article date to something that is proper English."
  (interactive (list t))
  (article-date-ut 'english highlight))

(defun article-date-original (&optional highlight)
  "Convert the current article date to what it was originally.
This is only useful if you have used some other date conversion
function and want to see what the date was before converting."
  (interactive (list t))
  (article-date-ut 'original highlight))

(defun article-date-lapsed (&optional highlight)
  "Convert the current article date to time lapsed since it was sent."
  (interactive (list t))
  (article-date-ut 'lapsed highlight))

(defun article-update-date-lapsed ()
  "Function to be run from a timer to update the lapsed time line."
  (let (deactivate-mark)
    (save-excursion
      (ignore-errors
	(walk-windows
	 (lambda (w)
	   (set-buffer (window-buffer w))
	   (when (eq major-mode 'gnus-article-mode)
	     (goto-char (point-min))
	     (when (re-search-forward "^X-Sent:" nil t)
	       (article-date-lapsed t))))
	 nil 'visible)))))

(defun gnus-start-date-timer (&optional n)
  "Start a timer to update the X-Sent header in the article buffers.
The numerical prefix says how frequently (in seconds) the function
is to run."
  (interactive "p")
  (unless n
    (setq n 1))
  (gnus-stop-date-timer)
  (setq article-lapsed-timer
	(nnheader-run-at-time 1 n 'article-update-date-lapsed)))

(defun gnus-stop-date-timer ()
  "Stop the X-Sent timer."
  (interactive)
  (when article-lapsed-timer
    (nnheader-cancel-timer article-lapsed-timer)
    (setq article-lapsed-timer nil)))

(defun article-date-user (&optional highlight)
  "Convert the current article date to the user-defined format.
This format is defined by the `gnus-article-time-format' variable."
  (interactive (list t))
  (article-date-ut 'user highlight))

(defun article-date-iso8601 (&optional highlight)
  "Convert the current article date to ISO8601."
  (interactive (list t))
  (article-date-ut 'iso8601 highlight))

;; (defun article-show-all ()
;;   "Show all hidden text in the article buffer."
;;   (interactive)
;;   (save-excursion
;;     (widen)
;;     (let ((buffer-read-only nil))
;;       (gnus-article-unhide-text (point-min) (point-max))
;;       (gnus-remove-text-with-property 'gnus-prev)
;;       (gnus-remove-text-with-property 'gnus-next))))

(defun article-show-all-headers ()
  "Show all hidden headers in the article buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (article-narrow-to-head)
      (let ((buffer-read-only nil))
	(gnus-article-unhide-text (point-min) (point-max))))))

(defun article-remove-leading-whitespace ()
  "Remove excessive whitespace from all headers."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((buffer-read-only nil))
	(article-narrow-to-head)
	(goto-char (point-min))
	(while (re-search-forward "^[^ :]+: \\([ \t]+\\)" nil t)
	  (delete-region (match-beginning 1) (match-end 1)))))))

(defun article-emphasize (&optional arg)
  "Emphasize text according to `gnus-emphasis-alist'."
  (interactive (gnus-article-hidden-arg))
  (unless (gnus-article-check-hidden-text 'emphasis arg)
    (save-excursion
      (let ((alist (or
		    (condition-case nil
			(with-current-buffer gnus-summary-buffer
			  gnus-article-emphasis-alist)
		      (error))
		    gnus-emphasis-alist))
	    (buffer-read-only nil)
	    (props (append '(article-type emphasis)
			   gnus-hidden-properties))
	    regexp elem beg invisible visible face)
	(article-goto-body)
	(setq beg (point))
	(while (setq elem (pop alist))
	  (goto-char beg)
	  (setq regexp (car elem)
		invisible (nth 1 elem)
		visible (nth 2 elem)
		face (nth 3 elem))
	  (while (re-search-forward regexp nil t)
	    (when (and (match-beginning visible) (match-beginning invisible))
	      (gnus-article-hide-text
	       (match-beginning invisible) (match-end invisible) props)
	      (gnus-article-unhide-text-type
	       (match-beginning visible) (match-end visible) 'emphasis)
	      (gnus-put-overlay-excluding-newlines
	       (match-beginning visible) (match-end visible) 'face face)
	      (gnus-add-wash-type 'emphasis)
	      (goto-char (match-end invisible)))))))))

(defun gnus-article-setup-highlight-words (&optional highlight-words)
  "Setup newsgroup emphasis alist."
  (unless gnus-article-emphasis-alist
    (let ((name (and gnus-newsgroup-name
		     (gnus-group-real-name gnus-newsgroup-name))))
      (make-local-variable 'gnus-article-emphasis-alist)
      (setq gnus-article-emphasis-alist
	    (nconc
	     (let ((alist gnus-group-highlight-words-alist) elem highlight)
	       (while (setq elem (pop alist))
		 (when (and name (string-match (car elem) name))
		   (setq alist nil
			 highlight (copy-sequence (cdr elem)))))
	       highlight)
	     (copy-sequence highlight-words)
	     (if gnus-newsgroup-name
		 (copy-sequence (gnus-group-find-parameter
				 gnus-newsgroup-name 'highlight-words t)))
	     gnus-emphasis-alist)))))

(eval-when-compile
  (defvar gnus-summary-article-menu)
  (defvar gnus-summary-post-menu))

;;; Saving functions.

(defun gnus-article-save (save-buffer file &optional num)
  "Save the currently selected article."
  (unless gnus-save-all-headers
    ;; Remove headers according to `gnus-saved-headers'.
    (let ((gnus-visible-headers
	   (or gnus-saved-headers gnus-visible-headers))
	  (gnus-article-buffer save-buffer))
      (save-excursion
	(set-buffer save-buffer)
	(article-hide-headers 1 t))))
  (save-window-excursion
    (if (not gnus-default-article-saver)
	(error "No default saver is defined")
      ;; !!! Magic!  The saving functions all save
      ;; `gnus-save-article-buffer' (or so they think), but we
      ;; bind that variable to our save-buffer.
      (set-buffer gnus-article-buffer)
      (let* ((gnus-save-article-buffer save-buffer)
	     (filename
	      (cond
	       ((not gnus-prompt-before-saving) 'default)
	       ((eq gnus-prompt-before-saving 'always) nil)
	       (t file)))
	     (gnus-number-of-articles-to-be-saved
	      (when (eq gnus-prompt-before-saving t)
		num)))			; Magic
	(set-buffer gnus-article-current-summary)
	(funcall gnus-default-article-saver filename)))))

(defun gnus-read-save-file-name (prompt &optional filename
					function group headers variable)
  (let ((default-name
	  (funcall function group headers (symbol-value variable)))
	result)
    (setq result
	  (expand-file-name
	   (cond
	    ((eq filename 'default)
	     default-name)
	    ((eq filename t)
	     default-name)
	    (filename filename)
	    (t
	     (let* ((split-name (gnus-get-split-value gnus-split-methods))
		    (prompt
		     (format prompt
			     (if (and gnus-number-of-articles-to-be-saved
				      (> gnus-number-of-articles-to-be-saved 1))
				 (format "these %d articles"
					 gnus-number-of-articles-to-be-saved)
			       "this article")))
		    (file
		     ;; Let the split methods have their say.
		     (cond
		      ;; No split name was found.
		      ((null split-name)
		       (read-file-name
			(concat prompt " (default "
				(file-name-nondirectory default-name) ") ")
			(file-name-directory default-name)
			default-name))
		      ;; A single group name is returned.
		      ((stringp split-name)
		       (setq default-name
			     (funcall function split-name headers
				      (symbol-value variable)))
		       (read-file-name
			(concat prompt " (default "
				(file-name-nondirectory default-name) ") ")
			(file-name-directory default-name)
			default-name))
		      ;; A single split name was found
		      ((= 1 (length split-name))
		       (let* ((name (expand-file-name
				     (car split-name)
				     gnus-article-save-directory))
			      (dir (cond ((file-directory-p name)
					  (file-name-as-directory name))
					 ((file-exists-p name) name)
					 (t gnus-article-save-directory))))
			 (read-file-name
			  (concat prompt " (default " name ") ")
			  dir name)))
		      ;; A list of splits was found.
		      (t
		       (setq split-name (nreverse split-name))
		       (let (result)
			 (let ((file-name-history
				(nconc split-name file-name-history)))
			   (setq result
				 (expand-file-name
				  (read-file-name
				   (concat prompt " (`M-p' for defaults) ")
				   gnus-article-save-directory
				   (car split-name))
				  gnus-article-save-directory)))
			 (car (push result file-name-history)))))))
	       ;; Create the directory.
	       (gnus-make-directory (file-name-directory file))
	       ;; If we have read a directory, we append the default file name.
	       (when (file-directory-p file)
		 (setq file (expand-file-name (file-name-nondirectory
					       default-name)
					      (file-name-as-directory file))))
	       ;; Possibly translate some characters.
	       (nnheader-translate-file-chars file))))))
    (gnus-make-directory (file-name-directory result))
    (set variable result)))

(defun gnus-article-archive-name (group)
  "Return the first instance of an \"Archive-name\" in the current buffer."
  (let ((case-fold-search t))
    (when (re-search-forward "archive-name: *\\([^ \n\t]+\\)[ \t]*$" nil t)
      (nnheader-concat gnus-article-save-directory
		       (match-string 1)))))

(defun gnus-article-nndoc-name (group)
  "If GROUP is an nndoc group, return the name of the parent group."
  (when (eq (car (gnus-find-method-for-group group)) 'nndoc)
    (gnus-group-get-parameter group 'save-article-group)))

(defun gnus-summary-save-in-rmail (&optional filename)
  "Append this article to Rmail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (setq filename (gnus-read-save-file-name
		  "Save %s in rmail file:" filename
		  gnus-rmail-save-name gnus-newsgroup-name
		  gnus-current-headers 'gnus-newsgroup-last-rmail))
  (gnus-eval-in-buffer-window gnus-save-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(gnus-output-to-rmail filename))))
  filename)

(defun gnus-summary-save-in-mail (&optional filename)
  "Append this article to Unix mail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (setq filename (gnus-read-save-file-name
		  "Save %s in Unix mail file:" filename
		  gnus-mail-save-name gnus-newsgroup-name
		  gnus-current-headers 'gnus-newsgroup-last-mail))
  (gnus-eval-in-buffer-window gnus-save-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(if (and (file-readable-p filename)
		 (mail-file-babyl-p filename))
	    (rmail-output-to-rmail-file filename t)
	  (gnus-output-to-mail filename)))))
  filename)

(defun gnus-summary-save-in-file (&optional filename overwrite)
  "Append this article to file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory'."
  (setq filename (gnus-read-save-file-name
		  "Save %s in file:" filename
		  gnus-file-save-name gnus-newsgroup-name
		  gnus-current-headers 'gnus-newsgroup-last-file))
  (gnus-eval-in-buffer-window gnus-save-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(when (and overwrite
		   (file-exists-p filename))
	  (delete-file filename))
	(gnus-output-to-file filename))))
  filename)

(defun gnus-summary-write-to-file (&optional filename)
  "Write this article to a file, overwriting it if the file exists.
Optional argument FILENAME specifies file name.
The directory to save in defaults to `gnus-article-save-directory'."
  (gnus-summary-save-in-file nil t))

(defun gnus-summary-save-body-in-file (&optional filename)
  "Append this article body to a file.
Optional argument FILENAME specifies file name.
The directory to save in defaults to `gnus-article-save-directory'."
  (setq filename (gnus-read-save-file-name
		  "Save %s body in file:" filename
		  gnus-file-save-name gnus-newsgroup-name
		  gnus-current-headers 'gnus-newsgroup-last-file))
  (gnus-eval-in-buffer-window gnus-save-article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(when (article-goto-body)
	  (narrow-to-region (point) (point-max)))
	(gnus-output-to-file filename))))
  filename)

(defun gnus-summary-save-in-pipe (&optional command)
  "Pipe this article to subprocess."
  (setq command
	(cond ((and (eq command 'default)
		    gnus-last-shell-command)
	       gnus-last-shell-command)
	      ((stringp command)
	       command)
	      (t (read-string
		  (format
		   "Shell command on %s: "
		   (if (and gnus-number-of-articles-to-be-saved
			    (> gnus-number-of-articles-to-be-saved 1))
		       (format "these %d articles"
			       gnus-number-of-articles-to-be-saved)
		     "this article"))
		  gnus-last-shell-command))))
  (when (string-equal command "")
    (if gnus-last-shell-command
	(setq command gnus-last-shell-command)
      (error "A command is required")))
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-restriction
      (widen)
      (shell-command-on-region (point-min) (point-max) command nil)))
  (setq gnus-last-shell-command command))

(defun gnus-summary-pipe-to-muttprint (&optional command)
  "Pipe this article to muttprint."
  (setq command (read-string
		 "Print using command: " gnus-summary-muttprint-program
		 nil gnus-summary-muttprint-program))
  (gnus-summary-save-in-pipe command))

;;; Article file names when saving.

(defun gnus-capitalize-newsgroup (newsgroup)
  "Capitalize NEWSGROUP name."
  (when (not (zerop (length newsgroup)))
    (concat (char-to-string (upcase (aref newsgroup 0)))
	    (substring newsgroup 1))))

(defun gnus-Numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is ~/News/News.group/num.
Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if (gnus-use-long-file-name 'not-save)
		       (gnus-capitalize-newsgroup newsgroup)
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (mail-header-number headers)))
	   gnus-article-save-directory)))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-numeric-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group/num.	Otherwise, it is like ~/News/news/group/num."
  (let ((default
	  (expand-file-name
	   (concat (if (gnus-use-long-file-name 'not-save)
		       newsgroup
		     (gnus-newsgroup-directory-form newsgroup))
		   "/" (int-to-string (mail-header-number headers)))
	   gnus-article-save-directory)))
    (if (and last-file
	     (string-equal (file-name-directory default)
			   (file-name-directory last-file))
	     (string-match "^[0-9]+$" (file-name-nondirectory last-file)))
	default
      (or last-file default))))

(defun gnus-plain-save-name (newsgroup headers &optional last-file)
  "Generate file name from NEWSGROUP, HEADERS, and optional LAST-FILE.
If variable `gnus-use-long-file-name' is non-nil, it is
~/News/news.group.  Otherwise, it is like ~/News/news/group/news."
  (or last-file
      (expand-file-name
       (if (gnus-use-long-file-name 'not-save)
	   newsgroup
	 (file-relative-name
	  (expand-file-name "news" (gnus-newsgroup-directory-form newsgroup))
	  default-directory))
       gnus-article-save-directory)))

(defun gnus-sender-save-name (newsgroup headers &optional last-file)
  "Generate file name from sender."
  (let ((from (mail-header-from headers)))
    (expand-file-name
     (if (and from (string-match "\\([^ <]+\\)@" from))
	 (match-string 1 from)
       "nobody")
     gnus-article-save-directory)))

(defun article-verify-x-pgp-sig ()
  "Verify X-PGP-Sig."
  (interactive)
  (if (gnus-buffer-live-p gnus-original-article-buffer)
      (let ((sig (with-current-buffer gnus-original-article-buffer
		   (gnus-fetch-field "X-PGP-Sig")))
	    items info headers)
	(when (and sig
		   mml2015-use
		   (mml2015-clear-verify-function))
	  (with-temp-buffer
	    (insert-buffer gnus-original-article-buffer)
	    (setq items (split-string sig))
	    (message-narrow-to-head)
	    (let ((inhibit-point-motion-hooks t)
		  (case-fold-search t))
	      ;; Don't verify multiple headers.
	      (setq headers (mapconcat (lambda (header)
					 (concat header ": "
						 (mail-fetch-field header) "\n"))
				       (split-string (nth 1 items) ",") "")))
	    (delete-region (point-min) (point-max))
	    (insert "-----BEGIN PGP SIGNED MESSAGE-----\n\n")
	    (insert "X-Signed-Headers: " (nth 1 items) "\n")
	    (insert headers)
	    (widen)
	    (forward-line)
	    (while (not (eobp))
	      (if (looking-at "^-")
		  (insert "- "))
	      (forward-line))
	    (insert "\n-----BEGIN PGP SIGNATURE-----\n")
	    (insert "Version: " (car items) "\n\n")
	    (insert (mapconcat 'identity (cddr items) "\n"))
	    (insert "\n-----END PGP SIGNATURE-----\n")
	    (let ((mm-security-handle (list (format "multipart/signed"))))
	      (mml2015-clean-buffer)
	      (let ((coding-system-for-write (or gnus-newsgroup-charset
						 'iso-8859-1)))
		(funcall (mml2015-clear-verify-function)))
	      (setq info
		    (or (mm-handle-multipart-ctl-parameter
			 mm-security-handle 'gnus-details)
			(mm-handle-multipart-ctl-parameter
			 mm-security-handle 'gnus-info)))))
	  (when info
	    (let (buffer-read-only bface eface)
	      (save-restriction
		(message-narrow-to-head)
		(goto-char (point-max))
		(forward-line -1)
		(setq bface (get-text-property (gnus-point-at-bol) 'face)
		      eface (get-text-property (1- (gnus-point-at-eol)) 'face))
		(message-remove-header "X-Gnus-PGP-Verify")
		(if (re-search-forward "^X-PGP-Sig:" nil t)
		    (forward-line)
		  (goto-char (point-max)))
		(narrow-to-region (point) (point))
		(insert "X-Gnus-PGP-Verify: " info "\n")
		(goto-char (point-min))
		(forward-line)
		(while (not (eobp))
		  (if (not (looking-at "^[ \t]"))
		      (insert " "))
		  (forward-line))
		;; Do highlighting.
		(goto-char (point-min))
		(when (looking-at "\\([^:]+\\): *")
		  (put-text-property (match-beginning 1) (1+ (match-end 1))
				     'face bface)
		  (put-text-property (match-end 0) (point-max)
				     'face eface)))))))))

(defun article-verify-cancel-lock ()
  "Verify Cancel-Lock header."
  (interactive)
  (if (gnus-buffer-live-p gnus-original-article-buffer)
      (canlock-verify gnus-original-article-buffer)))

(defun article-monafy ()
  "Display body part with mona font."
  (interactive)
  (unless (if (featurep 'xemacs)
	      (find-face 'gnus-mona-face)
	    (facep 'gnus-mona-face))
    (require 'navi2ch-mona)
    (set-face-font (make-face 'gnus-mona-face) navi2ch-mona-font))
  (save-excursion
    (let ((buffer-read-only nil))
      (article-goto-body)
      (gnus-overlay-put
       (gnus-make-overlay (point) (point-max))
       'face 'gnus-mona-face))))

(eval-and-compile
  (mapcar
   (lambda (func)
     (let (afunc gfunc)
       (if (consp func)
	   (setq afunc (car func)
		 gfunc (cdr func))
	 (setq afunc func
	       gfunc (intern (format "gnus-%s" func))))
       (defalias gfunc
	 (when (fboundp afunc)
	   `(lambda (&optional interactive &rest args)
	      ,(documentation afunc t)
	      (interactive (list t))
	      (save-excursion
		(set-buffer gnus-article-buffer)
		(if interactive
		    (call-interactively ',afunc)
		  (apply ',afunc args))))))))
   '(article-hide-headers
     article-verify-x-pgp-sig
     article-verify-cancel-lock
     article-monafy
     article-hide-boring-headers
     article-toggle-headers
     article-treat-overstrike
     article-fill-long-lines
     article-capitalize-sentences
     article-remove-cr
     article-remove-leading-whitespace
     article-display-x-face
     article-de-quoted-unreadable
     article-de-base64-unreadable
     article-decode-HZ
     article-wash-html
     article-unsplit-urls
     article-hide-list-identifiers
     article-hide-pgp
     article-strip-banner
     article-babel
     article-hide-pem
     article-hide-signature
     article-strip-headers-in-body
     article-remove-trailing-blank-lines
     article-strip-leading-blank-lines
     article-strip-multiple-blank-lines
     article-strip-leading-space
     article-strip-trailing-space
     article-strip-blank-lines
     article-strip-all-blank-lines
     article-date-local
     article-date-english
     article-date-iso8601
     article-date-original
     article-date-ut
     article-decode-mime-words
     article-decode-charset
     article-decode-encoded-words
     article-date-user
     article-date-lapsed
     article-emphasize
     article-treat-dumbquotes
     article-normalize-headers
     (article-show-all-headers . gnus-article-show-all-headers)
;;      (article-show-all . gnus-article-show-all)
     )))

;;;
;;; Gnus article mode
;;;

(put 'gnus-article-mode 'mode-class 'special)

(gnus-define-keys gnus-article-mode-map
  " " gnus-article-goto-next-page
  "\177" gnus-article-goto-prev-page
  [delete] gnus-article-goto-prev-page
  [backspace] gnus-article-goto-prev-page
  "\C-c^" gnus-article-refer-article
  "h" gnus-article-show-summary
  "s" gnus-article-show-summary
  "\C-c\C-m" gnus-article-mail
  "?" gnus-article-describe-briefly
  "e" gnus-summary-edit-article
  "<" beginning-of-buffer
  ">" end-of-buffer
  "\C-c\C-i" gnus-info-find-node
  "\C-c\C-b" gnus-bug
  "R" gnus-article-reply-with-original
  "F" gnus-article-followup-with-original
  "\C-hk" gnus-article-describe-key
  "\C-hc" gnus-article-describe-key-briefly

  "\C-d" gnus-article-read-summary-keys
  "\M-*" gnus-article-read-summary-keys
  "\M-#" gnus-article-read-summary-keys
  "\M-^" gnus-article-read-summary-keys
  "\M-g" gnus-article-read-summary-keys)

;; Define almost undefined keys to `gnus-article-read-summary-keys'.
(let (keys)
  (let ((key 32))
    (while (<= key 127)
      (push (char-to-string key) keys)
      (incf key))
    (dolist (elem (accessible-keymaps gnus-summary-mode-map))
      (setq key (car elem))
      (when (and (> (length key) 0)
		 (not (eq 'menu-bar (aref key 0)))
		 (symbolp (lookup-key gnus-summary-mode-map key)))
	(push key keys))))
  (dolist (key keys)
    (unless (lookup-key gnus-article-mode-map key)
      (define-key gnus-article-mode-map key 'gnus-article-read-summary-keys))))

(defun gnus-article-make-menu-bar ()
  (unless (boundp 'gnus-article-commands-menu)
    (gnus-summary-make-menu-bar))
  (gnus-turn-off-edit-menu 'article)
  (unless (boundp 'gnus-article-article-menu)
    (easy-menu-define
     gnus-article-article-menu gnus-article-mode-map ""
     '("Article"
       ["Scroll forwards" gnus-article-goto-next-page t]
       ["Scroll backwards" gnus-article-goto-prev-page t]
       ["Show summary" gnus-article-show-summary t]
       ["Fetch Message-ID at point" gnus-article-refer-article t]
       ["Mail to address at point" gnus-article-mail t]
       ["Send a bug report" gnus-bug t]))

    (easy-menu-define
     gnus-article-treatment-menu gnus-article-mode-map ""
     ;; Fixme: this should use :active (and maybe :visible).
     '("Treatment"
       ["Hide headers" gnus-article-toggle-headers t]
       ["Hide signature" gnus-article-hide-signature t]
       ["Hide citation" gnus-article-hide-citation t]
       ["Treat overstrike" gnus-article-treat-overstrike t]
       ["Remove carriage return" gnus-article-remove-cr t]
       ["Remove leading whitespace" gnus-article-remove-leading-whitespace t]
       ["Treat html" gnus-article-wash-html t]
       ["Remove newlines from within URLs" gnus-article-unsplit-urls t]
       ["Decode HZ" gnus-article-decode-HZ t]))

    ;; Note "Commands" menu is defined in gnus-sum.el for consistency

    ;; Note "Post" menu is defined in gnus-sum.el for consistency

    (gnus-run-hooks 'gnus-article-menu-hook)))

;; Fixme: do something for the Emacs tool bar in Article mode a la
;; Summary.

(defun gnus-article-mode ()
  "Major mode for displaying an article.

All normal editing commands are switched off.

The following commands are available in addition to all summary mode
commands:
\\<gnus-article-mode-map>
\\[gnus-article-next-page]\t Scroll the article one page forwards
\\[gnus-article-prev-page]\t Scroll the article one page backwards
\\[gnus-article-refer-article]\t Go to the article referred to by an article id near point
\\[gnus-article-show-summary]\t Display the summary buffer
\\[gnus-article-mail]\t Send a reply to the address near point
\\[gnus-article-describe-briefly]\t Describe the current mode briefly
\\[gnus-info-find-node]\t Go to the Gnus info node"
  (interactive)
  (gnus-simplify-mode-line)
  (setq mode-name "Article")
  (setq major-mode 'gnus-article-mode)
  (make-local-variable 'minor-mode-alist)
  (unless (assq 'gnus-show-mime minor-mode-alist)
    (push (list 'gnus-show-mime " MIME") minor-mode-alist))
  (use-local-map gnus-article-mode-map)
  (when (gnus-visual-p 'article-menu 'menu)
    (gnus-article-make-menu-bar))
  (gnus-update-format-specifications nil 'article-mode)
  (set (make-local-variable 'page-delimiter) gnus-page-delimiter)
  (make-local-variable 'gnus-page-broken)
  (make-local-variable 'gnus-button-marker-list)
  (make-local-variable 'gnus-article-current-summary)
  (make-local-variable 'gnus-article-mime-handles)
  (make-local-variable 'gnus-article-decoded-p)
  (make-local-variable 'gnus-article-mime-handle-alist)
  (make-local-variable 'gnus-article-wash-types)
  (make-local-variable 'gnus-article-image-alist)
  (make-local-variable 'gnus-article-charset)
  (make-local-variable 'gnus-article-ignored-charsets)
  (gnus-set-default-directory)
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (set-syntax-table gnus-article-mode-syntax-table)
  (gnus-run-hooks 'gnus-article-mode-hook))

(defun gnus-article-setup-buffer ()
  "Initialize the article buffer."
  (let* ((name (if gnus-single-article-buffer "*Article*"
		 (concat "*Article " gnus-newsgroup-name "*")))
	 (original
	  (progn (string-match "\\*Article" name)
		 (concat " *Original Article"
			 (substring name (match-end 0))))))
    (setq gnus-article-buffer name)
    (setq gnus-original-article-buffer original)
    (setq gnus-article-mime-handle-alist nil)
    ;; This might be a variable local to the summary buffer.
    (unless gnus-single-article-buffer
      (save-excursion
	(set-buffer gnus-summary-buffer)
	(setq gnus-article-buffer name)
	(setq gnus-original-article-buffer original)
	(gnus-set-global-variables)))
    (gnus-article-setup-highlight-words)
    ;; Init original article buffer.
    (save-excursion
      (set-buffer (gnus-get-buffer-create gnus-original-article-buffer))
      (set-buffer-multibyte t)
      (setq major-mode 'gnus-original-article-mode)
      (make-local-variable 'gnus-original-article))
    (if (get-buffer name)
	(save-excursion
	  (set-buffer name)
	  (buffer-disable-undo)
	  (setq buffer-read-only t)
	  (unless (eq major-mode 'gnus-article-mode)
	    (gnus-article-mode))
	  (current-buffer))
      (save-excursion
	(set-buffer (gnus-get-buffer-create name))
	(gnus-article-mode)
	(make-local-variable 'gnus-summary-buffer)
	(gnus-summary-set-local-parameters gnus-newsgroup-name)
	(current-buffer)))))

;; Set article window start at LINE, where LINE is the number of lines
;; from the head of the article.
(defun gnus-article-set-window-start (&optional line)
  (set-window-start
   (gnus-get-buffer-window gnus-article-buffer t)
   (save-excursion
     (set-buffer gnus-article-buffer)
     (goto-char (point-min))
     (if (not line)
	 (point-min)
       (gnus-message 6 "Moved to bookmark")
       (search-forward "\n\n" nil t)
       (forward-line line)
       (point)))))

;;; @@ article filters
;;;

(defun gnus-article-display-mime-message ()
  "Article display method for MIME message."
  ;; called from `gnus-original-article-buffer'.
  (let (charset all-headers)
    (with-current-buffer gnus-summary-buffer
      (setq charset default-mime-charset
	    all-headers gnus-have-all-headers))
    (make-local-variable 'default-mime-charset)
    (setq default-mime-charset charset)
    (with-current-buffer (get-buffer-create gnus-article-buffer)
      (make-local-variable 'default-mime-charset)
      (setq default-mime-charset charset))
    (mime-display-message mime-message-structure
			  gnus-article-buffer nil gnus-article-mode-map)
    (when all-headers
      (gnus-article-hide-headers nil -1)))
  (run-hooks 'gnus-mime-article-prepare-hook))

(defun gnus-article-display-traditional-message ()
  "Article display method for traditional message."
  (set-buffer gnus-article-buffer)
  (let (buffer-read-only)
    (erase-buffer)
    (insert-buffer-substring gnus-original-article-buffer)))

(defun gnus-article-make-full-mail-header (&optional number charset)
  "Create a new mail header structure in a raw article buffer."
  (unless (and number charset)
    (save-current-buffer
      (set-buffer gnus-summary-buffer)
      (unless number
	(setq number (or (cdr gnus-article-current) 0)))
      (unless charset
	(setq charset (or default-mime-charset 'x-ctext)))))
  (goto-char (point-min))
  (let ((header-end (if (search-forward "\n\n" nil t)
			(1- (point))
		      (goto-char (point-max))))
	(chars (- (point-max) (point)))
	(lines (count-lines (point) (point-max)))
	(default-mime-charset charset)
	xref)
    (narrow-to-region (point-min) header-end)
    (setq xref (std11-fetch-field "xref"))
    (prog1
	(make-full-mail-header
	 number
	 (std11-fetch-field "subject")
	 (std11-fetch-field "from")
	 (std11-fetch-field "date")
	 (std11-fetch-field "message-id")
	 (std11-fetch-field "references")
	 chars
	 lines
	 (when xref (concat "Xref: " xref)))
      (widen))))

(defun gnus-article-prepare (article &optional all-headers header)
  "Prepare ARTICLE in article mode buffer.
ARTICLE should either be an article number or a Message-ID.
If ARTICLE is an id, HEADER should be the article headers.
If ALL-HEADERS is non-nil, no headers are hidden."
  (save-excursion
    ;; Make sure we start in a summary buffer.
    (unless (eq major-mode 'gnus-summary-mode)
      (set-buffer gnus-summary-buffer))
    (setq gnus-summary-buffer (current-buffer))
    (let* ((gnus-article (if header (mail-header-number header) article))
	   (summary-buffer (current-buffer))
	   (gnus-tmp-internal-hook gnus-article-internal-prepare-hook)
	   (group gnus-newsgroup-name)
	   result)
      (save-excursion
	(gnus-article-setup-buffer)
	(set-buffer gnus-article-buffer)
	;; Deactivate active regions.
	(when (and (boundp 'transient-mark-mode)
		   transient-mark-mode)
	  (setq mark-active nil))
	(if (not (setq result (let ((buffer-read-only nil))
				(gnus-request-article-this-buffer
				 article group))))
	    ;; There is no such article.
	    (save-excursion
	      (when (and (numberp article)
			 (not (memq article gnus-newsgroup-sparse)))
		(setq gnus-article-current
		      (cons gnus-newsgroup-name article))
		(set-buffer gnus-summary-buffer)
		(setq gnus-current-article article)
		(if (eq (gnus-article-mark article) gnus-undownloaded-mark)
		    (progn
		      (gnus-summary-set-agent-mark article)
		      (message "Message marked for downloading"))
		  (gnus-summary-mark-article article gnus-canceled-mark)
		  (unless (memq article gnus-newsgroup-sparse)
		    (gnus-error 1 "No such article (may have expired or been canceled)")))))
	  (if (or (eq result 'pseudo)
		  (eq result 'nneething))
	      (progn
		(save-excursion
		  (set-buffer summary-buffer)
		  (push article gnus-newsgroup-history)
		  (setq gnus-last-article gnus-current-article
			gnus-current-article 0
			gnus-current-headers nil
			gnus-article-current nil)
		  (if (eq result 'nneething)
		      (gnus-configure-windows 'summary)
		    (gnus-configure-windows 'article))
		  (gnus-set-global-variables))
		(let ((gnus-article-mime-handle-alist-1
		       gnus-article-mime-handle-alist))
		  (gnus-set-mode-line 'article)))
	    ;; The result from the `request' was an actual article -
	    ;; or at least some text that is now displayed in the
	    ;; article buffer.
	    (when (and (numberp article)
		       (not (eq article gnus-current-article)))
	      ;; Seems like a new article has been selected.
	      ;; `gnus-current-article' must be an article number.
	      (save-excursion
		(set-buffer summary-buffer)
		(push article gnus-newsgroup-history)
		(setq gnus-last-article gnus-current-article
		      gnus-current-article article
		      gnus-current-headers
		      (gnus-summary-article-header gnus-current-article)
		      gnus-article-current
		      (cons gnus-newsgroup-name gnus-current-article))
		(unless (vectorp gnus-current-headers)
		  (setq gnus-current-headers nil))
		(gnus-summary-goto-subject gnus-current-article)
		(when (gnus-summary-show-thread)
		  ;; If the summary buffer really was folded, the
		  ;; previous goto may not actually have gone to
		  ;; the right article, but the thread root instead.
		  ;; So we go again.
		  (gnus-summary-goto-subject gnus-current-article))
		(gnus-run-hooks 'gnus-mark-article-hook)
		(gnus-set-mode-line 'summary)
		(when (gnus-visual-p 'article-highlight 'highlight)
		  (gnus-run-hooks 'gnus-visual-mark-article-hook))
		;; Set the global newsgroup variables here.
		(gnus-set-global-variables)
		(setq gnus-have-all-headers
		      (or all-headers gnus-show-all-headers))))
	    (save-excursion
	      (gnus-configure-windows 'article))
	    (when (or (numberp article)
		      (stringp article))
	      (gnus-article-prepare-display)
	      ;; Do page break.
	      (goto-char (point-min))
	      (setq gnus-page-broken
		    (when gnus-break-pages
		      (gnus-narrow-to-page)
		      t)))
	    (let ((gnus-article-mime-handle-alist-1
		   gnus-article-mime-handle-alist))
	      (gnus-set-mode-line 'article))
	    (article-goto-body)
	    (set-window-point (get-buffer-window (current-buffer)) (point))
	    (gnus-configure-windows 'article)
	    t))))))

(defun gnus-article-prepare-mime-display (&optional number)
  (goto-char (point-min))
  (when (re-search-forward "^[^\t ]+:" nil t)
    (goto-char (match-beginning 0)))
  (let ((entity (if (eq 1 (point-min))
		    (get-text-property 1 'mime-view-entity)
		  (get-text-property (point) 'mime-view-entity)))
	last-entity child-entity next type)
    (setq child-entity (mime-entity-children entity))
    (if child-entity
	(setq last-entity (nth (1- (length child-entity))
			       child-entity))
      (setq last-entity entity))
    (save-restriction
      (narrow-to-region (point)
			(if (search-forward "\n\n" nil t)
			    (point)
			  (point-max)))
      (gnus-treat-article 'head)
      (put-text-property (point-min) (point-max) 'article-treated-header t)
      (goto-char (point-max)))
    (while (and (not (eobp)) entity)
      (setq next (set-marker
		  (make-marker)
		  (next-single-property-change (point) 'mime-view-entity
					       nil (point-max))))
      (let ((types (mime-entity-content-type entity)))
	(while (eq 'multipart (mime-content-type-primary-type types))
	  (setq entity (car (mime-entity-children entity))
		types (mime-entity-content-type entity)))
	(when types
	  (setq type (format "%s/%s"
			     (mime-content-type-primary-type types)
			     (mime-content-type-subtype types)))))
      (if (string-equal type "message/rfc822")
	  (progn
	    (setq next (point))
	    (let ((children (mime-entity-children entity))
		  last-children)
	      (when children
		(setq last-children (nth (1- (length children)) children))
		(while
		    (and
		     (not (eq last-children
			      (get-text-property next 'mime-view-entity)))
		     (setq next
			   (next-single-property-change next
							'mime-view-entity
							nil (point-max)))))))
	    (setq next (next-single-property-change next 'mime-view-entity
						    nil (point-max)))
	    (save-restriction
	      (narrow-to-region (point) next)
	      (gnus-article-prepare-mime-display)
	      (goto-char (point-max)))
	    (setq entity (get-text-property (point) 'mime-view-entity)))
	(save-restriction
	  (narrow-to-region (point) next)
	  ;; Kludge. We have to count true number, but for now,
	  ;; part number is here only to achieve `last'.
	  (gnus-treat-article nil 1
			      (if (eq entity last-entity)
				  1 2)
			      type)
	  (goto-char (point-max)))
	(setq entity (get-text-property next 'mime-view-entity))))))

;;;###autoload
(defun gnus-article-prepare-display ()
  "Make the current buffer look like a nice article."
  (let ((gnus-article-buffer (current-buffer))
	buffer-read-only)
    (unless (eq major-mode 'gnus-article-mode)
      (gnus-article-mode))
    (setq buffer-read-only nil
	  gnus-button-marker-list nil
	  gnus-article-wash-types nil
	  gnus-article-image-alist nil)
    (save-restriction
      (widen)
      (static-if (featurep 'xemacs)
	  (map-extents (lambda (extent maparg) (delete-extent extent)))
	(let ((lists (overlay-lists)))
	  (dolist (overlay (nconc (car lists) (cdr lists)))
	    (delete-overlay overlay)))))
    (gnus-run-hooks 'gnus-tmp-internal-hook))
  (set-buffer gnus-original-article-buffer)
  ;; Display message.
  (setq mime-message-structure gnus-current-headers)
  (mime-buffer-entity-set-buffer-internal mime-message-structure
					  gnus-original-article-buffer)
  (mime-entity-set-representation-type-internal mime-message-structure
						'mime-buffer-entity)
  (luna-send mime-message-structure 'initialize-instance
	     mime-message-structure)
  (if gnus-show-mime
      (let (mime-display-header-hook mime-display-text/plain-hook)
	(funcall gnus-article-display-method-for-mime))
    (funcall gnus-article-display-method-for-traditional))
  ;; Call the treatment functions.
  (let ((inhibit-read-only t))
    (save-restriction
      (widen)
      (if gnus-show-mime
	  (gnus-article-prepare-mime-display)
	(narrow-to-region (goto-char (point-min))
			  (if (search-forward "\n\n" nil t)
			      (point)
			    (point-max)))
	(gnus-treat-article 'head)
	(put-text-property (point-min) (point-max) 'article-treated-header t)
	(goto-char (point-max))
	(widen)
	(narrow-to-region (point) (point-max))
	(gnus-treat-article nil))
      (put-text-property (point-min) (point-max) 'read-only nil)))
  (gnus-run-hooks 'gnus-article-prepare-hook))

(defun gnus-article-decode-article-as-default-mime-charset ()
  "Decode an article as `default-mime-charset'.  It won't work if the
value of the variable `gnus-show-mime' is non-nil."
  (unless gnus-show-mime
    (set (make-local-variable 'default-mime-charset)
	 (with-current-buffer gnus-summary-buffer
	   default-mime-charset))
    (decode-mime-charset-region (point-min) (point-max)
				default-mime-charset)))

;;;
;;; Gnus MIME viewing functions
;;;

(defvar gnus-mime-button-line-format "%{%([%p. %d%T]%)%}%e\n"
  "Format of the MIME buttons.

Valid specifiers include:
%t  The MIME type
%T  MIME type, along with additional info
%n  The `name' parameter
%d  The description, if any
%l  The length of the encoded part
%p  The part identifier number
%e  Dots if the part isn't displayed

General format specifiers can also be used.  See
(gnus)Formatting Variables.")

(defvar gnus-mime-button-line-format-alist
  '((?t gnus-tmp-type ?s)
    (?T gnus-tmp-type-long ?s)
    (?n gnus-tmp-name ?s)
    (?d gnus-tmp-description ?s)
    (?p gnus-tmp-id ?s)
    (?l gnus-tmp-length ?d)
    (?e gnus-tmp-dots ?s)))

(defvar gnus-mime-button-commands
  '((gnus-article-press-button "\r" "Toggle Display")
    (gnus-mime-view-part "v" "View Interactively...")
    (gnus-mime-view-part-as-type "t" "View As Type...")
    (gnus-mime-view-part-as-charset "C" "View As charset...")
    (gnus-mime-save-part "o" "Save...")
    (gnus-mime-save-part-and-strip "\C-o" "Save and Strip")
    (gnus-mime-copy-part "c" "View As Text, In Other Buffer")
    (gnus-mime-inline-part "i" "View As Text, In This Buffer")
    (gnus-mime-view-part-internally "E" "View Internally")
    (gnus-mime-view-part-externally "e" "View Externally")
    (gnus-mime-print-part "p" "Print")
    (gnus-mime-pipe-part "|" "Pipe To Command...")
    (gnus-mime-action-on-part "." "Take action on the part")))

(defun gnus-article-mime-part-status ()
  (with-current-buffer gnus-article-buffer
    (let ((entity (get-text-property (point-min) 'mime-view-entity))
	  children)
      (if (and entity
	       (setq children (mime-entity-children entity))
	       (setq children (length children)))
	  (if (eq 1 children)
	      " (1 part)"
	    (format " (%d parts)" children))
	""))))

(defvar gnus-mime-button-map
  (let ((map (make-sparse-keymap)))
    (unless (>= (string-to-number emacs-version) 21)
      ;; XEmacs doesn't care.
      (set-keymap-parent map gnus-article-mode-map))
    (define-key map gnus-mouse-2 'gnus-article-push-button)
    (define-key map gnus-down-mouse-3 'gnus-mime-button-menu)
    (dolist (c gnus-mime-button-commands)
      (define-key map (cadr c) (car c)))
    map))

(defun gnus-mime-button-menu (event)
  "Construct a context-sensitive menu of MIME commands."
  (interactive "e")
  (save-window-excursion
    (let ((pos (event-start event)))
      (select-window (posn-window pos))
      (goto-char (posn-point pos))
      (gnus-article-check-buffer)
      (let ((response (x-popup-menu
		       t `("MIME Part"
			   ("" ,@(mapcar (lambda (c)
					   (cons (caddr c) (car c)))
					 gnus-mime-button-commands))))))
	(if response
	    (call-interactively response))))))

(defun gnus-mime-view-all-parts (&optional handles)
  "View all the MIME parts."
  (interactive)
  (save-current-buffer
    (set-buffer gnus-article-buffer)
    (let ((handles (or handles gnus-article-mime-handles))
	  (mail-parse-charset gnus-newsgroup-charset)
	  (mail-parse-ignored-charsets
	   (with-current-buffer gnus-summary-buffer
	     gnus-newsgroup-ignored-charsets)))
      (when handles
	(mm-remove-parts handles)
	(goto-char (point-min))
	(or (search-forward "\n\n") (goto-char (point-max)))
	(let (buffer-read-only)
	  (delete-region (point) (point-max))
	  (mm-display-parts handles))))))

(defun gnus-mime-save-part-and-strip ()
  "Save the MIME part under point then replace it with an external body."
  (interactive)
  (gnus-article-check-buffer)
  (let* ((data (get-text-property (point) 'gnus-data))
	 file param
	 (handles gnus-article-mime-handles))
    (if (mm-multiple-handles gnus-article-mime-handles)
	(error "This function is not implemented"))
    (setq file (and data (mm-save-part data)))
    (when file
      (with-current-buffer (mm-handle-buffer data)
	(erase-buffer)
	(insert "Content-Type: " (mm-handle-media-type data))
	(mml-insert-parameter-string (cdr (mm-handle-type data))
				     '(charset))
	(insert "\n")
	(insert "Content-ID: " (message-make-message-id) "\n")
	(insert "Content-Transfer-Encoding: binary\n")
	(insert "\n"))
      (setcdr data
	      (cdr (mm-make-handle nil
				   `("message/external-body"
				     (access-type . "LOCAL-FILE")
				     (name . ,file)))))
      (set-buffer gnus-summary-buffer)
      (gnus-article-edit-article
       `(lambda ()
	  (erase-buffer)
	  (let ((mail-parse-charset (or gnus-article-charset
					',gnus-newsgroup-charset))
		(mail-parse-ignored-charsets
		 (or gnus-article-ignored-charsets
		     ',gnus-newsgroup-ignored-charsets))
		(mbl mml-buffer-list))
	    (setq mml-buffer-list nil)
	    (insert-buffer gnus-original-article-buffer)
	    (mime-to-mml ',handles)
	    (setq gnus-article-mime-handles nil)
	    (let ((mbl1 mml-buffer-list))
	      (setq mml-buffer-list mbl)
	      (set (make-local-variable 'mml-buffer-list) mbl1))
	    ;; LOCAL argument of add-hook differs between GNU Emacs
	    ;; and XEmacs. make-local-hook makes sure they are local.
	    (make-local-hook 'kill-buffer-hook)
	    (add-hook 'kill-buffer-hook 'mml-destroy-buffers t t)))
       `(lambda (no-highlight)
	  (let ((mail-parse-charset (or gnus-article-charset
					',gnus-newsgroup-charset))
		(message-options message-options)
		(message-options-set-recipient)
		(mail-parse-ignored-charsets
		 (or gnus-article-ignored-charsets
		     ',gnus-newsgroup-ignored-charsets)))
	    (mml-to-mime)
	    (mml-destroy-buffers)
	    (remove-hook 'kill-buffer-hook
			 'mml-destroy-buffers t)
	    (kill-local-variable 'mml-buffer-list))
	  (gnus-summary-edit-article-done
	   ,(or (mail-header-references gnus-current-headers) "")
	   ,(gnus-group-read-only-p)
	   ,gnus-summary-buffer no-highlight))))))

(defun gnus-mime-save-part ()
  "Save the MIME part under point."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data)))
    (when data
      (mm-save-part data))))

(defun gnus-mime-pipe-part ()
  "Pipe the MIME part under point to a process."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data)))
    (when data
      (mm-pipe-part data))))

(defun gnus-mime-view-part ()
  "Interactively choose a viewing method for the MIME part under point."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data)))
    (when data
      (setq gnus-article-mime-handles
	    (mm-merge-handles
	     gnus-article-mime-handles (setq data (copy-sequence data))))
      (mm-interactively-view-part data))))

(defun gnus-mime-view-part-as-type-internal ()
  (gnus-article-check-buffer)
  (let* ((name (mail-content-type-get
		(mm-handle-type (get-text-property (point) 'gnus-data))
		'name))
	 (def-type (and name (mm-default-file-encoding name))))
    (and def-type (cons def-type 0))))

(defun gnus-mime-view-part-as-type (&optional mime-type)
  "Choose a MIME media type, and view the part as such."
  (interactive)
  (unless mime-type
    (setq mime-type (completing-read
		     "View as MIME type: "
		     (mapcar #'list (mailcap-mime-types))
		     nil nil
		     (gnus-mime-view-part-as-type-internal))))
  (gnus-article-check-buffer)
  (let ((handle (get-text-property (point) 'gnus-data)))
    (when handle
      (setq handle
	    (mm-make-handle (mm-handle-buffer handle)
			    (cons mime-type (cdr (mm-handle-type handle)))
			    (mm-handle-encoding handle)
			    (mm-handle-undisplayer handle)
			    (mm-handle-disposition handle)
			    (mm-handle-description handle)
			    nil
			    (mm-handle-id handle)))
      (setq gnus-article-mime-handles
	    (mm-merge-handles gnus-article-mime-handles handle))
      (gnus-mm-display-part handle))))

(defun gnus-mime-copy-part (&optional handle)
  "Put the MIME part under point into a new buffer."
  (interactive)
  (gnus-article-check-buffer)
  (let* ((handle (or handle (get-text-property (point) 'gnus-data)))
	 (contents (and handle (mm-get-part handle)))
	 (base (and handle
		    (file-name-nondirectory
		     (or
		      (mail-content-type-get (mm-handle-type handle) 'name)
		      (mail-content-type-get (mm-handle-type handle)
					     'filename)
		      "*decoded*"))))
	 (buffer (and base (generate-new-buffer base))))
    (when contents
      (switch-to-buffer buffer)
      (insert contents)
      ;; We do it this way to make `normal-mode' set the appropriate mode.
      (unwind-protect
	  (progn
	    (setq buffer-file-name (expand-file-name base))
	    (normal-mode))
	(setq buffer-file-name nil))
      (goto-char (point-min)))))

(defun gnus-mime-print-part (&optional handle filename)
  "Print the MIME part under point."
  (interactive (list nil (ps-print-preprint current-prefix-arg)))
  (gnus-article-check-buffer)
  (let* ((handle (or handle (get-text-property (point) 'gnus-data)))
	 (contents (and handle (mm-get-part handle)))
	 (file (make-temp-name (expand-file-name "mm." mm-tmp-directory)))
	 (printer (mailcap-mime-info (mm-handle-type handle) "print")))
    (when contents
	(if printer
	    (unwind-protect
		(progn
		  (with-temp-file file
		    (insert contents))
		  (call-process shell-file-name nil
				(generate-new-buffer " *mm*")
				nil
				shell-command-switch
				(mm-mailcap-command
				 printer file (mm-handle-type handle))))
	      (delete-file file))
	  (with-temp-buffer
	    (insert contents)
	    (gnus-print-buffer))
	  (ps-despool filename)))))

(defun gnus-mime-inline-part (&optional handle arg)
  "Insert the MIME part under point into the current buffer."
  (interactive (list nil current-prefix-arg))
  (gnus-article-check-buffer)
  (let* ((handle (or handle (get-text-property (point) 'gnus-data)))
	 contents charset
	 (b (point))
	 buffer-read-only)
    (when handle
      (if (and (not arg) (mm-handle-undisplayer handle))
	  (mm-remove-part handle)
	(setq contents (mm-get-part handle))
	(cond
	 ((not arg)
	  (setq charset (or (mail-content-type-get
			     (mm-handle-type handle) 'charset)
			    gnus-newsgroup-charset)))
	 ((numberp arg)
	  (if (mm-handle-undisplayer handle)
	      (mm-remove-part handle))
	  (setq charset
		(or (cdr (assq arg
			       gnus-summary-show-article-charset-alist))
		    (mm-read-coding-system "Charset: ")))))
	(forward-line 2)
	(mm-insert-inline handle
			  (if (and charset
				   (setq charset (mm-charset-to-coding-system
						  charset))
				   (not (eq charset 'ascii)))
			      (mm-decode-coding-string contents charset)
			    contents))
	(goto-char b)))))

(defun gnus-mime-view-part-as-charset (&optional handle arg)
  "Insert the MIME part under point into the current buffer using the
specified charset."
  (interactive (list nil current-prefix-arg))
  (gnus-article-check-buffer)
  (let* ((handle (or handle (get-text-property (point) 'gnus-data)))
	 contents charset
	 (b (point))
	 buffer-read-only)
    (when handle
      (if (mm-handle-undisplayer handle)
	  (mm-remove-part handle))
      (let ((gnus-newsgroup-charset
	     (or (cdr (assq arg
			    gnus-summary-show-article-charset-alist))
		 (mm-read-coding-system "Charset: ")))
	    (gnus-newsgroup-ignored-charsets 'gnus-all))
	(gnus-article-press-button)))))

(defun gnus-mime-view-part-externally (&optional handle)
  "View the MIME part under point with an external viewer."
  (interactive)
  (gnus-article-check-buffer)
  (let* ((handle (or handle (get-text-property (point) 'gnus-data)))
	 (mm-user-display-methods nil)
	 (mm-inlined-types nil)
	 (mail-parse-charset gnus-newsgroup-charset)
	 (mail-parse-ignored-charsets
	  (save-excursion (set-buffer gnus-summary-buffer)
			  gnus-newsgroup-ignored-charsets)))
    (when handle
      (if (mm-handle-undisplayer handle)
	  (mm-remove-part handle)
	(mm-display-part handle)))))

(defun gnus-mime-view-part-internally (&optional handle)
  "View the MIME part under point with an internal viewer.
If no internal viewer is available, use an external viewer."
  (interactive)
  (gnus-article-check-buffer)
  (let* ((handle (or handle (get-text-property (point) 'gnus-data)))
	 (mm-inlined-types '(".*"))
	 (mm-inline-large-images t)
	 (mail-parse-charset gnus-newsgroup-charset)
	 (mail-parse-ignored-charsets
	  (save-excursion (set-buffer gnus-summary-buffer)
			  gnus-newsgroup-ignored-charsets)))
    (when handle
      (if (mm-handle-undisplayer handle)
	  (mm-remove-part handle)
	(mm-display-part handle)))))

(defun gnus-mime-action-on-part (&optional action)
  "Do something with the MIME attachment at \(point\)."
  (interactive
   (list (completing-read "Action: " gnus-mime-action-alist)))
  (gnus-article-check-buffer)
  (let ((action-pair (assoc action gnus-mime-action-alist)))
    (if action-pair
	(funcall (cdr action-pair)))))

(defun gnus-article-part-wrapper (n function)
  (save-current-buffer
    (set-buffer gnus-article-buffer)
    (when (> n (length gnus-article-mime-handle-alist))
      (error "No such part"))
    (gnus-article-goto-part n)
    (let ((handle (cdr (assq n gnus-article-mime-handle-alist))))
      (funcall function handle))))

(defun gnus-article-pipe-part (n)
  "Pipe MIME part N, which is the numerical prefix."
  (interactive "p")
  (gnus-article-part-wrapper n 'mm-pipe-part))

(defun gnus-article-save-part (n)
  "Save MIME part N, which is the numerical prefix."
  (interactive "p")
  (gnus-article-part-wrapper n 'mm-save-part))

(defun gnus-article-interactively-view-part (n)
  "View MIME part N interactively, which is the numerical prefix."
  (interactive "p")
  (gnus-article-part-wrapper n 'mm-interactively-view-part))

(defun gnus-article-copy-part (n)
  "Copy MIME part N, which is the numerical prefix."
  (interactive "p")
  (gnus-article-part-wrapper n 'gnus-mime-copy-part))

(defun gnus-article-view-part-as-charset (n)
  "Copy MIME part N, which is the numerical prefix."
  (interactive "p")
  (gnus-article-part-wrapper n 'gnus-mime-view-part-as-charset))

(defun gnus-article-view-part-externally (n)
  "View MIME part N externally, which is the numerical prefix."
  (interactive "p")
  (gnus-article-part-wrapper n 'gnus-mime-view-part-externally))

(defun gnus-article-inline-part (n)
  "Inline MIME part N, which is the numerical prefix."
  (interactive "p")
  (gnus-article-part-wrapper n 'gnus-mime-inline-part))

(defun gnus-article-mime-match-handle-first (condition)
  (if condition
      (let ((alist gnus-article-mime-handle-alist) ihandle n)
	(while (setq ihandle (pop alist))
	  (if (and (cond
		    ((functionp condition)
		     (funcall condition (cdr ihandle)))
		    ((eq condition 'undisplayed)
		     (not (or (mm-handle-undisplayer (cdr ihandle))
			      (equal (mm-handle-media-type (cdr ihandle))
				     "multipart/alternative"))))
		    ((eq condition 'undisplayed-alternative)
		     (not (mm-handle-undisplayer (cdr ihandle))))
		    (t t))
		   (gnus-article-goto-part (car ihandle))
		   (or (not n) (< (car ihandle) n)))
	      (setq n (car ihandle))))
	(or n 1))
    1))

(defun gnus-article-view-part (&optional n)
  "View MIME part N, which is the numerical prefix."
  (interactive "P")
  (save-current-buffer
    (set-buffer gnus-article-buffer)
    (or (numberp n) (setq n (gnus-article-mime-match-handle-first
			     gnus-article-mime-match-handle-function)))
    (when (> n (length gnus-article-mime-handle-alist))
      (error "No such part"))
    (let ((handle (cdr (assq n gnus-article-mime-handle-alist))))
      (when (gnus-article-goto-part n)
	(if (equal (car handle) "multipart/alternative")
	    (gnus-article-press-button)
	  (when (eq (gnus-mm-display-part handle) 'internal)
	    (gnus-set-window-start)))))))

(defsubst gnus-article-mime-total-parts ()
  (if (bufferp (car gnus-article-mime-handles))
      1 ;; single part
    (1- (length gnus-article-mime-handles))))

(defun gnus-mm-display-part (handle)
  "Display HANDLE and fix MIME button."
  (let ((id (get-text-property (point) 'gnus-part))
	(point (point))
	buffer-read-only)
    (forward-line 1)
    (prog1
	(let ((window (selected-window))
	      (mail-parse-charset gnus-newsgroup-charset)
	      (mail-parse-ignored-charsets
	       (if (gnus-buffer-live-p gnus-summary-buffer)
		   (save-excursion
		     (set-buffer gnus-summary-buffer)
		     gnus-newsgroup-ignored-charsets)
		 nil)))
	  (save-excursion
	    (unwind-protect
		(let ((win (gnus-get-buffer-window (current-buffer) t))
		      (beg (point)))
		  (when win
		    (select-window win))
		  (goto-char point)
		  (forward-line)
		  (if (mm-handle-displayed-p handle)
		      ;; This will remove the part.
		      (mm-display-part handle)
		    (save-restriction
		      (narrow-to-region (point)
					(if (eobp) (point) (1+ (point))))
		      (mm-display-part handle)
		      ;; We narrow to the part itself and
		      ;; then call the treatment functions.
		      (goto-char (point-min))
		      (forward-line 1)
		      (narrow-to-region (point) (point-max))
		      (gnus-treat-article
		       nil id
		       (gnus-article-mime-total-parts)
		       (mm-handle-media-type handle)))))
	      (if (window-live-p window)
		  (select-window window)))))
      (goto-char point)
      (delete-region (gnus-point-at-bol) (progn (forward-line 1) (point)))
      (gnus-insert-mime-button
       handle id (list (mm-handle-displayed-p handle)))
      (goto-char point))))

(defun gnus-article-goto-part (n)
  "Go to MIME part N."
  (let ((point (text-property-any (point-min) (point-max) 'gnus-part n)))
    (when point
      (goto-char point))))

(defun gnus-insert-mime-button (handle gnus-tmp-id &optional displayed)
  (let ((gnus-tmp-name
	 (or (mail-content-type-get (mm-handle-type handle) 'name)
	     (mail-content-type-get (mm-handle-disposition handle) 'filename)
	     (mail-content-type-get (mm-handle-type handle) 'url)
	     ""))
	(gnus-tmp-type (mm-handle-media-type handle))
	(gnus-tmp-description
	 (mail-decode-encoded-word-string (or (mm-handle-description handle)
					      "")))
	(gnus-tmp-dots
	 (if (if displayed (car displayed)
	       (mm-handle-displayed-p handle))
	     "" "..."))
	(gnus-tmp-length (with-current-buffer (mm-handle-buffer handle)
			   (buffer-size)))
	gnus-tmp-type-long b e)
    (when (string-match ".*/" gnus-tmp-name)
      (setq gnus-tmp-name (replace-match "" t t gnus-tmp-name)))
    (setq gnus-tmp-type-long (concat gnus-tmp-type
				     (and (not (equal gnus-tmp-name ""))
					  (concat "; " gnus-tmp-name))))
    (unless (equal gnus-tmp-description "")
      (setq gnus-tmp-type-long (concat " --- " gnus-tmp-type-long)))
    (unless (bolp)
      (insert "\n"))
    (setq b (point))
    (gnus-eval-format
     gnus-mime-button-line-format gnus-mime-button-line-format-alist
     `(,@(gnus-local-map-property gnus-mime-button-map)
	 gnus-callback gnus-mm-display-part
	 gnus-part ,gnus-tmp-id
	 article-type annotation
	 gnus-data ,handle))
    (setq e (point))
    (widget-convert-button
     'link b e
     :mime-handle handle
     :action 'gnus-widget-press-button
     :button-keymap gnus-mime-button-map
     :help-echo
     (lambda (widget/window &optional overlay pos)
       ;; Needed to properly clear the message due to a bug in
       ;; wid-edit (XEmacs only).
       (if (boundp 'help-echo-owns-message)
	   (setq help-echo-owns-message t))
       (format
	"%S: %s the MIME part; %S: more options"
	(aref gnus-mouse-2 0)
	;; XEmacs will get a single widget arg; Emacs 21 will get
	;; window, overlay, position.
	(if (mm-handle-displayed-p
	     (if overlay
		 (with-current-buffer (gnus-overlay-buffer overlay)
		   (widget-get (widget-at (gnus-overlay-start overlay))
			       :mime-handle))
	       (widget-get widget/window :mime-handle)))
	    "hide" "show")
	(aref gnus-down-mouse-3 0))))))

(defun gnus-widget-press-button (elems el)
  (goto-char (widget-get elems :from))
  (gnus-article-press-button))

(defvar gnus-displaying-mime nil)

(defun gnus-display-mime (&optional ihandles)
  "Display the MIME parts."
  (save-excursion
    (save-selected-window
      (let ((window (get-buffer-window gnus-article-buffer))
	    (point (point)))
	(when window
	  (select-window window)
	  ;; We have to do this since selecting the window
	  ;; may change the point.  So we set the window point.
	  (set-window-point window point)))
      (let* ((handles (or ihandles (mm-dissect-buffer
				    nil gnus-article-loose-mime)
			  (mm-uu-dissect)))
	     buffer-read-only handle name type b e display)
	(when (and (not ihandles)
		   (not gnus-displaying-mime))
	  ;; Top-level call; we clean up.
	  (when gnus-article-mime-handles
	    (mm-destroy-parts gnus-article-mime-handles)
	    (setq gnus-article-mime-handle-alist nil));; A trick.
	  (setq gnus-article-mime-handles handles)
	  ;; We allow users to glean info from the handles.
	  (when gnus-article-mime-part-function
	    (gnus-mime-part-function handles)))
	(if (and handles
		 (or (not (stringp (car handles)))
		     (cdr handles)))
	    (progn
	      (when (and (not ihandles)
			 (not gnus-displaying-mime))
		;; Clean up for mime parts.
		(article-goto-body)
		(delete-region (point) (point-max)))
	      (let ((gnus-displaying-mime t))
		(gnus-mime-display-part handles)))
	  (save-restriction
	    (article-goto-body)
	    (narrow-to-region (point) (point-max))
	    (gnus-treat-article nil 1 1)
	    (widen)))
	(unless ihandles
	  ;; Highlight the headers.
	  (save-excursion
	    (save-restriction
	      (article-goto-body)
	      (narrow-to-region (point-min) (point))
	      (gnus-treat-article 'head))))))))

(defvar gnus-mime-display-multipart-as-mixed nil)
(defvar gnus-mime-display-multipart-alternative-as-mixed nil)
(defvar gnus-mime-display-multipart-related-as-mixed nil)

(defun gnus-mime-display-part (handle)
  (cond
   ;; Single part.
   ((not (stringp (car handle)))
    (gnus-mime-display-single handle))
   ;; User-defined multipart
   ((cdr (assoc (car handle) gnus-mime-multipart-functions))
    (funcall (cdr (assoc (car handle) gnus-mime-multipart-functions))
	     handle))
   ;; multipart/alternative
   ((and (equal (car handle) "multipart/alternative")
	 (not (or gnus-mime-display-multipart-as-mixed
		  gnus-mime-display-multipart-alternative-as-mixed)))
    (let ((id (1+ (length gnus-article-mime-handle-alist))))
      (push (cons id handle) gnus-article-mime-handle-alist)
      (gnus-mime-display-alternative (cdr handle) nil nil id)))
   ;; multipart/related
   ((and (equal (car handle) "multipart/related")
	 (not (or gnus-mime-display-multipart-as-mixed
		  gnus-mime-display-multipart-related-as-mixed)))
    ;;;!!!We should find the start part, but we just default
    ;;;!!!to the first part.
    ;;(gnus-mime-display-part (cadr handle))
    ;;;!!! Most multipart/related is an HTML message plus images.
    ;;;!!! Unfortunately we are unable to let W3 display those
    ;;;!!! included images, so we just display it as a mixed multipart.
    ;;(gnus-mime-display-mixed (cdr handle))
    ;;;!!! No, w3 can display everything just fine.
    (gnus-mime-display-part (cadr handle)))
   ((equal (car handle) "multipart/signed")
    (gnus-add-wash-type 'signed)
    (gnus-mime-display-security handle))
   ((equal (car handle) "multipart/encrypted")
    (gnus-add-wash-type 'encrypted)
    (gnus-mime-display-security handle))
   ;; Other multiparts are handled like multipart/mixed.
   (t
    (gnus-mime-display-mixed (cdr handle)))))

(defun gnus-mime-part-function (handles)
  (if (stringp (car handles))
      (mapcar 'gnus-mime-part-function (cdr handles))
    (funcall gnus-article-mime-part-function handles)))

(defun gnus-mime-display-mixed (handles)
  (mapcar 'gnus-mime-display-part handles))

(defun gnus-mime-display-single (handle)
  (let ((type (mm-handle-media-type handle))
	(ignored gnus-ignored-mime-types)
	(not-attachment t)
	(move nil)
	display text)
    (catch 'ignored
      (progn
	(while ignored
	  (when (string-match (pop ignored) type)
	    (throw 'ignored nil)))
	(if (and (setq not-attachment
		       (and (not (mm-inline-override-p handle))
			    (or (not (mm-handle-disposition handle))
				(equal (car (mm-handle-disposition handle))
				       "inline")
				(mm-attachment-override-p handle))))
		 (mm-automatic-display-p handle)
		 (or (and
		      (mm-inlinable-p handle)
		      (mm-inlined-p handle))
		     (mm-automatic-external-display-p type)))
	    (setq display t)
	  (when (equal (mm-handle-media-supertype handle) "text")
	    (setq text t)))
	(let ((id (1+ (length gnus-article-mime-handle-alist)))
	      beg)
	  (push (cons id handle) gnus-article-mime-handle-alist)
	  (when (or (not display)
		    (not (gnus-unbuttonized-mime-type-p type)))
	    ;(gnus-article-insert-newline)
	    (gnus-insert-mime-button
	     handle id (list (or display (and not-attachment text))))
	    (gnus-article-insert-newline)
	    ;(gnus-article-insert-newline)
	    ;; Remember modify the number of forward lines.
	    (setq move t))
	  (setq beg (point))
	  (cond
	   (display
	    (when move
	      (forward-line -1)
	      (setq beg (point)))
	    (let ((mail-parse-charset gnus-newsgroup-charset)
		  (mail-parse-ignored-charsets
		   (save-excursion (condition-case ()
				       (set-buffer gnus-summary-buffer)
				     (error))
				   gnus-newsgroup-ignored-charsets)))
	      (mm-display-part handle t))
	    (goto-char (point-max)))
	   ((and text not-attachment)
	    (when move
	      (forward-line -1)
	      (setq beg (point)))
	    (gnus-article-insert-newline)
	    (mm-insert-inline handle (mm-get-part handle))
	    (goto-char (point-max))))
	  ;; Do highlighting.
	  (save-excursion
	    (save-restriction
	      (narrow-to-region beg (point))
	      (gnus-treat-article
	       nil id
	       (gnus-article-mime-total-parts)
	       (mm-handle-media-type handle)))))))))

(defun gnus-unbuttonized-mime-type-p (type)
  "Say whether TYPE is to be unbuttonized."
  (unless gnus-inhibit-mime-unbuttonizing
    (when (catch 'found
	    (let ((types gnus-unbuttonized-mime-types))
	      (while types
		(when (string-match (pop types) type)
		  (throw 'found t)))))
      (not (catch 'found
	     (let ((types gnus-buttonized-mime-types))
	       (while types
		 (when (string-match (pop types) type)
		   (throw 'found t)))))))))

(defun gnus-article-insert-newline ()
  "Insert a newline, but mark it as undeletable."
  (gnus-put-text-property
   (point) (progn (insert "\n") (point)) 'gnus-undeletable t))

(defun gnus-mime-display-alternative (handles &optional preferred ibegend id)
  (let* ((preferred (or preferred (mm-preferred-alternative handles)))
	 (ihandles handles)
	 (point (point))
	 handle buffer-read-only from props begend not-pref)
    (save-window-excursion
      (save-restriction
	(when ibegend
	  (narrow-to-region (car ibegend)
			    (or (cdr ibegend)
				(progn
				  (goto-char (car ibegend))
				  (forward-line 2)
				  (point))))
	  (delete-region (point-min) (point-max))
	  (mm-remove-parts handles))
	(setq begend (list (point-marker)))
	;; Do the toggle.
	(unless (setq not-pref (cadr (member preferred ihandles)))
	  (setq not-pref (car ihandles)))
	(when (or ibegend
		  (not preferred)
		  (not (gnus-unbuttonized-mime-type-p
			"multipart/alternative")))
	  (gnus-add-text-properties
	   (setq from (point))
	   (progn
	     (insert (format "%d.  " id))
	     (point))
	   `(gnus-callback
	     (lambda (handles)
	       (unless ,(not ibegend)
		 (setq gnus-article-mime-handle-alist
		       ',gnus-article-mime-handle-alist))
	       (gnus-mime-display-alternative
		',ihandles ',not-pref ',begend ,id))
	     ,@(gnus-local-map-property gnus-mime-button-map)
	     ,gnus-mouse-face-prop ,gnus-article-mouse-face
	     face ,gnus-article-button-face
	     gnus-part ,id
	     gnus-data ,handle))
	  (widget-convert-button 'link from (point)
				 :action 'gnus-widget-press-button
				 :button-keymap gnus-widget-button-keymap)
	  ;; Do the handles
	  (while (setq handle (pop handles))
	    (gnus-add-text-properties
	     (setq from (point))
	     (progn
	       (insert (format "(%c) %-18s"
			       (if (equal handle preferred) ?* ? )
			       (mm-handle-media-type handle)))
	       (point))
	     `(gnus-callback
	       (lambda (handles)
		 (unless ,(not ibegend)
		   (setq gnus-article-mime-handle-alist
			 ',gnus-article-mime-handle-alist))
		 (gnus-mime-display-alternative
		  ',ihandles ',handle ',begend ,id))
	       ,@(gnus-local-map-property gnus-mime-button-map)
	       ,gnus-mouse-face-prop ,gnus-article-mouse-face
	       face ,gnus-article-button-face
	       gnus-part ,id
	       gnus-data ,handle))
	    (widget-convert-button 'link from (point)
				   :action 'gnus-widget-press-button
				   :button-keymap gnus-widget-button-keymap)
	    (insert "  "))
	  (insert "\n\n"))
	(when preferred
	  (if (stringp (car preferred))
	      (gnus-display-mime preferred)
	    (let ((mail-parse-charset gnus-newsgroup-charset)
		  (mail-parse-ignored-charsets
		   (save-excursion (set-buffer gnus-summary-buffer)
				   gnus-newsgroup-ignored-charsets)))
	      (mm-display-part preferred)
	      ;; Do highlighting.
	      (save-excursion
		(save-restriction
		  (narrow-to-region (car begend) (point-max))
		  (gnus-treat-article
		   nil (length gnus-article-mime-handle-alist)
		   (gnus-article-mime-total-parts)
		   (mm-handle-media-type handle))))))
	  (goto-char (point-max))
	  (setcdr begend (point-marker)))))
    (when ibegend
      (goto-char point))))

(defconst gnus-article-wash-status-strings
  (let ((alist '((cite "c" "Possible hidden citation text"
		       " " "All citation text visible")
		 (headers "h" "Hidden headers"
			  " " "All headers visible.")
		 (pgp "p" "Encrypted or signed message status hidden"
		      " " "No hidden encryption nor digital signature status")
		 (signature "s" "Signature has been hidden"
			    " " "Signature is visible")
		 (overstrike "o" "Overstrike (^H) characters applied"
			     " " "No overstrike characters applied")
		 (gnus-show-mime "m" "Mime processing is activated"
				 " " "Mime processing is not activated")
		 (emphasis "e" "/*_Emphasis_*/ characters applied"
			   " " "No /*_emphasis_*/ characters applied")))
	result)
    (dolist (entry alist result)
      (let ((key (nth 0 entry))
	    (on (copy-sequence (nth 1 entry)))
	    (on-help (nth 2 entry))
	    (off (copy-sequence (nth 3 entry)))
	    (off-help (nth 4 entry)))
	(put-text-property 0 1 'help-echo on-help on)
	(put-text-property 0 1 'help-echo off-help off)
	(push (list key on off) result))))
  "Alist of strings describing wash status in the mode line.
Each entry has the form (KEY ON OF), where the KEY is a symbol
representing the particular washing function, ON is the string to use
in the article mode line when the washing function is active, and OFF
is the string to use when it is inactive.")

(defun gnus-article-wash-status-entry (key value)
  (let ((entry (assoc key gnus-article-wash-status-strings)))
    (if value (nth 1 entry) (nth 2 entry))))

(defun gnus-article-wash-status ()
  "Return a string which display status of article washing."
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((cite (memq 'cite gnus-article-wash-types))
	  (headers (memq 'headers gnus-article-wash-types))
	  (boring (memq 'boring-headers gnus-article-wash-types))
	  (pgp (memq 'pgp gnus-article-wash-types))
	  (pem (memq 'pem gnus-article-wash-types))
	  (signed (memq 'signed gnus-article-wash-types))
	  (encrypted (memq 'encrypted gnus-article-wash-types))
	  (signature (memq 'signature gnus-article-wash-types))
	  (overstrike (memq 'overstrike gnus-article-wash-types))
	  (emphasis (memq 'emphasis gnus-article-wash-types)))
      (concat
       (gnus-article-wash-status-entry 'cite cite)
       (gnus-article-wash-status-entry 'headers (or headers boring))
       (gnus-article-wash-status-entry 'pgp (or pgp pem signed encrypted))
       (gnus-article-wash-status-entry 'signature signature)
       (gnus-article-wash-status-entry 'overstrike overstrike)
       (gnus-article-wash-status-entry 'gnus-show-mime gnus-show-mime)
       (gnus-article-wash-status-entry 'emphasis emphasis)))))

(defun gnus-add-wash-type (type)
  "Add a washing of TYPE to the current status."
  (add-to-list 'gnus-article-wash-types type))

(defun gnus-delete-wash-type (type)
  "Add a washing of TYPE to the current status."
  (setq gnus-article-wash-types (delq type gnus-article-wash-types)))

(defun gnus-add-image (category image)
  "Add IMAGE of CATEGORY to the list of displayed images."
  (let ((entry (assq category gnus-article-image-alist)))
    (unless entry
      (setq entry (list category))
      (push entry gnus-article-image-alist))
    (nconc entry (list image))))

(defun gnus-delete-images (category)
  "Delete all images in CATEGORY."
  (let ((entry (assq category gnus-article-image-alist)))
    (dolist (image (cdr entry))
      (gnus-remove-image image))
    (setq gnus-article-image-alist (delq entry gnus-article-image-alist))
    (gnus-delete-wash-type category)))

(defalias 'gnus-article-hide-headers-if-wanted 'gnus-article-maybe-hide-headers)

(defun gnus-article-maybe-hide-headers ()
  "Hide unwanted headers if `gnus-have-all-headers' is nil.
Provided for backwards compatibility."
  (when (and (or (not (gnus-buffer-live-p gnus-summary-buffer))
		 (not (save-excursion (set-buffer gnus-summary-buffer)
				      gnus-have-all-headers)))
	     (not gnus-inhibit-hiding))
    (gnus-article-hide-headers)))

;;; Article savers.

(defun gnus-output-to-file (file-name)
  "Append the current article to a file named FILE-NAME."
  (let ((artbuf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring artbuf)
      ;; Append newline at end of the buffer as separator, and then
      ;; save it to file.
      (goto-char (point-max))
      (insert "\n")
      (let ((file-name-coding-system nnmail-pathname-coding-system)
	    (pathname-coding-system nnmail-pathname-coding-system))
	(write-region-as-binary (point-min) (point-max) file-name 'append))
      t)))

(defun gnus-narrow-to-page (&optional arg)
  "Narrow the article buffer to a page.
If given a numerical ARG, move forward ARG pages."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (goto-char (point-min))
    (widen)
    ;; Remove any old next/prev buttons.
    (when (gnus-visual-p 'page-marker)
      (let ((buffer-read-only nil))
	(gnus-remove-text-with-property 'gnus-prev)
	(gnus-remove-text-with-property 'gnus-next)))
    (when
	(cond ((< arg 0)
	       (re-search-backward page-delimiter nil 'move (1+ (abs arg))))
	      ((> arg 0)
	       (re-search-forward page-delimiter nil 'move arg)))
      (goto-char (match-end 0)))
    (narrow-to-region
     (point)
     (if (re-search-forward page-delimiter nil 'move)
	 (match-beginning 0)
       (point)))
    (when (and (gnus-visual-p 'page-marker)
	       (not (= (point-min) 1)))
      (save-excursion
	(goto-char (point-min))
	(gnus-insert-prev-page-button)))
    (when (and (gnus-visual-p 'page-marker)
	       (< (+ (point-max) 2) (buffer-size)))
      (save-excursion
	(goto-char (point-max))
	(gnus-insert-next-page-button)))))

;; Article mode commands

(defun gnus-article-goto-next-page ()
  "Show the next page of the article."
  (interactive)
  (when (gnus-article-next-page)
    (goto-char (point-min))
    (gnus-article-read-summary-keys nil (gnus-character-to-event ?n))))

(defun gnus-article-goto-prev-page ()
  "Show the next page of the article."
  (interactive)
  (if (bobp) (gnus-article-read-summary-keys nil (gnus-character-to-event ?p))
    (gnus-article-prev-page nil)))

(defun gnus-article-next-page (&optional lines)
  "Show the next page of the current article.
If end of article, return non-nil.  Otherwise return nil.
Argument LINES specifies lines to be scrolled up."
  (interactive "p")
  (let ((start (window-start))
	end-of-buffer end-of-page)
    (save-excursion
      (move-to-window-line -1)
      (if (<= (point) start)
	  (progn
	    (forward-line 2)
	    (setq start (point)))
	(forward-line 1)
	(setq start nil))
      (unless (or (cond ((eq (1+ (buffer-size)) (point))
			 (and (pos-visible-in-window-p)
			      (setq end-of-buffer t)))
			((eobp)
			 (setq end-of-page t)))
		  (not lines))
	(move-to-window-line lines)
	(unless (search-backward "\n\n" nil t)
	  (setq start (point)))))
    (cond (end-of-buffer t)
	  (end-of-page
	   (gnus-narrow-to-page 1)
	   nil)
	  (t
	   (if start
	       (set-window-start (selected-window) start)
	     (let (window-pixel-scroll-increment)
	       (scroll-up lines)))
	   nil))))

(defun gnus-article-prev-page (&optional lines)
  "Show previous page of current article.
Argument LINES specifies lines to be scrolled down."
  (interactive "p")
  (let (beginning-of-buffer beginning-of-page)
    (save-excursion
      (move-to-window-line 0)
      (cond ((eq 1 (point))
	     (setq beginning-of-buffer t))
	    ((bobp)
	     (setq beginning-of-page t))))
    (cond (beginning-of-buffer)
	  (beginning-of-page
	   (gnus-narrow-to-page -1))
	  (t
	   (condition-case nil
	       (let (window-pixel-scroll-increment)
		 (scroll-down lines))
	     (beginning-of-buffer
	      (goto-char (point-min))))))))

(defun gnus-article-refer-article ()
  "Read article specified by message-id around point."
  (interactive)
  (let ((point (point)))
    (search-forward ">" nil t)		;Move point to end of "<....>".
    (if (re-search-backward "\\(<[^<> \t\n]+>\\)" nil t)
	(let ((message-id (match-string 1)))
	  (goto-char point)
	  (set-buffer gnus-summary-buffer)
	  (gnus-summary-refer-article message-id))
      (goto-char (point))
      (error "No references around point"))))

(defun gnus-article-show-summary ()
  "Reconfigure windows to show summary buffer."
  (interactive)
  (if (not (gnus-buffer-live-p gnus-summary-buffer))
      (error "There is no summary buffer for this article buffer")
    (gnus-article-set-globals)
    (gnus-configure-windows 'article)
    (gnus-summary-goto-subject gnus-current-article)
    (gnus-summary-position-point)))

(defun gnus-article-describe-briefly ()
  "Describe article mode commands briefly."
  (interactive)
  (gnus-message 6 (substitute-command-keys "\\<gnus-article-mode-map>\\[gnus-article-goto-next-page]:Next page	 \\[gnus-article-goto-prev-page]:Prev page  \\[gnus-article-show-summary]:Show summary  \\[gnus-info-find-node]:Run Info  \\[gnus-article-describe-briefly]:This help")))

(defun gnus-article-summary-command ()
  "Execute the last keystroke in the summary buffer."
  (interactive)
  (let ((obuf (current-buffer))
	(owin (current-window-configuration))
	func)
    (switch-to-buffer gnus-article-current-summary 'norecord)
    (setq func (lookup-key (current-local-map) (this-command-keys)))
    (call-interactively func)
    (set-buffer obuf)
    (set-window-configuration owin)
    (set-window-point (get-buffer-window (current-buffer)) (point))))

(defun gnus-article-summary-command-nosave ()
  "Execute the last keystroke in the summary buffer."
  (interactive)
  (let (func)
    (pop-to-buffer gnus-article-current-summary 'norecord)
    (setq func (lookup-key (current-local-map) (this-command-keys)))
    (call-interactively func)))

(defun gnus-article-check-buffer ()
  "Beep if not in an article buffer."
  (unless (eq (get-buffer gnus-article-buffer) (current-buffer))
    (error "Command invoked outside of a Gnus article buffer")))

(defun gnus-article-read-summary-keys (&optional arg key not-restore-window)
  "Read a summary buffer key sequence and execute it from the article buffer."
  (interactive "P")
  (gnus-article-check-buffer)
  (let ((nosaves
	 '("q" "Q"  "c" "r" "\C-c\C-f" "m"  "a" "f"
	   "Zc" "ZC" "ZE" "ZJ" "ZQ" "ZZ" "Zn" "ZR" "ZG" "ZN" "ZP"
	   "=" "^" "\M-^" "|"))
	(nosave-but-article
	 '("A\r"))
	(nosave-in-article
	 '("\C-d"))
	(up-to-top
	 '("n" "Gn" "p" "Gp"))
	keys new-sum-point)
    (save-excursion
      (set-buffer gnus-article-current-summary)
      (let (gnus-pick-mode)
	(push (or key last-command-event) unread-command-events)
	(setq keys (static-if (featurep 'xemacs)
		       (events-to-keys (read-key-sequence nil))
		     (read-key-sequence nil)))))
    (message "")

    (if (or (member keys nosaves)
	    (member keys nosave-but-article)
	    (member keys nosave-in-article))
	(let (func)
	  (save-window-excursion
	    (pop-to-buffer gnus-article-current-summary 'norecord)
	    ;; We disable the pick minor mode commands.
	    (let (gnus-pick-mode)
	      (setq func (lookup-key (current-local-map) keys))))
	  (if (or (not func)
		  (numberp func))
	      (ding)
	    (unless (member keys nosave-in-article)
	      (set-buffer gnus-article-current-summary))
	    (call-interactively func)
	    (setq new-sum-point (point)))
	  (when (member keys nosave-but-article)
	    (pop-to-buffer gnus-article-buffer 'norecord)))
      ;; These commands should restore window configuration.
      (let ((obuf (current-buffer))
	    (owin (current-window-configuration))
	    (opoint (point))
	    (summary gnus-article-current-summary)
	    func in-buffer selected)
	(if not-restore-window
	    (pop-to-buffer summary 'norecord)
	  (switch-to-buffer summary 'norecord))
	(setq in-buffer (current-buffer))
	;; We disable the pick minor mode commands.
	(if (and (setq func (let (gnus-pick-mode)
			      (lookup-key (current-local-map) keys)))
		 (functionp func))
	    (progn
	      (call-interactively func)
	      (setq new-sum-point (point))
	      (when (eq in-buffer (current-buffer))
		(setq selected (gnus-summary-select-article))
		(set-buffer obuf)
		(unless not-restore-window
		  (set-window-configuration owin))
		(when (eq selected 'old)
		  (article-goto-body)
		  (set-window-start (get-buffer-window (current-buffer))
				    1)
		  (set-window-point (get-buffer-window (current-buffer))
				    (point)))
		(let ((win (get-buffer-window gnus-article-current-summary)))
		  (when win
		    (set-window-point win new-sum-point))))    )
	  (switch-to-buffer gnus-article-buffer)
	  (ding))))))

(defun gnus-article-describe-key (key)
  "Display documentation of the function invoked by KEY.  KEY is a string."
  (interactive "kDescribe key: ")
  (gnus-article-check-buffer)
  (if (eq (key-binding key) 'gnus-article-read-summary-keys)
      (save-excursion
	(set-buffer gnus-article-current-summary)
	(let (gnus-pick-mode)
	  (if (featurep 'xemacs)
	      (progn
		(push (elt key 0) unread-command-events)
		(setq key (events-to-keys
			   (read-key-sequence "Describe key: "))))
	    (setq unread-command-events
		  (mapcar
		   (lambda (x) (if (>= x 128) (list 'meta (- x 128)) x))
		   key))
	    (setq key (read-key-sequence "Describe key: "))))
	(describe-key key))
    (describe-key key)))

(defun gnus-article-describe-key-briefly (key &optional insert)
  "Display documentation of the function invoked by KEY.  KEY is a string."
  (interactive "kDescribe key: \nP")
  (gnus-article-check-buffer)
  (if (eq (key-binding key) 'gnus-article-read-summary-keys)
      (save-excursion
	(set-buffer gnus-article-current-summary)
	(let (gnus-pick-mode)
	  (if (featurep 'xemacs)
	      (progn
		(push (elt key 0) unread-command-events)
		(setq key (events-to-keys
			   (read-key-sequence "Describe key: "))))
	    (setq unread-command-events
		  (mapcar
		   (lambda (x) (if (>= x 128) (list 'meta (- x 128)) x))
		   key))
	    (setq key (read-key-sequence "Describe key: "))))
	(describe-key-briefly key insert))
    (describe-key-briefly key insert)))

(defun gnus-article-reply-with-original (&optional wide)
  "Start composing a reply mail to the current message.
The text in the region will be yanked.  If the region isn't active,
the entire article will be yanked."
  (interactive "P")
  (let ((article (cdr gnus-article-current)) cont)
    (if (not (mark t))
	(gnus-summary-reply (list (list article)) wide)
      (setq cont (buffer-substring (point) (mark t)))
      ;; Deactivate active regions.
      (when (and (boundp 'transient-mark-mode)
		 transient-mark-mode)
	(setq mark-active nil))
      (gnus-summary-reply
       (list (list article cont)) wide))))

(defun gnus-article-followup-with-original ()
  "Compose a followup to the current article.
The text in the region will be yanked.  If the region isn't active,
the entire article will be yanked."
  (interactive)
  (let ((article (cdr gnus-article-current))
	cont)
    (if (not (mark t))
	(gnus-summary-followup (list (list article)))
      (setq cont (buffer-substring (point) (mark t)))
      ;; Deactivate active regions.
      (when (and (boundp 'transient-mark-mode)
		 transient-mark-mode)
	(setq mark-active nil))
      (gnus-summary-followup
       (list (list article cont))))))

(defun gnus-article-hide (&optional arg force)
  "Hide all the gruft in the current article.
This means that PGP stuff, signatures, cited text and (some)
headers will be hidden.
If given a prefix, show the hidden text instead."
  (interactive (append (gnus-article-hidden-arg) (list 'force)))
  (gnus-article-hide-headers arg)
  (gnus-article-hide-list-identifiers arg)
  (gnus-article-hide-pgp arg)
  (gnus-article-hide-citation-maybe arg force)
  (gnus-article-hide-signature arg))

(defun gnus-article-maybe-highlight ()
  "Do some article highlighting if article highlighting is requested."
  (when (gnus-visual-p 'article-highlight 'highlight)
    (gnus-article-highlight-some)))

(defun gnus-check-group-server ()
  ;; Make sure the connection to the server is alive.
  (unless (gnus-server-opened
	   (gnus-find-method-for-group gnus-newsgroup-name))
    (gnus-check-server (gnus-find-method-for-group gnus-newsgroup-name))
    (gnus-request-group gnus-newsgroup-name t)))

(eval-when-compile
  (autoload 'nneething-get-file-name "nneething"))

(defun gnus-request-article-this-buffer (article group)
  "Get an article and insert it into this buffer.
T-gnus change: Insert an article into `gnus-original-article-buffer'."
  (let (do-update-line sparse-header)
    ;; The current buffer is `gnus-article-buffer'.
    (prog1
	(save-excursion
	  (erase-buffer)
	  (gnus-kill-all-overlays)
	  (setq group (or group gnus-newsgroup-name))

	  ;; Using `gnus-request-article' directly will insert the article into
	  ;; `nntp-server-buffer' - so we'll save some time by not having to
	  ;; copy it from the server buffer into the article buffer.

	  ;; We only request an article by message-id when we do not have the
	  ;; headers for it, so we'll have to get those.
	  (when (stringp article)
	    (gnus-read-header article))

	  ;; If the article number is negative, that means that this article
	  ;; doesn't belong in this newsgroup (possibly), so we find its
	  ;; message-id and request it by id instead of number.
	  (when (and (numberp article)
		     gnus-summary-buffer
		     (get-buffer gnus-summary-buffer)
		     (gnus-buffer-exists-p gnus-summary-buffer))
	    (save-excursion
	      (set-buffer gnus-summary-buffer)
	      (let ((header (gnus-summary-article-header article)))
		(when (< article 0)
		  (cond
		   ((memq article gnus-newsgroup-sparse)
		    ;; This is a sparse gap article.
		    (setq do-update-line article)
		    (setq article (mail-header-id header))
		    (setq sparse-header (gnus-read-header article))
		    (setq gnus-newsgroup-sparse
			  (delq article gnus-newsgroup-sparse)))
		   ((vectorp header)
		    ;; It's a real article.
		    (setq article (mail-header-id header)))
		   (t
		    ;; It is an extracted pseudo-article.
		    (setq article 'pseudo)
		    (gnus-request-pseudo-article header))))

		(let ((method (gnus-find-method-for-group
			       gnus-newsgroup-name)))
		  (when (and (eq (car method) 'nneething)
			     (vectorp header))
		    (let ((dir (nneething-get-file-name
				(mail-header-id header))))
		      (when (and (stringp dir)
				 (file-directory-p dir))
			(setq article 'nneething)
			(gnus-group-enter-directory dir))))))))
	  (setq gnus-original-article (cons group article))

	  ;; The current buffer is `gnus-original-article-buffer'.
	  (if (get-buffer gnus-original-article-buffer)
	      (set-buffer gnus-original-article-buffer)
	    (set-buffer (gnus-get-buffer-create gnus-original-article-buffer))
	    (set-buffer-multibyte nil)
	    (buffer-disable-undo)
	    (setq major-mode 'gnus-original-article-mode)
	    (setq buffer-read-only nil))

	  (cond
	   ;; Refuse to select canceled articles.
	   ((and (numberp article)
		 gnus-summary-buffer
		 (get-buffer gnus-summary-buffer)
		 (gnus-buffer-exists-p gnus-summary-buffer)
		 (eq (cdr (save-excursion
			    (set-buffer gnus-summary-buffer)
			    (assq article gnus-newsgroup-reads)))
		     gnus-canceled-mark))
	    nil)
	   ;; Check the backlog.
	   ((and gnus-keep-backlog
		 (gnus-backlog-request-article group article (current-buffer)))
	    'article)
	   ;; Check asynchronous pre-fetch.
	   ((gnus-async-request-fetched-article group article (current-buffer))
	    (gnus-async-prefetch-next group article gnus-summary-buffer)
	    (when (and (numberp article) gnus-keep-backlog)
	      (gnus-backlog-enter-article group article (current-buffer)))
	    'article)
	   ;; Check the cache.
	   ((and gnus-use-cache
		 (numberp article)
		 (gnus-cache-request-article article group))
	    'article)
	   ;; Check the agent cache.
	   ((and gnus-agent gnus-agent-cache gnus-plugged
		 (numberp article)
		 (gnus-agent-request-article article group))
	    'article)
	   ;; Get the article and put into the article buffer.
	   ((or (stringp article)
		(numberp article))
	    (let ((gnus-override-method gnus-override-method)
		  (methods (and (stringp article)
				gnus-refer-article-method))
		  result
		  (buffer-read-only nil))
	      (if (or (not (listp methods))
		      (and (symbolp (car methods))
			   (assq (car methods) nnoo-definition-alist)))
		  (setq methods (list methods)))
	      (when (and (null gnus-override-method)
			 methods)
		(setq gnus-override-method (pop methods)))
	      (while (not result)
		(when (eq gnus-override-method 'current)
		  (setq gnus-override-method
			(with-current-buffer gnus-summary-buffer
			  gnus-current-select-method)))
		(erase-buffer)
		(gnus-kill-all-overlays)
		(let ((gnus-newsgroup-name group))
		  (gnus-check-group-server))
		(when (gnus-request-article article group (current-buffer))
		  (when (numberp article)
		    (gnus-async-prefetch-next group article
					      gnus-summary-buffer)
		    (when gnus-keep-backlog
		      (gnus-backlog-enter-article
		       group article (current-buffer))))
		  (setq result 'article))
		(if (not result)
		    (if methods
			(setq gnus-override-method (pop methods))
		      (setq result 'done))))
	      (and (eq result 'article) 'article)))
	   ;; It was a pseudo.
	   (t article)))

      ;; Associate this article with the current summary buffer.
      (setq gnus-article-current-summary gnus-summary-buffer)

      ;; Copy the requested article from `gnus-original-article-buffer'.
      (unless (equal (buffer-name (current-buffer))
		     (buffer-name (get-buffer gnus-original-article-buffer)))
	(insert-buffer gnus-original-article-buffer))

      ;; Decode charsets.
      (run-hooks 'gnus-article-decode-hook)
      ;; Mark article as decoded or not.
      (setq gnus-article-decoded-p gnus-article-decode-hook)

      ;; Update sparse articles.
      (when (and do-update-line
		 (or (numberp article)
		     (stringp article)))
	(let ((buf (current-buffer)))
	  (set-buffer gnus-summary-buffer)
	  (gnus-summary-update-article do-update-line sparse-header)
	  (gnus-summary-goto-subject do-update-line nil t)
	  (set-window-point (gnus-get-buffer-window (current-buffer) t)
			    (point))
	  (set-buffer buf))))))

;;;
;;; Article editing
;;;

(defcustom gnus-article-edit-mode-hook nil
  "Hook run in article edit mode buffers."
  :group 'gnus-article-various
  :type 'hook)

(defcustom gnus-article-edit-article-setup-function
  'gnus-article-mime-edit-article-setup
  "Function called to setup an editing article buffer."
  :group 'gnus-article-various
  :type 'function)

(defvar gnus-article-edit-done-function nil)

(defvar gnus-article-edit-mode-map nil)

;; Should we be using derived.el for this?
(unless gnus-article-edit-mode-map
  (setq gnus-article-edit-mode-map (make-keymap))
  (set-keymap-parent gnus-article-edit-mode-map text-mode-map)

  (gnus-define-keys gnus-article-edit-mode-map
    "\C-c?"    describe-mode
    "\C-c\C-c" gnus-article-edit-done
    "\C-c\C-k" gnus-article-edit-exit
    "\C-c\C-f\C-t" message-goto-to
    "\C-c\C-f\C-o" message-goto-from
    "\C-c\C-f\C-b" message-goto-bcc
    ;;"\C-c\C-f\C-w" message-goto-fcc
    "\C-c\C-f\C-c" message-goto-cc
    "\C-c\C-f\C-s" message-goto-subject
    "\C-c\C-f\C-r" message-goto-reply-to
    "\C-c\C-f\C-n" message-goto-newsgroups
    "\C-c\C-f\C-d" message-goto-distribution
    "\C-c\C-f\C-f" message-goto-followup-to
    "\C-c\C-f\C-m" message-goto-mail-followup-to
    "\C-c\C-f\C-k" message-goto-keywords
    "\C-c\C-f\C-u" message-goto-summary
    "\C-c\C-f\C-i" message-insert-or-toggle-importance
    "\C-c\C-f\C-a" message-gen-unsubscribed-mft
    "\C-c\C-b" message-goto-body
    "\C-c\C-i" message-goto-signature

    "\C-c\C-t" message-insert-to
    "\C-c\C-n" message-insert-newsgroups
    "\C-c\C-o" message-sort-headers
    "\C-c\C-e" message-elide-region
    "\C-c\C-v" message-delete-not-region
    "\C-c\C-z" message-kill-to-signature
    "\M-\r" message-newline-and-reformat
    "\C-c\C-a" mml-attach-file
    "\C-a" message-beginning-of-line
    "\t" message-tab
    "\M-;" comment-region)

  (gnus-define-keys (gnus-article-edit-wash-map
		     "\C-c\C-w" gnus-article-edit-mode-map)
    "f" gnus-article-edit-full-stops))

(easy-menu-define
  gnus-article-edit-mode-field-menu gnus-article-edit-mode-map ""
  '("Field"
    ["Fetch To" message-insert-to t]
    ["Fetch Newsgroups" message-insert-newsgroups t]
    "----"
    ["To" message-goto-to t]
    ["From" message-goto-from t]
    ["Subject" message-goto-subject t]
    ["Cc" message-goto-cc t]
    ["Reply-To" message-goto-reply-to t]
    ["Summary" message-goto-summary t]
    ["Keywords" message-goto-keywords t]
    ["Newsgroups" message-goto-newsgroups t]
    ["Followup-To" message-goto-followup-to t]
    ["Mail-Followup-To" message-goto-mail-followup-to t]
    ["Distribution" message-goto-distribution t]
    ["Body" message-goto-body t]
    ["Signature" message-goto-signature t]))

(define-derived-mode gnus-article-edit-mode text-mode "Article Edit"
  "Major mode for editing articles.
This is an extended text-mode.

\\{gnus-article-edit-mode-map}"
  (make-local-variable 'gnus-article-edit-done-function)
  (make-local-variable 'gnus-prev-winconf)
  (set (make-local-variable 'font-lock-defaults)
       '(message-font-lock-keywords t))
  (set (make-local-variable 'mail-header-separator) "")
  (easy-menu-add message-mode-field-menu message-mode-map)
  (mml-mode)
  (setq buffer-read-only nil)
  (buffer-enable-undo)
  (widen))

(defun gnus-article-edit (&optional force)
  "Edit the current article.
This will have permanent effect only in mail groups.
If FORCE is non-nil, allow editing of articles even in read-only
groups."
  (interactive "P")
  (when (and (not force)
	     (gnus-group-read-only-p))
    (error "The current newsgroup does not support article editing"))
  (gnus-article-date-original)
  (gnus-article-edit-article
   'ignore
   `(lambda (no-highlight)
      'ignore
      (gnus-summary-edit-article-done
       ,(or (mail-header-references gnus-current-headers) "")
       ,(gnus-group-read-only-p) ,gnus-summary-buffer no-highlight))))

(defun gnus-article-edit-article (start-func exit-func)
  "Start editing the contents of the current article buffer."
  (let ((winconf (current-window-configuration)))
    (set-buffer gnus-article-buffer)
    (gnus-article-edit-mode)
    (funcall start-func)
    (set-buffer-modified-p nil)
    (gnus-configure-windows 'edit-article)
    (setq gnus-article-edit-done-function exit-func)
    (setq gnus-prev-winconf winconf)
    (when gnus-article-edit-article-setup-function
      (funcall gnus-article-edit-article-setup-function))
    (gnus-message 6 "C-c C-c to end edits; C-c C-k to exit")))

(defun gnus-article-edit-done (&optional arg)
  "Update the article edits and exit."
  (interactive "P")
  (let ((func gnus-article-edit-done-function)
	(buf (current-buffer))
	(start (window-start))
	(p (point))
	(winconf gnus-prev-winconf))
    (remove-hook 'gnus-article-mode-hook
		 'gnus-article-mime-edit-article-unwind)
    (funcall func arg)
    (set-buffer buf)
    ;; The cache and backlog have to be flushed somewhat.
    (when gnus-keep-backlog
      (gnus-backlog-remove-article
       (car gnus-article-current) (cdr gnus-article-current)))
    ;; Flush original article as well.
    (save-excursion
      (when (get-buffer gnus-original-article-buffer)
	(set-buffer gnus-original-article-buffer)
	(setq gnus-original-article nil)))
    (when gnus-use-cache
      (gnus-cache-update-article
       (car gnus-article-current) (cdr gnus-article-current)))
    ;; We remove all text props from the article buffer.
    (kill-all-local-variables)
    (gnus-set-text-properties (point-min) (point-max) nil)
    (gnus-article-mode)
    (set-window-configuration winconf)
    (set-buffer buf)
    (set-window-start (get-buffer-window buf) start)
    (set-window-point (get-buffer-window buf) (point))))

(defun gnus-article-edit-exit ()
  "Exit the article editing without updating."
  (interactive)
  (when (or (not (buffer-modified-p))
	    (yes-or-no-p "Article modified; kill anyway? "))
    (let ((curbuf (current-buffer))
	  (p (point))
	  (window-start (window-start)))
      (erase-buffer)
      (if (gnus-buffer-live-p gnus-original-article-buffer)
	  (insert-buffer gnus-original-article-buffer))
      (let ((winconf gnus-prev-winconf))
	(gnus-article-mode)
	(set-window-configuration winconf)
	;; Tippy-toe some to make sure that point remains where it was.
	(save-current-buffer
	  (set-buffer curbuf)
	  (set-window-start (get-buffer-window (current-buffer)) window-start)
	  (goto-char p))))))

(defun gnus-article-edit-full-stops ()
  "Interactively repair spacing at end of sentences."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^$" nil t)
    (let ((case-fold-search nil))
      (query-replace-regexp "\\([.!?][])}]* \\)\\([[({A-Z]\\)" "\\1 \\2"))))

;;;
;;; Article editing with MIME-Edit
;;;

(defcustom gnus-article-mime-edit-article-setup-hook nil
  "Hook run after setting up a MIME editing article buffer."
  :group 'gnus-article-various
  :type 'hook)

(defun gnus-article-mime-edit-article-unwind ()
  "Unwind `gnus-article-buffer' if article editing was given up."
  (remove-hook 'gnus-article-mode-hook 'gnus-article-mime-edit-article-unwind)
  (when (featurep 'font-lock)
    (setq font-lock-defaults nil)
    (font-lock-mode -1))
  (when mime-edit-mode-flag
    (mime-edit-exit 'nomime 'no-error)
    (message "")))

(defun gnus-article-mime-edit-article-setup ()
  "Convert current buffer to MIME-Edit buffer and turn on MIME-Edit mode
after replacing with the original article."
  (setq gnus-show-mime t)
  (setq gnus-article-edit-done-function
	`(lambda (&rest args)
	   (when mime-edit-mode-flag
	     (let (mime-edit-insert-user-agent-field)
	       (mime-edit-exit))
	     (message ""))
	   (goto-char (point-min))
	   (let (case-fold-search)
	     (when (re-search-forward
		    (format "^%s$" (regexp-quote mail-header-separator))
		    nil t)
	       (replace-match "")))
	   (apply ,gnus-article-edit-done-function args)
	   (insert
	    (prog1
		(buffer-substring-no-properties (point-min) (point-max))
	      (set-buffer (get-buffer-create gnus-original-article-buffer))
	      (erase-buffer)))
	   (setq gnus-current-headers (gnus-article-make-full-mail-header))
	   (set-buffer gnus-article-buffer)
	   (gnus-article-prepare-display)))
  (substitute-key-definition 'gnus-article-edit-done
			     'gnus-article-mime-edit-done
			     gnus-article-edit-mode-map)
  (substitute-key-definition 'gnus-article-edit-exit
			     'gnus-article-mime-edit-exit
			     gnus-article-edit-mode-map)
  (erase-buffer)
  (insert-buffer gnus-original-article-buffer)
  (let ((ofn (symbol-function 'mime-edit-decode-single-part-in-buffer)))
    (fset 'mime-edit-decode-single-part-in-buffer
	  (lambda (&rest args)
	    (if (let ((content-type (car args)))
		  (and (eq 'message (mime-content-type-primary-type
				     content-type))
		       (eq 'rfc822 (mime-content-type-subtype content-type))))
		(setcar (cdr args) 'not-decode-text))
	    (apply ofn args)))
    (unwind-protect
	(mime-edit-again)
      (fset 'mime-edit-decode-single-part-in-buffer ofn)))
  (when (featurep 'font-lock)
    (set (make-local-variable 'font-lock-defaults)
	 '(message-font-lock-keywords t))
    (font-lock-set-defaults)
    (turn-on-font-lock))
  (set-buffer-modified-p nil)
  (delete-other-windows)
  (add-hook 'gnus-article-mode-hook 'gnus-article-mime-edit-article-unwind)
  (gnus-run-hooks 'gnus-article-mime-edit-article-setup-hook))

(defun gnus-article-mime-edit-done (&optional arg)
  "Update the article MIME edits and exit."
  (interactive "P")
  (when (featurep 'font-lock)
    (setq font-lock-defaults nil)
    (font-lock-mode -1))
  (gnus-article-edit-done arg))

(defun gnus-article-mime-edit-exit ()
  "Exit the article MIME editing without updating."
  (interactive)
  (when (or (not (buffer-modified-p))
	    (yes-or-no-p "Article modified; kill anyway? "))
    (when (featurep 'font-lock)
      (setq font-lock-defaults nil)
      (font-lock-mode -1))
    (when mime-edit-mode-flag
      (let (mime-edit-insert-user-agent-field)
	(mime-edit-exit))
      (message ""))
    (goto-char (point-min))
    (let (case-fold-search)
      (when (re-search-forward
	     (format "^%s$" (regexp-quote mail-header-separator)) nil t)
	(replace-match "")))
    (let ((winconf gnus-prev-winconf))
      (insert (prog1
		  (buffer-substring-no-properties (point-min) (point-max))
		(set-buffer (get-buffer-create gnus-original-article-buffer))
		(erase-buffer)))
      (setq gnus-current-headers (gnus-article-make-full-mail-header))
      (set-buffer gnus-article-buffer)
      (gnus-article-prepare-display)
      (set-window-configuration winconf))))

;;;
;;; Article highlights
;;;

;; Written by Per Abrahamsen <abraham@iesd.auc.dk>.

;;; Internal Variables:

(defcustom gnus-button-url-regexp "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?\\([-a-zA-Z0-9_=!?#$@~`%&*+|\\/:;.,]\\|\\w\\)+\\([-a-zA-Z0-9_=#$@~`%&*+|\\/]\\|\\w\\)\\)"
  "Regular expression that matches URLs."
  :group 'gnus-article-buttons
  :type 'regexp)

(defcustom gnus-button-alist
  `(("<\\(url:[>\n\t ]*?\\)?\\(nntp\\|news\\):[>\n\t ]*\\([^>\n\t ]*@[^>\n\t ]*\\)>"
     0 t gnus-button-handle-news 3)
    ("\\b\\(nntp\\|news\\):\\([^>\n\t ]*@[^>)!;:,\n\t ]*\\)" 0 t
     gnus-button-handle-news 2)
    ("\\(\\b<\\(url:[>\n\t ]*\\)?news:[>\n\t ]*\\(//\\)?\\([^>\n\t ]*\\)>\\)"
     1 t
     gnus-button-fetch-group 4)
    ("\\bnews:\\(//\\)?\\([^'\">\n\t ]+\\)" 0 t gnus-button-fetch-group 2)
    ("\\bin\\( +article\\| +message\\)? +\\(<\\([^\n @<>]+@[^\n @<>]+\\)>\\)" 2
     t gnus-button-message-id 3)
    ("\\(<URL: *\\)mailto: *\\([^> \n\t]+\\)>" 0 t gnus-url-mailto 2)
    ("mailto:\\([-a-zA-Z.@_+0-9%=?]+\\)" 0 t gnus-url-mailto 1)
    ("\\bmailto:\\([^ \n\t]+\\)" 0 t gnus-url-mailto 1)
    ;; This is info
    ("\\binfo:\\(//\\)?\\([^'\">\n\t ]+\\)" 0 t
     gnus-button-handle-info 2)
    ;; This is how URLs _should_ be embedded in text...
    ("<URL: *\\([^<>]*\\)>" 1 t gnus-button-embedded-url 1)
    ;; Raw URLs.
    (,gnus-button-url-regexp 0 t browse-url 0))
  "*Alist of regexps matching buttons in article bodies.

Each entry has the form (REGEXP BUTTON FORM CALLBACK PAR...), where
REGEXP: is the string matching text around the button,
BUTTON: is the number of the regexp grouping actually matching the button,
FORM: is a lisp expression which must eval to true for the button to
be added,
CALLBACK: is the function to call when the user push this button, and each
PAR: is a number of a regexp grouping whose text will be passed to CALLBACK.

CALLBACK can also be a variable, in that case the value of that
variable it the real callback function."
  :group 'gnus-article-buttons
  :type '(repeat (list regexp
		       (integer :tag "Button")
		       (sexp :tag "Form")
		       (function :tag "Callback")
		       (repeat :tag "Par"
			       :inline t
			       (integer :tag "Regexp group")))))

(defcustom gnus-header-button-alist
  `(("^\\(References\\|Message-I[Dd]\\):" "<[^<>]+>"
     0 t gnus-button-message-id 0)
    ("^\\(From\\|Reply-To\\):" ": *\\(.+\\)$" 1 t gnus-button-reply 1)
    ("^\\(Cc\\|To\\):" "[^ \t\n<>,()\"]+@[^ \t\n<>,()\"]+"
     0 t gnus-button-mailto 0)
    ("^X-[Uu][Rr][Ll]:" ,gnus-button-url-regexp 0 t browse-url 0)
    ("^Subject:" ,gnus-button-url-regexp 0 t browse-url 0)
    ("^[^:]+:" ,gnus-button-url-regexp 0 t browse-url 0)
    ("^[^:]+:" "\\bmailto:\\([-a-zA-Z.@_+0-9%=?]+\\)" 0 t gnus-url-mailto 1)
    ("^[^:]+:" "\\(<\\(url: \\)?news:\\([^>\n ]*\\)>\\)" 1 t
     gnus-button-message-id 3))
  "*Alist of headers and regexps to match buttons in article heads.

This alist is very similar to `gnus-button-alist', except that each
alist has an additional HEADER element first in each entry:

\(HEADER REGEXP BUTTON FORM CALLBACK PAR)

HEADER is a regexp to match a header.  For a fuller explanation, see
`gnus-button-alist'."
  :group 'gnus-article-buttons
  :group 'gnus-article-headers
  :type '(repeat (list (regexp :tag "Header")
		       regexp
		       (integer :tag "Button")
		       (sexp :tag "Form")
		       (function :tag "Callback")
		       (repeat :tag "Par"
			       :inline t
			       (integer :tag "Regexp group")))))

(defvar gnus-button-regexp nil)
(defvar gnus-button-marker-list nil)
;; Regexp matching any of the regexps from `gnus-button-alist'.

(defvar gnus-button-last nil)
;; The value of `gnus-button-alist' when `gnus-button-regexp' was build.

;;; Commands:

(defun gnus-article-push-button (event)
  "Check text under the mouse pointer for a callback function.
If the text under the mouse pointer has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (let* ((pos (posn-point (event-start event)))
	 (data (get-text-property pos 'gnus-data))
	 (fun (get-text-property pos 'gnus-callback)))
    (goto-char pos)
    (when fun
      (funcall fun data))))

(defun gnus-article-press-button ()
  "Check text at point for a callback function.
If the text at point has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive)
  (let* ((data (get-text-property (point) 'gnus-data))
	 (fun (get-text-property (point) 'gnus-callback)))
    (when fun
      (funcall fun data))))

(defun gnus-article-prev-button (n)
  "Move point to N buttons backward.
If N is negative, move forward instead."
  (interactive "p")
  (gnus-article-next-button (- n)))

(defun gnus-article-next-button (n)
  "Move point to N buttons forward.
If N is negative, move backward instead."
  (interactive "p")
  (let ((function (if (< n 0) 'previous-single-property-change
		    'next-single-property-change))
	(inhibit-point-motion-hooks t)
	(backward (< n 0))
	(limit (if (< n 0) (point-min) (point-max))))
    (setq n (abs n))
    (while (and (not (= limit (point)))
		(> n 0))
      ;; Skip past the current button.
      (when (get-text-property (point) 'gnus-callback)
	(goto-char (funcall function (point) 'gnus-callback nil limit)))
      ;; Go to the next (or previous) button.
      (gnus-goto-char (funcall function (point) 'gnus-callback nil limit))
      ;; Put point at the start of the button.
      (when (and backward (not (get-text-property (point) 'gnus-callback)))
	(goto-char (funcall function (point) 'gnus-callback nil limit)))
      ;; Skip past intangible buttons.
      (when (get-text-property (point) 'intangible)
	(incf n))
      (decf n))
    (unless (zerop n)
      (gnus-message 5 "No more buttons"))
    n))

(defun gnus-article-highlight (&optional force)
  "Highlight current article.
This function calls `gnus-article-highlight-headers',
`gnus-article-highlight-citation',
`gnus-article-highlight-signature', and `gnus-article-add-buttons' to
do the highlighting.  See the documentation for those functions."
  (interactive (list 'force))
  (gnus-article-highlight-headers)
  (gnus-article-highlight-citation force)
  (gnus-article-highlight-signature)
  (gnus-article-add-buttons force)
  (gnus-article-add-buttons-to-head))

(defun gnus-article-highlight-some (&optional force)
  "Highlight current article.
This function calls `gnus-article-highlight-headers',
`gnus-article-highlight-signature', and `gnus-article-add-buttons' to
do the highlighting.  See the documentation for those functions."
  (interactive (list 'force))
  (gnus-article-highlight-headers)
  (gnus-article-highlight-signature)
  (gnus-article-add-buttons))

(defun gnus-article-highlight-headers ()
  "Highlight article headers as specified by `gnus-header-face-alist'."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (let ((alist gnus-header-face-alist)
	    (buffer-read-only nil)
	    (case-fold-search t)
	    (inhibit-point-motion-hooks t)
	    entry regexp header-face field-face from hpoints fpoints)
	(article-narrow-to-head)
	(while (setq entry (pop alist))
	  (goto-char (point-min))
	  (setq regexp (concat "^\\("
			       (if (string-equal "" (nth 0 entry))
				   "[^\t ]"
				 (nth 0 entry))
			       "\\)")
		header-face (nth 1 entry)
		field-face (nth 2 entry))
	  (while (and (re-search-forward regexp nil t)
		      (not (eobp)))
	    (beginning-of-line)
	    (setq from (point))
	    (unless (search-forward ":" nil t)
	      (forward-char 1))
	    (when (and header-face
		       (not (memq (point) hpoints)))
	      (push (point) hpoints)
	      (gnus-put-text-property from (point) 'face header-face))
	    (when (and field-face
		       (not (memq (setq from (point)) fpoints)))
	      (push from fpoints)
	      (if (re-search-forward "^[^ \t]" nil t)
		  (forward-char -2)
		(goto-char (point-max)))
	      (gnus-put-text-property from (point) 'face field-face))))))))

(defun gnus-article-highlight-signature ()
  "Highlight the signature in an article.
It does this by highlighting everything after
`gnus-signature-separator' using `gnus-signature-face'."
  (interactive)
  (when gnus-signature-face
    (save-excursion
      (set-buffer gnus-article-buffer)
      (let ((buffer-read-only nil)
	    (inhibit-point-motion-hooks t))
	(save-restriction
	  (when (gnus-article-narrow-to-signature)
	    (gnus-overlay-put (gnus-make-overlay (point-min) (point-max))
			      'face gnus-signature-face)))))))

(defun gnus-article-buttonize-signature ()
  "Add button to the signature."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t))
      (when (gnus-article-search-signature)
	(gnus-article-add-button (match-beginning 0) (match-end 0)
				 'gnus-signature-toggle
				 (set-marker (make-marker)
					     (1+ (match-end 0))))))))

(defun gnus-button-in-region-p (b e prop)
  "Say whether PROP exists in the region."
  (text-property-not-all b e prop nil))

(defun gnus-article-add-buttons (&optional force)
  "Find external references in the article and make buttons of them.
\"External references\" are things like Message-IDs and URLs, as
specified by `gnus-button-alist'."
  (interactive (list 'force))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (case-fold-search t)
	  (alist gnus-button-alist)
	  beg entry regexp)
      ;; Remove all old markers.
      (let (marker entry new-list)
	(while (setq marker (pop gnus-button-marker-list))
	  (if (or (< marker (point-min)) (>= marker (point-max)))
	      (push marker new-list)
	    (goto-char marker)
	    (when (setq entry (gnus-button-entry))
	      (put-text-property (match-beginning (nth 1 entry))
				 (match-end (nth 1 entry))
				 'gnus-callback nil))
	    (set-marker marker nil)))
	(setq gnus-button-marker-list new-list))
      ;; We skip the headers.
      (article-goto-body)
      (setq beg (point))
      (while (setq entry (pop alist))
	(setq regexp (car entry))
	(goto-char beg)
	(while (re-search-forward regexp nil t)
	  (let* ((start (and entry (match-beginning (nth 1 entry))))
		 (end (and entry (match-end (nth 1 entry))))
		 (from (match-beginning 0)))
	    (when (and (or (eq t (nth 2 entry))
			   (eval (nth 2 entry)))
		       (not (gnus-button-in-region-p
			     start end 'gnus-callback)))
	      ;; That optional form returned non-nil, so we add the
	      ;; button.
	      (gnus-article-add-button
	       start end 'gnus-button-push
	       (car (push (set-marker (make-marker) from)
			  gnus-button-marker-list))))))))))

;; Add buttons to the head of an article.
(defun gnus-article-add-buttons-to-head ()
  "Add buttons to the head of the article."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (let ((buffer-read-only nil)
	    (inhibit-point-motion-hooks t)
	    (case-fold-search t)
	    (alist gnus-header-button-alist)
	    entry beg end)
	(article-narrow-to-head)
	(while alist
	  ;; Each alist entry.
	  (setq entry (car alist)
		alist (cdr alist))
	  (goto-char (point-min))
	  (while (re-search-forward (car entry) nil t)
	    ;; Each header matching the entry.
	    (setq beg (match-beginning 0))
	    (setq end (or (and (re-search-forward "^[^ \t]" nil t)
			       (match-beginning 0))
			  (point-max)))
	    (goto-char beg)
	    (while (re-search-forward (nth 1 entry) end t)
	      ;; Each match within a header.
	      (let* ((entry (cdr entry))
		     (start (match-beginning (nth 1 entry)))
		     (end (match-end (nth 1 entry)))
		     (form (nth 2 entry)))
		(goto-char (match-end 0))
		(when (eval form)
		  (gnus-article-add-button
		   start end (nth 3 entry)
		   (buffer-substring (match-beginning (nth 4 entry))
				     (match-end (nth 4 entry)))))))
	    (goto-char end)))))))

;;; External functions:

(defun gnus-article-add-button (from to fun &optional data)
  "Create a button between FROM and TO with callback FUN and data DATA."
  (when gnus-article-button-face
    (gnus-overlay-put (gnus-make-overlay from to)
		      'face gnus-article-button-face))
  (gnus-add-text-properties
   from to
   (nconc (and gnus-article-mouse-face
	       (list gnus-mouse-face-prop gnus-article-mouse-face))
	  (list 'gnus-callback fun)
	  (and data (list 'gnus-data data))))
  (widget-convert-button 'link from to :action 'gnus-widget-press-button
			 ;; Quote `:button-keymap' for Mule 2.3
			 ;; but it won't work.
			 ':button-keymap gnus-widget-button-keymap))

;;; Internal functions:

(defun gnus-article-set-globals ()
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (gnus-set-global-variables)))

(defun gnus-signature-toggle (end)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (limit (next-single-property-change end 'mime-view-entity
					      nil (point-max))))
      (if (text-property-any end limit 'article-type 'signature)
	  (progn
	    (gnus-delete-wash-type 'signature)
	    (gnus-remove-text-properties-when
	     'article-type 'signature end limit
	     (cons 'article-type (cons 'signature
				       gnus-hidden-properties))))
	(gnus-add-wash-type 'signature)
	(gnus-add-text-properties-when
	 'article-type nil end limit
	 (cons 'article-type (cons 'signature
				   gnus-hidden-properties)))))
    (let ((gnus-article-mime-handle-alist-1 gnus-article-mime-handle-alist))
      (gnus-set-mode-line 'article))))

(defun gnus-button-entry ()
  ;; Return the first entry in `gnus-button-alist' matching this place.
  (let ((alist gnus-button-alist)
	(entry nil))
    (while alist
      (setq entry (pop alist))
      (if (looking-at (car entry))
	  (setq alist nil)
	(setq entry nil)))
    entry))

(defun gnus-button-push (marker)
  ;; Push button starting at MARKER.
  (save-excursion
    (goto-char marker)
    (let* ((entry (gnus-button-entry))
	   (inhibit-point-motion-hooks t)
	   (fun (nth 3 entry))
	   (args (mapcar (lambda (group)
			   (let ((string (match-string group)))
			     (gnus-set-text-properties
			      0 (length string) nil string)
			     string))
			 (nthcdr 4 entry))))
      (cond
       ((fboundp fun)
	(apply fun args))
       ((and (boundp fun)
	     (fboundp (symbol-value fun)))
	(apply (symbol-value fun) args))
       (t
	(gnus-message 1 "You must define `%S' to use this button"
		      (cons fun args)))))))

(defun gnus-parse-news-url (url)
  (let (scheme server group message-id articles)
    (with-temp-buffer
      (insert url)
      (goto-char (point-min))
      (when (looking-at "\\([A-Za-z]+\\):")
	(setq scheme (match-string 1))
	(goto-char (match-end 0)))
      (when (looking-at "//\\([^/]+\\)/")
	(setq server (match-string 1))
	(goto-char (match-end 0)))

      (cond
       ((looking-at "\\(.*@.*\\)")
	(setq message-id (match-string 1)))
       ((looking-at "\\([^/]+\\)/\\([-0-9]+\\)")
	(setq group (match-string 1)
	      articles (split-string (match-string 2) "-")))
       ((looking-at "\\([^/]+\\)/?")
	(setq group (match-string 1)))
       (t
	(error "Unknown news URL syntax"))))
    (list scheme server group message-id articles)))

(defun gnus-button-handle-news (url)
  "Fetch a news URL."
  (destructuring-bind (scheme server group message-id articles)
      (gnus-parse-news-url url)
    (cond
     (message-id
      (save-excursion
	(set-buffer gnus-summary-buffer)
	(if server
	    (let ((gnus-refer-article-method (list (list 'nntp server))))
	      (gnus-summary-refer-article message-id))
	  (gnus-summary-refer-article message-id))))
     (group
      (gnus-button-fetch-group url)))))

(defun gnus-button-handle-info (url)
  "Fetch an info URL."
  (if (string-match
       "^\\([^:/]+\\)?/\\(.*\\)"
       url)
      (gnus-info-find-node
       (concat "(" (or (gnus-url-unhex-string (match-string 1 url))
		       "Gnus")
	       ")"
	       (gnus-url-unhex-string (match-string 2 url))))
    (error "Can't parse %s" url)))

(defun gnus-button-message-id (message-id)
  "Fetch MESSAGE-ID."
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (gnus-summary-refer-article message-id)))

(defun gnus-button-fetch-group (address)
  "Fetch GROUP specified by ADDRESS."
  (if (not (string-match "[:/]" address))
      ;; This is just a simple group url.
      (gnus-group-read-ephemeral-group address gnus-select-method)
    (if (not
	 (string-match
	  "^\\([^:/]+\\)\\(:\\([^/]+\\)\\)?/\\([^/]+\\)\\(/\\([0-9]+\\)\\)?"
	  address))
	(error "Can't parse %s" address)
      (gnus-group-read-ephemeral-group
       (match-string 4 address)
       `(nntp ,(match-string 1 address)
	      (nntp-address ,(match-string 1 address))
	      (nntp-port-number ,(if (match-end 3)
				     (match-string 3 address)
				   "nntp")))
       nil nil nil
       (and (match-end 6) (list (string-to-int (match-string 6 address))))))))

(defun gnus-url-parse-query-string (query &optional downcase)
  (let (retval pairs cur key val)
    (setq pairs (split-string query "&"))
    (while pairs
      (setq cur (car pairs)
	    pairs (cdr pairs))
      (if (not (string-match "=" cur))
	  nil				; Grace
	(setq key (gnus-url-unhex-string (substring cur 0 (match-beginning 0)))
	      val (gnus-url-unhex-string (substring cur (match-end 0) nil) t))
	(if downcase
	    (setq key (downcase key)))
	(setq cur (assoc key retval))
	(if cur
	    (setcdr cur (cons val (cdr cur)))
	  (setq retval (cons (list key val) retval)))))
    retval))

(defun gnus-url-mailto (url)
  ;; Send mail to someone
  (when (string-match "mailto:/*\\(.*\\)" url)
    (setq url (substring url (match-beginning 1) nil)))
  (let (to args subject func)
    (if (string-match (regexp-quote "?") url)
	(setq to (gnus-url-unhex-string (substring url 0 (match-beginning 0)))
	      args (gnus-url-parse-query-string
		    (substring url (match-end 0) nil) t))
      (setq to (gnus-url-unhex-string url)))
    (setq args (cons (list "to" to) args)
	  subject (cdr-safe (assoc "subject" args)))
    (gnus-msg-mail)
    (while args
      (setq func (intern-soft (concat "message-goto-" (downcase (caar args)))))
      (if (fboundp func)
	  (funcall func)
	(message-position-on-field (caar args)))
      (insert (mapconcat 'identity (cdar args) ", "))
      (setq args (cdr args)))
    (if subject
	(message-goto-body)
      (message-goto-subject))))

(defun gnus-button-embedded-url (address)
  "Activate ADDRESS with `browse-url'."
  (browse-url (gnus-strip-whitespace address)))

;;; Next/prev buttons in the article buffer.

(defvar gnus-next-page-line-format "%{%(Next page...%)%}\n")
(defvar gnus-prev-page-line-format "%{%(Previous page...%)%}\n")

(defvar gnus-prev-page-map
  (let ((map (make-sparse-keymap)))
    (unless (>= emacs-major-version 21)
      ;; XEmacs doesn't care.
      (set-keymap-parent map gnus-article-mode-map))
    (define-key map gnus-mouse-2 'gnus-button-prev-page)
    (define-key map "\r" 'gnus-button-prev-page)
    map))

(defun gnus-insert-prev-page-button ()
  (let ((b (point))
	(buffer-read-only nil)
	(situation (get-text-property (point-min) 'mime-view-situation)))
    (gnus-eval-format
     gnus-prev-page-line-format nil
     `(,@(gnus-local-map-property gnus-prev-page-map)
	 gnus-prev t
	 gnus-callback gnus-article-button-prev-page
	 article-type annotation
	 mime-view-situation ,situation))
    (widget-convert-button
     'link b (point)
     :action 'gnus-button-prev-page
     :button-keymap gnus-prev-page-map)))

(defvar gnus-next-page-map
  (let ((map (make-sparse-keymap)))
    (unless (>= emacs-major-version 21)
      ;; XEmacs doesn't care.
      (set-keymap-parent map gnus-article-mode-map))
    (define-key map gnus-mouse-2 'gnus-button-next-page)
    (define-key map "\r" 'gnus-button-next-page)
    map))

(defun gnus-button-next-page (&optional args more-args)
  "Go to the next page."
  (interactive)
  (let ((win (selected-window)))
    (select-window (gnus-get-buffer-window gnus-article-buffer t))
    (gnus-article-next-page)
    (select-window win)))

(defun gnus-button-prev-page (&optional args more-args)
  "Go to the prev page."
  (interactive)
  (let ((win (selected-window)))
    (select-window (gnus-get-buffer-window gnus-article-buffer t))
    (gnus-article-prev-page)
    (select-window win)))

(defun gnus-insert-next-page-button ()
  (let ((b (point))
	(buffer-read-only nil)
	(situation (get-text-property (point-min) 'mime-view-situation)))
    (gnus-eval-format gnus-next-page-line-format nil
		      `(,@(gnus-local-map-property gnus-next-page-map)
			  gnus-next t
			  gnus-callback gnus-article-button-next-page
			  article-type annotation
			  mime-view-situation ,situation))
    (widget-convert-button
     'link b (point)
     :action 'gnus-button-next-page
     :button-keymap gnus-next-page-map)))

(defun gnus-article-button-next-page (arg)
  "Go to the next page."
  (interactive "P")
  (let ((win (selected-window)))
    (select-window (gnus-get-buffer-window gnus-article-buffer t))
    (gnus-article-next-page)
    (select-window win)))

(defun gnus-article-button-prev-page (arg)
  "Go to the prev page."
  (interactive "P")
  (let ((win (selected-window)))
    (select-window (gnus-get-buffer-window gnus-article-buffer t))
    (gnus-article-prev-page)
    (select-window win)))

(defvar gnus-decode-header-methods
  '(mail-decode-encoded-word-region)
  "List of methods used to decode headers.

This variable is a list of FUNCTION or (REGEXP . FUNCTION).  If item
is FUNCTION, FUNCTION will be apply to all newsgroups.  If item is a
\(REGEXP . FUNCTION), FUNCTION will be only apply to thes newsgroups
whose names match REGEXP.

For example:
\((\"chinese\" . gnus-decode-encoded-word-region-by-guess)
 mail-decode-encoded-word-region
 (\"chinese\" . rfc1843-decode-region))
")

(defvar gnus-decode-header-methods-cache nil)

(defun gnus-multi-decode-header (start end)
  "Apply the functions from `gnus-encoded-word-methods' that match."
  (unless (and gnus-decode-header-methods-cache
	       (eq gnus-newsgroup-name
		   (car gnus-decode-header-methods-cache)))
    (setq gnus-decode-header-methods-cache (list gnus-newsgroup-name))
    (mapcar (lambda (x)
	      (if (symbolp x)
		  (nconc gnus-decode-header-methods-cache (list x))
		(if (and gnus-newsgroup-name
			 (string-match (car x) gnus-newsgroup-name))
		    (nconc gnus-decode-header-methods-cache
			   (list (cdr x))))))
	    gnus-decode-header-methods))
  (let ((xlist gnus-decode-header-methods-cache))
    (pop xlist)
    (save-restriction
      (narrow-to-region start end)
      (while xlist
	(funcall (pop xlist) (point-min) (point-max))))))

;;;
;;; Treatment top-level handling.
;;;

(defun gnus-treat-article (condition &optional part-number total-parts type)
  (let ((length (- (point-max) (point-min)))
	(alist gnus-treatment-function-alist)
	(article-goto-body-goes-to-point-min-p t)
	(treated-type
	 (or (not type)
	     (catch 'found
	       (let ((list gnus-article-treat-types))
		 (while list
		   (when (string-match (pop list) type)
		     (throw 'found t)))))))
	(highlightp (gnus-visual-p 'article-highlight 'highlight))
	(entity (static-unless (featurep 'xemacs)
		  (when (eq 'head condition)
		    (get-text-property (point-min) 'mime-view-entity))))
	val elem buttonized)
    (gnus-run-hooks 'gnus-part-display-hook)
    (unless gnus-inhibit-treatment
      (while (setq elem (pop alist))
	(setq val
	      (save-excursion
		(if (gnus-buffer-live-p gnus-summary-buffer)
		    (set-buffer gnus-summary-buffer))
		(symbol-value (car elem))))
	(when (and (or (consp val)
		       treated-type)
		   (gnus-treat-predicate val)
		   (or (not (get (car elem) 'highlight))
		       highlightp))
	  (when (and (not buttonized)
		     (memq (car elem)
			   '(gnus-treat-hide-signature
			     gnus-treat-highlight-signature)))
	    (gnus-article-buttonize-signature)
	    (setq buttonized t))
	  (save-restriction
	    (funcall (cadr elem)))))
      ;; FSF Emacsen does not inherit the existing text properties
      ;; in the new text, so we should do it for `mime-view-entity'.
      (static-unless (featurep 'xemacs)
	(when entity
	  (put-text-property (point-min) (point-max)
			     'mime-view-entity entity))))))

;; Dynamic variables.
(eval-when-compile
  (defvar part-number)
  (defvar total-parts)
  (defvar type)
  (defvar condition)
  (defvar length))

(defun gnus-treat-predicate (val)
  (cond
   ((null val)
    nil)
   ((and (listp val)
	 (stringp (car val)))
    (apply 'gnus-or (mapcar `(lambda (s)
			       (string-match s ,(or gnus-newsgroup-name "")))
			    val)))
   ((listp val)
    (let ((pred (pop val)))
      (cond
       ((eq pred 'or)
	(apply 'gnus-or (mapcar 'gnus-treat-predicate val)))
       ((eq pred 'and)
	(apply 'gnus-and (mapcar 'gnus-treat-predicate val)))
       ((eq pred 'not)
	(not (gnus-treat-predicate (car val))))
       ((eq pred 'typep)
	(equal (car val) type))
       (t
	(error "%S is not a valid predicate" pred)))))
   ((eq val 'mime)
    gnus-show-mime)
   (condition
    (eq condition val))
   ((eq val t)
    t)
   ((eq val 'head)
    nil)
   ((eq val 'last)
    (eq part-number total-parts))
   ((numberp val)
    (< length val))
   (t
    (error "%S is not a valid value" val))))

(defun gnus-article-encrypt-body (protocol &optional n)
  "Encrypt the article body."
  (interactive
   (list
    (or gnus-article-encrypt-protocol
	(completing-read "Encrypt protocol: "
			 gnus-article-encrypt-protocol-alist
			 nil t))
    current-prefix-arg))
  (let ((func (cdr (assoc protocol gnus-article-encrypt-protocol-alist))))
    (unless func
      (error (format "Can't find the encrypt protocol %s" protocol)))
    (if (equal gnus-newsgroup-name "nndraft:drafts")
	(error "Can't encrypt the article in group nndraft:drafts"))
    (if (equal gnus-newsgroup-name "nndraft:queue")
	(error "Don't encrypt the article in group nndraft:queue"))
    (gnus-summary-iterate n
      (save-excursion
	(set-buffer gnus-summary-buffer)
	(let ((mail-parse-charset gnus-newsgroup-charset)
	      (mail-parse-ignored-charsets gnus-newsgroup-ignored-charsets)
	      (summary-buffer gnus-summary-buffer)
	      references point)
	  (gnus-set-global-variables)
	  (when (gnus-group-read-only-p)
	    (error "The current newsgroup does not support article encrypt"))
	  (gnus-summary-show-article t)
	  (setq references
		(or (mail-header-references gnus-current-headers) ""))
	  (set-buffer gnus-article-buffer)
	  (let* ((buffer-read-only nil)
		 (headers
		  (mapcar (lambda (field)
			    (and (save-restriction
				   (message-narrow-to-head)
				   (goto-char (point-min))
				   (search-forward field nil t))
				 (prog2
				     (message-narrow-to-field)
				     (buffer-substring (point-min) (point-max))
				   (delete-region (point-min) (point-max))
				   (widen))))
			  '("Content-Type:" "Content-Transfer-Encoding:"
			    "Content-Disposition:"))))
	    (message-narrow-to-head)
	    (message-remove-header "MIME-Version")
	    (goto-char (point-max))
	    (setq point (point))
	    (insert (apply 'concat headers))
	    (widen)
	    (narrow-to-region point (point-max))
	    (let ((message-options message-options))
	      (message-options-set 'message-sender user-mail-address)
	      (message-options-set 'message-recipients user-mail-address)
	      (message-options-set 'message-sign-encrypt 'not)
	      (funcall func))
	    (goto-char (point-min))
	    (insert "MIME-Version: 1.0\n")
	    (widen)
	    (gnus-summary-edit-article-done
	     references nil summary-buffer t))
	  (when gnus-keep-backlog
	    (gnus-backlog-remove-article
	     (car gnus-article-current) (cdr gnus-article-current)))
	  (save-excursion
	    (when (get-buffer gnus-original-article-buffer)
	      (set-buffer gnus-original-article-buffer)
	      (setq gnus-original-article nil)))
	  (when gnus-use-cache
	    (gnus-cache-update-article
	     (car gnus-article-current) (cdr gnus-article-current))))))))

(defvar gnus-mime-security-button-line-format "%{%([[%t:%i]%D]%)%}\n"
  "The following specs can be used:
%t  The security MIME type
%i  Additional info
%d  Details
%D  Details if button is pressed")

(defvar gnus-mime-security-button-end-line-format "%{%([[End of %t]%D]%)%}\n"
  "The following specs can be used:
%t  The security MIME type
%i  Additional info
%d  Details
%D  Details if button is pressed")

(defvar gnus-mime-security-button-line-format-alist
  '((?t gnus-tmp-type ?s)
    (?i gnus-tmp-info ?s)
    (?d gnus-tmp-details ?s)
    (?D gnus-tmp-pressed-details ?s)))

(defvar gnus-mime-security-button-map
  (let ((map (make-sparse-keymap)))
    (unless (>= (string-to-number emacs-version) 21)
      (set-keymap-parent map gnus-article-mode-map))
    (define-key map gnus-mouse-2 'gnus-article-push-button)
    (define-key map "\r" 'gnus-article-press-button)
    map))

(defvar gnus-mime-security-details-buffer nil)

(defvar gnus-mime-security-button-pressed nil)

(defvar gnus-mime-security-show-details-inline t
  "If non-nil, show details in the article buffer.")

(defun gnus-mime-security-verify-or-decrypt (handle)
  (mm-remove-parts (cdr handle))
  (let ((region (mm-handle-multipart-ctl-parameter handle 'gnus-region))
	point buffer-read-only)
    (if region
	(goto-char (car region)))
    (save-restriction
      (narrow-to-region (point) (point))
      (with-current-buffer (mm-handle-multipart-original-buffer handle)
	(let* ((mm-verify-option 'known)
	       (mm-decrypt-option 'known)
	       (nparts (mm-possibly-verify-or-decrypt (cdr handle) handle)))
	  (unless (eq nparts (cdr handle))
	    (mm-destroy-parts (cdr handle))
	    (setcdr handle nparts))))
      (setq point (point))
      (gnus-mime-display-security handle)
      (goto-char (point-max)))
    (when region
      (delete-region (point) (cdr region))
      (set-marker (car region) nil)
      (set-marker (cdr region) nil))
    (goto-char point)))

(defun gnus-mime-security-show-details (handle)
  (let ((details (mm-handle-multipart-ctl-parameter handle 'gnus-details)))
    (if details
	(if gnus-mime-security-show-details-inline
	    (let ((gnus-mime-security-button-pressed t)
		  (gnus-mime-security-button-line-format
		   (get-text-property (point) 'gnus-line-format))
		  buffer-read-only)
	      (forward-char -1)
	      (while (eq (get-text-property (point) 'gnus-line-format)
			 gnus-mime-security-button-line-format)
		(forward-char -1))
	      (forward-char)
	      (save-restriction
		(narrow-to-region (point) (point))
		(gnus-insert-mime-security-button handle))
	      (delete-region (point)
			     (or (text-property-not-all
				  (point) (point-max)
				  'gnus-line-format
				  gnus-mime-security-button-line-format)
				 (point-max))))
	  (if (gnus-buffer-live-p gnus-mime-security-details-buffer)
	      (with-current-buffer gnus-mime-security-details-buffer
		(erase-buffer)
		t)
	    (setq gnus-mime-security-details-buffer
		  (gnus-get-buffer-create "*MIME Security Details*")))
	  (with-current-buffer gnus-mime-security-details-buffer
	    (insert details)
	    (goto-char (point-min)))
	  (pop-to-buffer gnus-mime-security-details-buffer))
      (gnus-message 5 "No details."))))

(defun gnus-mime-security-press-button (handle)
  (if (mm-handle-multipart-ctl-parameter handle 'gnus-info)
      (gnus-mime-security-show-details handle)
    (gnus-mime-security-verify-or-decrypt handle)))

(defun gnus-insert-mime-security-button (handle &optional displayed)
  (let* ((protocol (mm-handle-multipart-ctl-parameter handle 'protocol))
	 (gnus-tmp-type
	  (concat
	   (or (nth 2 (assoc protocol mm-verify-function-alist))
	       (nth 2 (assoc protocol mm-decrypt-function-alist))
	       "Unknown")
	   (if (equal (car handle) "multipart/signed")
	       " Signed" " Encrypted")
	   " Part"))
	 (gnus-tmp-info
	  (or (mm-handle-multipart-ctl-parameter handle 'gnus-info)
	      "Undecided"))
	 (gnus-tmp-details
	  (mm-handle-multipart-ctl-parameter handle 'gnus-details))
	 gnus-tmp-pressed-details
	 b e)
    (setq gnus-tmp-details
	  (if gnus-tmp-details
	      (concat "\n" gnus-tmp-details) ""))
    (setq gnus-tmp-pressed-details
	  (if gnus-mime-security-button-pressed gnus-tmp-details ""))
    (unless (bolp)
      (insert "\n"))
    (setq b (point))
    (gnus-eval-format
     gnus-mime-security-button-line-format
     gnus-mime-security-button-line-format-alist
     `(,@(gnus-local-map-property gnus-mime-security-button-map)
	 gnus-callback gnus-mime-security-press-button
	 gnus-line-format ,gnus-mime-security-button-line-format
	 article-type annotation
	 gnus-data ,handle))
    (setq e (point))
    (widget-convert-button
     'link b e
     :mime-handle handle
     :action 'gnus-widget-press-button
     :button-keymap gnus-mime-security-button-map
     :help-echo
     (lambda (widget/window &optional overlay pos)
       ;; Needed to properly clear the message due to a bug in
       ;; wid-edit (XEmacs only).
       (if (boundp 'help-echo-owns-message)
	   (setq help-echo-owns-message t))
       (format
	"%S: show detail"
	(aref gnus-mouse-2 0))))))

(defun gnus-mime-display-security (handle)
  (save-restriction
    (narrow-to-region (point) (point))
    (unless (gnus-unbuttonized-mime-type-p (car handle))
      (gnus-insert-mime-security-button handle))
    (gnus-mime-display-mixed (cdr handle))
    (unless (bolp)
      (insert "\n"))
    (unless (gnus-unbuttonized-mime-type-p (car handle))
      (let ((gnus-mime-security-button-line-format
	     gnus-mime-security-button-end-line-format))
	(gnus-insert-mime-security-button handle)))
    (mm-set-handle-multipart-parameter
     handle 'gnus-region
     (cons (set-marker (make-marker) (point-min))
	   (set-marker (make-marker) (point-max))))))


;;; @ for mime-view
;;;

(defun gnus-article-header-presentation-method (entity situation)
  (mime-insert-header entity)
  (article-decode-group-name))

(set-alist 'mime-header-presentation-method-alist
	   'gnus-original-article-mode
	   #'gnus-article-header-presentation-method)

(defun gnus-mime-preview-quitting-method ()
  (mime-preview-kill-buffer)
  (delete-other-windows)
  (gnus-article-show-summary)
  (gnus-summary-select-article gnus-show-all-headers t))

(set-alist 'mime-preview-quitting-method-alist
	   'gnus-original-article-mode #'gnus-mime-preview-quitting-method)

(set-alist 'mime-preview-following-method-alist
	   'gnus-original-article-mode #'gnus-following-method)

(set-alist 'mime-preview-over-to-previous-method-alist
	   'gnus-original-article-mode
	   (lambda ()
	     (if (> (point-min) 1)
		 (gnus-article-prev-page)
	       (gnus-article-read-summary-keys
		nil (gnus-character-to-event ?P)))))

(set-alist 'mime-preview-over-to-next-method-alist
	   'gnus-original-article-mode'
	   (lambda ()
	     (if (< (point-max) (buffer-size))
		 (gnus-article-next-page)
	       (gnus-article-read-summary-keys
		nil (gnus-character-to-event ?N)))))


;;; @ end
;;;

(gnus-ems-redefine)

(provide 'gnus-art)

(run-hooks 'gnus-art-load-hook)

;;; gnus-art.el ends here

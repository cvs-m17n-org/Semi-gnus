;; This is a sample of `~/.lpath.el' file.
;;
;; This file will be loaded from dgnushack.el at the compile time.  It
;; is supposed to be used for telling old Emacsen where EMU, APEL or
;; CUSTOM packages have already installed.
;;
;; For instance, if you would like to build T-gnus with Mule 2.3 based
;; on Emacs 19.34, copy this file to `~/.lpath.el' and edit it suitably
;; for your environment.

(if (boundp 'MULE)
    (let ((additional-load-path
	   (list
	    ;; Where is EMU packege?
	    "/usr/local/share/mule/19.34/site-lisp/"
	    ;; Where is APEL package?
	    "/usr/local/share/mule/site-lisp/apel/"
	    ;; Where is CUSTOM package?
	    "/usr/local/share/mule/site-lisp/custom/"

	    ;; Note that you have no need to specify paths of FLIM, SEMI
	    ;; or WEMI if they are installed under the directory which is
	    ;; same as the parent directory of APEL.

	    ;; If you have installed Emacs W3 package,
	    ;; uncomment and edit the following line appropriately.
	    ;; "/usr/local/share/mule/site-lisp/w3/"
	    )))
      ;; No user servicable parts beyond this point.

      (let ((i (length additional-load-path))
	    p)
	(while (> i 0)
	  (setq i (1- i)
		p (nth i additional-load-path))
	  (if (file-directory-p p)
	      (progn
		(if (string-match "/apel/?$" p)
		    (setq load-path
			  (cons (substring p 0 (1+ (match-beginning 0)))
				load-path)))
		(setq load-path (cons p load-path))))))))

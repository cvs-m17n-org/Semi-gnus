;; This is a sample of `~/.lpath.el' file.
;;
;; This file will be loaded from dgnushack.el at the compile time.  It
;; is supposed to be used for telling old Emacsen where EMU, APEL or
;; CUSTOM packages have already installed.
;;
;; For instance, if you would like to make gnus with Mule 2.3 based on
;; Emacs 19.34, copy this file to `~/.lpath.el' and modify it suitably
;; for your environment.

(when (boundp 'MULE)
  (let ((EMU
	 ;; Where is EMU packege?
	 "/usr/local/share/mule/19.34/site-lisp"
	 )
	(APEL
	 ;; Where is APEL package?
	 "/usr/local/share/mule/site-lisp/apel"
	 )
	(CUSTOM
	 ;; Where is CUSTOM package?
	 "/usr/local/share/mule/site-lisp/custom"
	 ))
    ;; No user servicable parts beyond this point.

    (when (string-match "/apel/?$" APEL)
      (setq APEL (substring APEL 0 (match-beginning 0))))
    (setq load-path (nconc (list EMU APEL CUSTOM) load-path))))

EMACS=emacs
XEMACS=xemacs

all: lick info

lick:
	cd lisp; $(MAKE) EMACS=$(EMACS) all

# Rule for Lars and nobody else.
some:
	cd lisp; $(MAKE) EMACS=$(EMACS) some
l:
	cd lisp; $(MAKE) EMACS=$(EMACS) clever

info:
	cd texi; $(MAKE) EMACS=$(EMACS) all

clean:
	rm -f */*.orig */*.rej *.orig *.rej

xsome: 
	cd lisp; $(MAKE) EMACS=$(XEMACS) some

elclean:
	rm lisp/*.elc

x:
	make EMACS=xemacs

distclean:
	make clean
	rm -r *~
	for i in lisp texi; do (cd $$i; make distclean); done

osome:
	make EMACS=emacs-19.34 some

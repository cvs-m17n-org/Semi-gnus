TEXI2DVI=texi2dvi
EMACS=emacs
MAKEINFO=$(EMACS) -batch -q -no-site-file
INFOSWI=-l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
XINFOSWI=-l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
LATEX=latex
DVIPS=dvips
PERL=perl
INFODIR=/usr/local/info

all: gnus message

most: texi2latex.elc latex latexps

gnus: gnus.texi
	$(MAKEINFO) -eval '(find-file "gnus.texi")' $(XINFOSWI)

message: message.texi
	$(MAKEINFO) -eval '(find-file "message.texi")' $(XINFOSWI)

dvi: gnus.dvi message.dvi

.texi.dvi :
	$(PERL) -n -e 'print unless (/\@iflatex/ .. /\@end iflatex/)' $< > gnustmp.texi
	$(TEXI2DVI) gnustmp.texi
	cp gnustmp.dvi $*.dvi
	rm gnustmp.*

refcard.dvi: refcard.tex gnuslogo.refcard gnusref.tex
	$(LATEX) refcard.tex

sclean:
	rm -f gnus.*.bak *.ky *.cp *.fn *.cps *.kys *.log *.aux *.dvi *.vr \
	*.tp *.toc \
	*.pg gnus.latexi *.aux *.[cgk]idx \
	gnus.ilg gnus.ind gnus.[cgk]ind gnus.idx \
	gnus.tmptexi gnus.tmplatexi gnus.tmplatexi1 texput.log *.orig *.rej \
	gnus.latexi*~*

clean:
	make sclean 
	rm -f *.latexi 
	rm tmp/*.ps

makeinfo: 
	makeinfo -o gnus gnus.texi	
	makeinfo -o message message.texi	

texi2latex.elc: texi2latex.el
	$(EMACS) -batch -l bytecomp -f batch-byte-recompile-directory

latex: gnus.texi
	$(EMACS) -batch -q -no-site-file gnus.texi -l ./texi2latex.elc -f latexi-translate

latexps: 
	make texi2latex.elc
	egrep -v "label.*Index|chapter.*Index" gnus.latexi > gnus.tmplatexi1
	$(LATEX) gnus.tmplatexi1
	splitindex
	makeindex -o gnus.kind gnus.kidx
	makeindex -o gnus.cind gnus.cidx
	makeindex -o gnus.gind gnus.gidx
	sed 's/\\char 5E\\relax {}/\\symbol{"5E}/' < gnus.kind > gnus.tmpkind
	mv gnus.tmpkind gnus.kind
	egrep -v "end{document}" gnus.tmplatexi1 > gnus.tmplatexi
	cat postamble.tex >> gnus.tmplatexi
	$(LATEX) gnus.tmplatexi 
	$(LATEX) gnus.tmplatexi 
	$(DVIPS) -f gnus.dvi > /local/tmp/larsi/gnus.ps

pss:	
	make latex
	make latexps

psout:
	make latex
	make latexboth
	make out

latexboth: 
	rm -f gnus-manual-a4.ps.gz gnus-manual-standard.ps.gz 
	make latexps
	mv /local/tmp/larsi/gnus.ps /local/tmp/larsi/gnus-manual-a4.ps
	gzip /local/tmp/larsi/gnus-manual-a4.ps 
	sed 's/,a4paper//' gnus.latexi > gnus-standard.latexi 
	mv gnus-standard.latexi gnus.latexi
	make latexps
	mv /local/tmp/larsi/gnus.ps /local/tmp/larsi/gnus-manual-standard.ps 
	gzip /local/tmp/larsi/gnus-manual-standard.ps 

out:
	cp /local/tmp/larsi/gnus-manual-standard.ps.gz \
	/local/tmp/larsi/gnus-manual-a4.ps.gz \
	/local/ftp/pub/emacs/gnus/manual
	mv /local/tmp/larsi/gnus-manual-standard.ps.gz \
	/local/tmp/larsi/gnus-manual-a4.ps.gz \
	/hom/larsi/www_docs/www.gnus.org/documents

veryclean:
	make clean
	rm -f gnus.dvi gnus.ps

distclean:
	make clean
	rm -f *.orig *.rej *.elc *~ gnus gnus-[0-9] gnus-[0-9][0-9]
	rm -f message message-[0-9]

install:
	cp gnus gnus-[0-9] gnus-[0-9][0-9] $(INFODIR)
	cp message $(INFODIR)

tmps:
	if [ ! -d /local/tmp/larsi ]; then mkdir /local/tmp/larsi; fi
	make screens
	make herds
	make etc
	make piconss
	make xfaces

herds:
	cd ps ; for i in new-herd-[0-9]*.gif; do echo $$i; giftoppm $$i | pnmcrop -white | pnmmargin -white 9 | pnmscale 2 | pnmsmooth -size 5 5 | ppmtopgm | pgmtops > ../tmp/`basename $$i .gif`.ps; done
	cd ps ; giftoppm new-herd-section.gif | pnmscale 4 | pnmsmooth -size 11 11 | ppmtopgm | pgmtops > ../tmp/new-herd-section.ps

screens:
	cd screen ; for i in *.gif; do echo $$i; giftoppm $$i | pnmmargin -black 1 | ppmtopgm | pgmtops > ../tmp/`basename $$i .gif`.ps; done	
	giftoppm ps/larsi.gif | ppmtopgm | pgmtops > tmp/larsi.ps

etc:
	cd ../etc/gnus; for i in gnus-*.xpm; do echo $$i; xpmtoppm $$i | ppmtopgm | pgmtops > ../../texi/tmp/`basename $$i .xpm`.ps; done

piconss:
	cd picons; for i in *.xbm; do echo $$i; xbmtopbm $$i | pgmtops > ../tmp/picons-`basename $$i .xbm`.ps; done
	cd picons; for i in *.gif; do echo $$i; giftoppm $$i | ppmtopgm | pgmtops > ../tmp/picons-`basename $$i .gif`.ps; done
	for i in tmp/picons-*.ps; do echo "\\gnuspicon{$$i}"; done > picons.tmplatexi

xfaces:
	cd xface; for i in *.gif; do echo $$i; giftoppm $$i | ppmtopgm | pgmtops > ../tmp/xface-`basename $$i .gif`.ps; done
	for i in tmp/xface-*.ps; do echo "\\gnusxface{$$i}"; done > xface.tmplatexi

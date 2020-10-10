.PHONY: clean html pdf include clean-include clean-crap info doc

doc: pdf html info clean-crap

clean-include:
	rm -rf include

clean-crap:
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr

clean: clean-include
	rm -f  *.pdf *.html *.info

include:
	sbcl \
	--eval '(ql:quickload :rashell)' \
	--eval '(with-compilation-unit () (load "docstrings.lisp"))' \
	--eval '(sb-texinfo:generate-includes "include/" (list :rashell) :base-package :rashell)' \
	--eval '(quit)'
	./runpatches

pdf: include
	texi2pdf rashell.texinfo

html: include
	makeinfo --html --no-split rashell.texinfo

info: include
	makeinfo rashell.texinfo

#publish:
#	rsync -va rashell.pdf rashell.html common-lisp.net:/project/rashell/public_html/draft/

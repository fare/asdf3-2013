ae := asdf3

src = asdf3.scrbl live-programming.scrbl old-bug.scrbl history.scrbl utils.rkt tutorial.scrbl

export PLTCOLLECTS:=$(shell pwd):${PLTCOLLECTS}

all: html # slideshow # PDF
html: ${ae}.html
pdf: ${ae}.pdf
PDF: pdf ${ae}.PDF

%.W: %.html
	w3m -T text/html $<

%.wc: %.html
	donuts.pl unhtml < $< | wc

%.PDF: %.pdf
	evince $<

%.pdf: %.scrbl ${src}
	scribble --dest-name $@ --pdf $<

${ae}.html: ${ae}.scrbl ${src}
%.html: %.scrbl utils.rkt
	scribble --dest-name $@ --html $<

%.latex: %.scrbl ${src}
	scribble --latex --dest tmp $<

clean:
	rm -f ${ae}.pdf ${ae}.html *.css *.js
	rm -rf tmp

mrproper:
	git clean -xfd

rsync: html pdf
	rsync -av ${ae}.html ${ae}.pdf common-lisp.net:~frideau/public_html/asdf-els2013/

slides: asdf-slides.rkt utils.rkt
	racket $<

long-slides: lil-slides-long.rkt utils.rkt
	racket $<

els-slides.pdf: els-slides.org
	emacs -batch --visit=$< --funcall org-export-as-pdf

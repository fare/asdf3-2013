ae := asdf3-2014

src = live-programming.scrbl utils.rkt bibliography.scrbl
# asdf3.scrbl old-bug.scrbl history.scrbl tutorial.scrbl

export PLTCOLLECTS:=$(shell pwd):${PLTCOLLECTS}

all: asdf asdf3-2014.PDF # html # slideshow # PDF
html: ${ae}.html
pdf: ${ae}.pdf
PDF: pdf ${ae}.PDF

asdf: asdf3-2014.html asdf3-2014.pdf
	rsync -av $^ *.js *.css ~/files/tmp/asdf/
	rsync -av ~/files/tmp/asdf/ bespin:files/tmp/asdf/


%.W: %.html
	w3m -T text/html $<

%.wc: %.html
	donuts.pl unhtml < $< | wc

%.PDF: %.pdf
	evince $<

%.pdf: %.scrbl ${src}
	scribble --dest-name $@ --pdf $<

${ae}.html: ${ae}.scrbl ${src}
%.html: %.scrbl utils.rkt bibliography.scrbl
	scribble --dest-name $@ --html $<

%.latex: %.scrbl ${src}
	scribble --latex --dest tmp $<

clean:
	rm -f *.pdf *.html *.tex *.css *.js
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

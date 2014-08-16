ae := asdf3-2014

src = asdf3-2014.scrbl ltu_culture_war.scrbl utils.rkt bibliography.scrbl
# asdf3.scrbl old-bug.scrbl history.scrbl tutorial.scrbl

export PLTCOLLECTS:=$(shell pwd):${PLTCOLLECTS}

all: ilc # asdf asdf3-2014.PDF # html # slideshow # PDF
html: ${ae}.html
pdf: ${ae}.pdf
PDF: pdf ${ae}.PDF

asdf: asdf3-2014.html asdf3-2014.pdf asdf3-els2014.html asdf3-els2014.pdf
	rsync -av --delete $^ *.js *.css ~/files/asdf3/
	rsync -av --delete ~/files/asdf3/ bespin:files/asdf3/
	rsync -av --delete ~/files/asdf3/ st:www/asdf3/

%.W: %.html
	w3m -T text/html $<

%.wc: %.html
	donuts.pl unhtml < $< | wc

%.PDF: %.pdf
	evince $<

%.pdf: %.scrbl ${src}
	time scribble --dest-name $@ --pdf $<

${ae}.html: ${ae}.scrbl ${src}
%.html: %.scrbl utils.rkt bibliography.scrbl
	time scribble --dest-name $@ --html $<

%.latex: %.scrbl ${src}
	time scribble --latex --dest tmp $<

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

ilc: ilc2014-slides.rkt utils.rkt
	racket $<

%.show: %.rkt utils.rkt
	slideshow --right-half-screen --time --clock --comment $<

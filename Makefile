PHONY: cleantex cleancode clean testcode testtex test all console sbt pdf coverage present

.DEFAULT_GOAL := all

CODEDIR=./code/monoid
PDFLATEX=latexmk -pdf -interaction=nonstopmode -file-line-error
SLIDESDIR=slides

all: $(SLIDESDIR)/slides.pdf test

cleantex:
	cd $(SLIDESDIR) && latexmk -C

cleancode:
	cd $(CODEDIR) && sbt clean

clean: cleantex cleancode

testcode:
	cd $(CODEDIR) && sbt test

testtex: $(SLIDESDIR)/slides.pdf

test: testtex testcode

$(SLIDESDIR)/slides.pdf: $(SLIDESDIR)/slides.tex $(SLIDESDIR)/tree.tex $(SLIDESDIR)/functional-programming-in-scala.png
	cd $(SLIDESDIR) && $(PDFLATEX) slides.tex

console:
	cd $(CODEDIR) && sbt console

sbt:
	cd $(CODEDIR) && sbt

pdf: $(SLIDESDIR)/slides.pdf

coverage:
	cd $(CODEDIR) && sbt coverage test coverageReport

present: $(SLIDESDIR)/slides.pdf
	@zathura --fork slides/slides.pdf

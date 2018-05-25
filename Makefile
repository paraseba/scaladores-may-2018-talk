PHONY: cleantex cleancode clean testcode testtex test all console sbt pdf coverage
.DEFAULT_GOAL := all

CODE=./code/monoid
PDFLATEX=latexmk -pdf -interaction=nonstopmode -file-line-error

all: slides.pdf test

cleantex:
	latexmk -C

cleancode:
	cd $(CODE) && sbt clean

clean: cleantex cleancode

testcode:
	cd $(CODE) && sbt test

testtex: slides.pdf

test: testtex testcode

slides.pdf: slides.tex additive-color.pdf_tex tree.tex functional-programming-in-scala.png assoc.png
	$(PDFLATEX) $<

console:
	cd $(CODE) && sbt console

sbt:
	cd $(CODE) && sbt

pdf: slides.pdf

coverage:
	cd $(CODE) && sbt coverage test coverageReport

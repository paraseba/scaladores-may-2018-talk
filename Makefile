PHONY: cleantex cleancode clean testcode testtex test all console sbt pdf
.DEFAULT_GOAL := all

CODE=./code/monoid
PDFLATEX=latexmk -pdf -interaction=nonstopmode -file-line-error

all: monoid-presentation.pdf test

cleantex:
	latexmk -C

cleancode:
	cd $(CODE) && sbt clean

clean: cleantex cleancode

testcode:
	cd $(CODE) && sbt test

testtex: monoid-presentation.pdf

test: testtex testcode

monoid-presentation.pdf: monoid-presentation.tex additive-color.pdf_tex tree.tex functional-programming-in-scala.png assoc.png
	$(PDFLATEX) $<

console:
	cd $(CODE) && sbt console

sbt:
	cd $(CODE) && sbt

pdf: monoid-presentation.pdf

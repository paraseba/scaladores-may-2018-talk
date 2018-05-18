PHONY: cleantex cleancode clean testcode testtex test all

CODE=./code/monoid

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
	latexmk -pdf monoid-presentation.tex

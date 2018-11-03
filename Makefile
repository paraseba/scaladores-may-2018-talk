# Copyright 2018 Sebastian B. Galkin

# This file is part of paraseba/scaladores-may-2018-talk.

# paraseba/scaladores-may-2018-talk is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# paraseba/scaladores-may-2018-talk is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

PHONY: cleantex cleancode clean testcode testtex test all console sbt pdf coverage present

.DEFAULT_GOAL := all

CODEDIR=./code/monoid
PDFLATEX=latexmk -interaction=nonstopmode -file-line-error -auxdir=tmp -outdir=tmp
SLIDESDIR1=slides/part1

all: test

cleantex:
	cd $(SLIDESDIR1) && $(PDFLATEX) -C part1.tex
	cd $(SLIDESDIR2) && $(PDFLATEX) -C part2.tex

cleancode:
	cd $(CODEDIR) && sbt clean

clean: cleantex cleancode

testcode:
	cd $(CODEDIR) && sbt test

testtex: $(SLIDESDIR1)/part1.pdf

test: testtex testcode

$(SLIDESDIR1)/part1.pdf: $(SLIDESDIR1)/part1.tex $(SLIDESDIR1)/tree.tex $(SLIDESDIR1)/functional-programming-in-scala.png
	cd $(SLIDESDIR1) && $(PDFLATEX) -pdf part1.tex && cp tmp/part1.pdf .

console:
	cd $(CODEDIR) && sbt console

sbt:
	cd $(CODEDIR) && sbt

pdf1: $(SLIDESDIR1)/part1.pdf

coverage:
	cd $(CODEDIR) && sbt coverage test coverageReport

present1: $(SLIDESDIR1)/part1.pdf
	@zathura --fork $(SLIDESDIR1)/part1.pdf

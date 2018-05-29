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

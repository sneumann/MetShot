LATEXFILES = *.aux\
        *.bbl\
        *.blg\
        *.ilg\
        *.log\
        *.nlo\
        *.nls\
        *.toc\
        *.aux\
        MetShot.tex\
        Rplots.pdf\
	MetShot-plotspectrum.pdf\
        *.dvi\
        *.map\
        *.out\
        MetShot-*.pdf\
        MetShot.R

all: install vignette clean

setvars:
ifeq (${R_HOME},)
R_HOME= $(shell R RHOME)
endif

install:
	"$(R_HOME)/bin/R" CMD INSTALL ..

vignette:
	"$(R_HOME)/bin/R" CMD Sweave MetShot.Rnw
	pdflatex MetShot.tex
	bibtex MetShot
	pdflatex MetShot.tex
	pdflatex MetShot.tex

clean:
	rm -f $(LATEXFILES)
	rm -f *~

# Makefile for generating the stanUtils R package
#
PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)
R_FILES := $(wildcard R/*.R)
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES)

ifeq ($(OS),Windows_NT) 
	RM = rm -rf
	CP = cp -f
	CD = cd
else
	RM = rm -rf
	CP = cp -f
	CD = cd
endif

#all: install clean

#./inst/doc/*.html: ./vignettes/*.Rmd
#	R --vanilla -e 'devtools::build_vignettes()'
	
#install: $(PKG_FILES) ./inst/doc/*.html
#	Rcmd INSTALL --build .
	
build: $(PKG_FILES)
	Rscript version_update.R
	Rcmd INSTALL --build .
	R --vanilla -e 'devtools::build_vignettes()'
	Rcmd INSTALL --build .

#DESCRIPTION NAMESPACE: $(R_FILES)
#	Rscript version_update.R

clean:
	$(RM) $(PKG_NAME)_*.zip
	$(RM) man/

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

all: install clean

./inst/doc/*.html: ./vignettes/*.Rmd
	Rcmd INSTALL --build .
	Rscript -e "library(devtools); build_vignettes()"
	
install: $(PKG_FILES) ./inst/doc/*.html
	Rcmd build --no-build-vignettes .
	Rcmd INSTALL --html .
	
DESCRIPTION NAMESPACE: $(R_FILES)
	Rscript version_update.R

clean:
	$(RM) $(PKG_NAME)_*.zip
	$(RM) $(PKG_NAME)_*.tar.gz
	$(RM) man/

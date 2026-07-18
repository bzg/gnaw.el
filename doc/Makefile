# Makefile - build the Gnaw manual as Info, PDF or HTML
#
# Building needs Emacs 29 or later (for the Org 9.6 kbd macro),
# makeinfo for the Info and HTML outputs, and texi2pdf for the PDF.

EMACS        ?= emacs
BATCH         = $(EMACS) -Q --batch
MAKEINFO     ?= makeinfo
TEXI2PDF     ?= texi2pdf
INSTALL_INFO ?= install-info
prefix       ?= /usr/local
infodir      ?= $(prefix)/share/info

.PHONY: all info pdf html install uninstall clean

all: info

info: gnaw.info
pdf: gnaw.pdf
html: gnaw.html

gnaw.texi: gnaw-manual.org
	$(BATCH) --eval "(require 'ox-texinfo)" \
	  gnaw-manual.org -f org-texinfo-export-to-texinfo

gnaw.info: gnaw.texi
	$(MAKEINFO) --no-split $< -o $@

gnaw.pdf: gnaw.texi
	$(TEXI2PDF) --batch --clean $<

gnaw.html: gnaw.texi
	$(MAKEINFO) --no-split --html $< -o $@

install: gnaw.info
	install -d $(DESTDIR)$(infodir)
	install -m 644 gnaw.info $(DESTDIR)$(infodir)
	$(INSTALL_INFO) --info-dir=$(DESTDIR)$(infodir) gnaw.info

uninstall:
	$(INSTALL_INFO) --info-dir=$(DESTDIR)$(infodir) --remove gnaw.info
	rm -f $(DESTDIR)$(infodir)/gnaw.info

clean:
	rm -f gnaw.texi gnaw.info gnaw.pdf gnaw.html \
	  gnaw.aux gnaw.cp gnaw.cps gnaw.fn gnaw.fns gnaw.ky gnaw.kys \
	  gnaw.log gnaw.toc gnaw.vr gnaw.vrs gnaw.tp gnaw.tps gnaw.pg

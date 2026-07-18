# Makefile - byte-compile gnaw.el and build the manual (see doc/Makefile)

EMACS ?= emacs
export EMACS

.PHONY: all compile info pdf html install uninstall clean

all: compile info

compile: gnaw.elc

gnaw.elc: gnaw.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile gnaw.el

info pdf html install uninstall:
	$(MAKE) -C doc $@

clean:
	rm -f gnaw.elc
	$(MAKE) -C doc clean

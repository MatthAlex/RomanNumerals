# Start of the makefile
# Shell
SHELL=/bin/sh

package = RomanNum
version ?= 0.9.2
tarname = $(package)
distdir = $(tarname)-$(version)

# Compiler variable,ie: ifort, mpif90, nagfor, etc..
FC = gfortran
FCFLAGS ?= -fbacktrace -Ofast
FCDEBUG ?= -fbacktrace -Wall -Wextra -fbounds-check -fasynchronous-unwind-tables -g
export FC FCFLAGS FCDEBUG

prefix		=/usr/local
exec_prefix =$(prefix)
bindir		=$(exec_prefix)/bin
export prefix exec_prefix bindir

.PHONY: all check clean install installcheck dist FORCE distcheck uninstall
# debug 

all install uninstall:
	$(MAKE) -C src $@

check: all
	$(MAKE) -C test check

clean:
	$(MAKE) -C src clean
	$(MAKE) -C test clean

dist: $(distdir).tar.gz

$(distdir).tar.gz: $(distdir)
	tar chof - $(distdir) | gzip -9 -c > $@
	rm -rf $(distdir)

$(distdir): FORCE
	mkdir -p $(distdir)/src
	mkdir -p $(distdir)/test
	cp makefile $(distdir)
	cp test/makefile $(distdir)/test
	cp test/*.f90 $(distdir)/test
	cp src/makefile $(distdir)/src
	cp src/*.f90 $(distdir)/src

distcheck: $(distdir).tar.gz
	gzip -cd $(distdir).tar.gz | tar xvf -
	cd $(distdir) && $(MAKE) all
	cd $(distdir) && $(MAKE) check
	cd $(distdir) && $(MAKE) DESTDIR=$${PWD}/inst install
	cd $(distdir) && $(MAKE) DESTDIR=$${PWD}/inst uninstall
	@remaining="`find $${PWD}/$(distdir)/_inst -type f | wc -l`"; \
	if test "$${remaining}" -ne 0; then \
	echo "*** $${remaining} file(s) remaining in stage directory!"; \
	exit 1; \
	fi
	cd $(distdir) && $(MAKE) clean
	rm -rf $(distdir)
	@echo "*** Package $(distdir).tar.gz is ready for distribution."

installcheck:
	which RomanNum
	@echo '=$(bindir)/RomanNum'

FORCE:
	-rm $(distdir).tar.gz >/dev/null 2>&1
	-rm -rf $(distdir) >/dev/null 2>&1

Makefile: Makefile.in config.status
	./config.status $@

config.status: configure
	./config.status --recheck

#debug:
#	$(MAKE) -C src debug


# End of the makefile

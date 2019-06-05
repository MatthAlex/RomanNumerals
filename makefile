# Start of the makefile
# Shell
SHELL=/bin/sh

package = RomanNum
version = 0.9.2
tarname = $(package)
distdir = $(tarname)-$(version)

INSTALL_DIR=/usr/local/bin/

.PHONY: all check clean install installcheck dist FORCE distcheck
# debug 

all:
	$(MAKE) -C src all

check: all
	$(MAKE) -C test check

clean:
	$(MAKE) -C src clean
	$(MAKE) -C test clean

install: all
	sudo install -D src/RomanNum $(INSTALL_DIR)

installcheck: install
	which RomanNum
	@echo '=/usr/local/bin/RomanNum'

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
	cd $(distdir) && $(MAKE) clean
	rm -rf $(distdir)
	@echo "*** Package $(distdir).tar.gz is ready for distribution."

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

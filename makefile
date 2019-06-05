# Start of the makefile
# Shell
SHELL=/bin/sh

package = RomanNum
version = 0.9.2
tarname = $(package)
distdir = $(tarname)-$(version)

INSTALL_DIR=/usr/local/bin/

.PHONY: all check clean install installcheck
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

Makefile: Makefile.in config.status
	./config.status $@

config.status: configure
	./config.status --recheck

#debug:
#	$(MAKE) -C src debug


# End of the makefile

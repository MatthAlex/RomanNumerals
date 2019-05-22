# Start of the makefile
# Shell
SHELL=/bin/sh
INSTALL_DIR=/usr/local/bin/

all: install
	$(MAKE) -C src all

check: all
	$(MAKE) -C test check

debug:
	$(MAKE) -C src debug

clean:
	$(MAKE) -C src clean
	$(MAKE) -C test clean

install: installcheck
	sudo install -D src/RomanNum $(INSTALL_DIR)

installcheck:
	which RomanNum
	@echo '=/usr/local/bin/RomanNum'

.PHONY: all check clean debug install installcheck

# End of the makefile

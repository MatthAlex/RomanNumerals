# Start of the makefile
# Shell
SHELL = /bin/sh

all:
	$(MAKE) -C src all

check: all
	$(MAKE) -C test check

debug:
	$(MAKE) -C src debug

clean:
	$(MAKE) -C src clean
	$(MAKE) -C test clean

.PHONY: all check clean debug

# End of the makefile

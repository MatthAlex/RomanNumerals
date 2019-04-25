# Start of the makefile
# Shell
SHELL = /bin/sh

all:
	$(MAKE) -C src all

check: all
	$(MAKE) -C test check

clean:
	$(MAKE) -C src clean
	$(MAKE) -C test clean

.PHONY: all check clean

# End of the makefile

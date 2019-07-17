TESTS          = test test2 test3 test4
check_PROGRAMS = test test2 test3 test4

AM_DEFAULT_SOURCE_EXT = .f90

AM_FCFLAGS = -I$(top_builddir)/common
AM_LDFLAGS = $(top_builddir)/common/libcommon.a

# check_SCRIPTS = greptest.sh
# TESTS = $(check_SCRIPTS)

# greptest.sh:
#          echo './jupiter | grep "Hello from .*jupiter!"' > greptest.sh
#          chmod +x greptest.sh

CLEANFILES = test test2 test3 test4 log.dat

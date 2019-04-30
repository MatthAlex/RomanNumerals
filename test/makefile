# Start of the makefile
# Shell
SHELL = /bin/sh

# Define variables
FC = gfortran
SRC_DIR = ../src
TEST_PROGS = test test2 test3 test4
TEST_OBJECTS = $(SRC_DIR)/parameters.o $(SRC_DIR)/routines.o
# ifort, mpif90, nagfor, etc..
FCFLAGS = -fbacktrace -Ofast -I$(SRC_DIR)
FCDEBUG = -fbacktrace -Wall -Wextra -fbounds-check -fasynchronous-unwind-tables -g -I$(SRC_DIR)
runAll = $(addprefix run_,$(TEST_PROGS))

.PHONY: check clean all cleanLog $(runAll)

# Makefile

$(TEST_PROGS): %: %.f90 $(TEST_OBJECTS)
	$(FC) -o $@ $(FCDEBUG) $^

# % does the pattern matching
# $* picks up what the pattern matched is
# runAll is dependent on each of its components: run_test*
# each run_test* is dependent on the test* executable
$(runAll): run_%: %
	@echo Run $*
	@./$* >> log.dat

check: cleanLog $(runAll)

# Clean up
cleanLog:
	$(RM) log.dat

clean: cleanLog
	$(RM) $(TEST_PROGS)

# End of the makefile

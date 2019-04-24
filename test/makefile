# Start of the makefile
# Shell
SHELL = /bin/sh

# Define variables
TEST_OBJECTS = $(SRC_DIR)/parameters.o $(SRC_DIR)/routines.o
FC = gfortran
SRC_DIR = ../src
TEST_PROGS = test test2 test3 test4
# ifort, mpif90, nagfor, etc..
FCFLAGS = -fbacktrace -Ofast
FCDEBUG = -fbacktrace -Wall -fbounds-check -fasynchronous-unwind-tables -g

.PHONY: testing check clean

# Makefile
$(TEST_PROGS): %: %.f90
	$(FC) -o $@ $^ $(FCDEBUG) $(TEST_OBJECTS)
	@echo "\nRunning test...\n"
	@$@ | awk '/Test/ {printf $0} /stop/ {printf $0}'

# $(TEST_OBJECTS): %.o %.f90

$(TEST_OBJECTS): %.o: %.f90
	$(FC) -c $(FCFLAGS) $(SRC_DIR)/$<

testing: $(TEST_OBJECTS) $(TEST_PROGS)

check: clean testing

# Dependencies
routines.o: parameters.o

# Clean up
clean:
	$(RM) $(TEST_PROGS)


# End of the makefile

# Start of the makefile
# Shell
SHELL = /bin/sh

# Define variables
OBJECTS = parameters.o routines.o
MAIN_PROG = RomanNum
TEST_DIR = ../test
TEST_PROGS = $(TEST_DIR)/test $(TEST_DIR)/test2 $(TEST_DIR)/test3 $(TEST_DIR)/test4
FC = gfortran
# ifort, mpif90, nagfor, etc..
FCFLAGS = -fbacktrace -Ofast
FCDEBUG = -fbacktrace -Wall -fbounds-check -fasynchronous-unwind-tables -g


# Makefile
build: $(OBJECTS)
	$(FC) -o $(MAIN_PROG) $(MAIN_PROG).f90 $(FCFLAGS) $(OBJECTS)

%.o: %.f90
	$(FC) -c $(FCFLAGS) $<

debug: $(OBJECTS)
	$(FC) -o $(MAIN_PROG) $(MAIN_PROG).f90 $(FCDEBUG) $(OBJECTS)

$(TEST_PROGS): %: %.f90
	$(FC) -o $@ $^ $(FCDEBUG) $(OBJECTS)
	@echo "\nRunning test...\n"
	@$@ | awk '/Test/ {printf $0} /stop/ {printf $0}'

.PHONY: testing
testing: $(OBJECTS) $(TEST_PROGS)
	
.PHONY: check
check: clean testing

# Dependencies
routines.o: parameters.o

# Clean up
.PHONY: clean
clean:
	$(RM) $(OBJECTS) RomanNum $(OBJECTS:.o=.mod)
	$(RM) $(TEST_PROGS)

# End of the makefile

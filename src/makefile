# Start of the makefile
# Shell
SHELL = /bin/sh

# Define variables
OBJECTS = parameters.o routines.o
MAIN_PROG = RomanNum
FC = gfortran
# ifort, mpif90, nagfor, etc..
FCFLAGS = -fbacktrace -Ofast
FCDEBUG = -fbacktrace -Wall -Wextra -fbounds-check -fasynchronous-unwind-tables -g

.PHONY: clean all debug

# Makefile
all: $(OBJECTS)
	$(FC) -o $(MAIN_PROG) $(MAIN_PROG).f90 $(FCFLAGS) $(OBJECTS)

%.o: %.f90
	$(FC) -c $(FCFLAGS) $<

debug: $(OBJECTS)
	$(FC) -o $(MAIN_PROG) $(MAIN_PROG).f90 $(FCDEBUG) $(OBJECTS)

# Dependencies
routines.o: parameters.o

# Clean up
clean:
	$(RM) $(OBJECTS) RomanNum $(OBJECTS:.o=.mod)

# End of the makefile

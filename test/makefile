# Start of the makefile
# Shell
SHELL = /bin/sh

# Define variables
objects = parameters.o routines.o
FC = gfortran
# ifort, mpif90, nagfor, etc..
FCFLAGS   = -fbacktrace -Ofast
FCDEBUG = -fbacktrace -Wall -fbounds-check


# Makefile
build: $(objects)
	$(FC) -o RomanNum RomanNum.f90 $(FCFLAGS) $(objects)
#routines.o: routines.f90
#	$(FC) -c routines.f90
#parameters.o: parameters.f90
#	$(FC) -c parameters.f90
%.o: %.f90
	$(FC) -c $(FCFLAGS) $<

debug: $(objects)
	$(FC) -o RomanNum RomanNum.f90 $(FCDEBUG) $(objects)
%.o: %.f90
	$(FC) -c $(FCDEBUG) $<

test: $(objects)
	$(FC) -o test test.f90 $(FCDEBUG) $(objects)
%.o: %.f90
	$(FC) -c $(FCDEBUG) $<

test2: $(objects)
	$(FC) -o test2 test2.f90 $(FCDEBUG) $(objects)
%.o: %.f90
	$(FC) -c $(FCDEBUG) $<

test3: $(objects)
	$(FC) -o test3 test3.f90 $(FCDEBUG) $(objects)
%.o: %.f90
	$(FC) -c $(FCDEBUG) $<

test4: $(objects)
	$(FC) -o test4 test4.f90 $(FCDEBUG) $(objects)
%.o: %.f90
	$(FC) -c $(FCDEBUG) $<
	
# Dependencies
routines.o: parameters.o

# Clean up
clean:
	@rm -f $(objects) test $(objects:.o=.mod)
	@rm -f $(objects) test2 $(objects:.o=.mod)
	@rm -f $(objects) test3 $(objects:.o=.mod)
	@rm -f $(objects) test4 $(objects:.o=.mod)


# End of the makefile

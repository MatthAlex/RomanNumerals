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
	
# Dependencies
routines.o: parameters.o

# Clean up
clean:
	@rm -f $(objects) RomanNum $(objects:.o=.mod)



# End of the makefile

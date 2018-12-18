# Start of the makefile
# Shell
SHELL = /bin/sh

# Define variables
objects = parameters.o routines.o
FC = gfortran-8	
# ifort, mpif90, nagfor, etc..
genFlags   = -fbacktrace -Ofast
debugFlags = # -Wall -fbounds-check


# Makefile
RomanNum: $(objects)
	$(FC) -o RomanNum RomanNum.f90 $(genFlags) $(debugFlags) $(objects)
#routines.o: routines.f90
#	$(FC) -c routines.f90
#parameters.o: parameters.f90
#	$(FC) -c parameters.f90
%.o: %.f90
	$(FC) -c $(debugFlags) $(genFlags) $<
	
# Dependencies
routines.o: parameters.o

# Clean up
clean:
	@rm -f $(objects) RomanNum $(objects:.o=.mod)



# End of the makefile

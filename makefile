# This is an commentary line in a makefile
# Start of the makefile

# Define variables
objects = parameters.o routines.o
f90comp = gfortran-8	# ifort, mpif90, etc..
genFlags   = -fbacktrace -Ofast
debugFlags = # -Wall -fbounds-check


# Makefile
RomanNum: $(objects)
	$(f90comp) -o RomanNum RomanNum.f90 $(genFlags) $(debugFlags) $(objects)
#routines.o: routines.f90
#	$(f90comp) -c routines.f90
#parameters.o: parameters.f90
#	$(f90comp) -c parameters.f90
%.o: %.f90
	$(f90comp) -c $(debugFlags) $(genFlags) $<

# Clean up
clean:
	rm $(objects) RomanNum



# End of the makefile

# Start of the makefile
# Shell
SHELL = /bin/sh

# Define variables
OBJECTS = parameters.o routines.o
# MAIN_PROG = RomanNum
FC = gfortran
# ifort, mpif90, nagfor, etc..
FCFLAGS = -fbacktrace -Ofast
FCDEBUG = -fbacktrace -Wall -Wextra -fbounds-check -fasynchronous-unwind-tables -g

.PHONY: clean all debug

# Makefile
all: RomanNum

RomanNum: $(OBJECTS)
	$(FC) -o $@ $(FCFLAGS) $@.f90 $(OBJECTS)

%.o: %.f90
	$(FC) -c $(FCFLAGS) $<

# Dependencies
routines.o: parameters.o

# Clean up
clean:
	@$(RM) $(OBJECTS) $(RomanNum) $(OBJECTS:.o=.mod)

# debug: $(OBJECTS)
# 	$(FC) -o $(MAIN_PROG) $(FCDEBUG) $(MAIN_PROG).f90 $(OBJECTS)

# End of the makefile
# $@ refers to the full target name of the current target or the archive filename part of a library archive target. This variable is valid in both explicit and implicit rules.
# $% refers to a member of an archive and is valid only when the current target is an archive member—that is, an object file that is a member of a static library. This variable is valid in both explicit and implicit rules.
# $? refers to the list of dependencies that are newer than the current target. This variable is valid in both explicit and implicit rules.
# $< refers to the member of the dependency list whose existence allowed the rule to be chosen for the target. This variable is only valid in implicit rules.
# $* refers to the current target name with its suffix deleted. This variable is guaranteed by POSIX to be valid only in implicit rules.

#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([TaskRevisited], [0.9.2], [m.alexandrakis@qmul.ac.uk])
AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_SRCDIR([src/RomanNum.f90])

AM_INIT_AUTOMAKE([foreign subdir-objects])
LT_INIT
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# Find Fortran Compiler
AC_LANG_PUSH([Fortran])
AC_PROG_FC([ifort gfortran], [Fortran 95])
AC_FC_PP_SRCEXT([f90])
AC_LANG_POP([Fortran])

# _DEBUG_START_
# AC_CHECK_PROG([bash_var], [bash], [yes], [no],, [/usr/sbin/bash])
# _DEBUG_END_
# Checks for programs.
AC_PROG_INSTALL
AQM_PROG_FORD([no],,[AC_CONFIG_FILES([taskrev-ford.md])])
AM_CONDITIONAL(WITH_DOC,[test x"$FORD" != x])

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

# --enable-maintainer-mode
AM_MAINTAINER_MODE

AC_CONFIG_FILES([Makefile common/Makefile src/Makefile test/Makefile doc/Makefile])
AC_OUTPUT

echo \
"-------------------------------------------------

 ${PACKAGE_NAME} Version ${PACKAGE_VERSION}

 Prefix: '${prefix}'.
 Compiler: '${FC} ${FCFLAGS}'

 Package features:
 Async Execution:  No
 Maintaner mode: Disabled
 To enable it: --enable-maintainer-mode
 
 Now type 'make @<:@<target>@:>@'
 where the optional <target> is:
 all                - build all binaries
 install            - install everything
 installcheck
 clean              - clean everything
 dist               - create a distribution package
 check              - run tests

 Installed at: `which RomanNum`

--------------------------------------------------"

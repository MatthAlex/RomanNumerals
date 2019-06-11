# Control whether we use Ford to document Fortran source
#
#  AQM_PROG_FORD([DEFAULT],[VERSION],[ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
#
# If an appropriate Ford program is found, the variable FORD is set to its
# path.  To be appropriate:
#   - $FORD -h must have output containing the word "Fortran";
#   - if VERSION is given, $FORD -V must report a version at least that.
#
# The variable FORD may be set on entry to specify the path used.
#
# DEFAULT gives the default state for use of Ford.
#
# If an appropriate Ford program is found, ACTION-IF-FOUND is evaluated if
# provided; if ACTION-IF-FOUND is not provided and an appropriate Ford
# program is found, the variable HAVE_FORD is defined. If there is no Ford
# program found,  ACTION-IF-NOT-FOUND is evaluated if provided.
#
# It may be useful to have in ACTION-IF-FOUND something like
#    [AC_CONFIG_FILES([project.md])]
# with the corresponding Ford driver file project.md.in.  Further, after the
# reference to AQM_PROG_FORD, an Automake conditional like
#    AM_CONDITIONAL(WITH_DOC,[test x"$FORD" != x])
dnl
AC_DEFUN([AQM_PROG_FORD], [
  AC_PREREQ([2.50])
  AC_PROG_SED
  AC_ARG_VAR([FORD],[Path of the Ford documentation generator])
  AC_ARG_WITH([ford],
    [AS_HELP_STRING([--with-ford[[=yes/no/PATH]]],
      [use Ford to document program [default: $1]])
  ],[
    use_ford=$withval
  ],[
    use_ford="$1"
  ])
  AC_MSG_CHECKING([whether to build documentation using Ford])
  AS_IF([test x"$use_ford" == xno], [AC_MSG_RESULT([no])], [
    AC_MSG_RESULT([yes])
    AS_IF([test x"$use_ford" != xyes], [FORD=$use_ford])
    AC_PATH_PROG([FORD],[ford])
    with_ford="no"
    AS_IF([test -x "$FORD"],[
      AC_MSG_CHECKING([whether the Ford executable understands Fortran])
      AS_IF([$FORD -h | grep -q Fortran], [
        AC_MSG_RESULT([yes])
        AS_IF([test -n "$2"],[
          AC_MSG_CHECKING([version of Ford at least $2])
          ford_version=`$FORD -V 2>&1 | $SED 's/^FORD,\ version\ //;q'`
          AX_COMPARE_VERSION([$2],[le],[$ford_version],[
            AC_MSG_RESULT([yes ($ford_version)])
            with_ford="yes"
          ],[
            AC_MSG_RESULT([no ($ford_version)])
          ])
        ], with_ford="yes")
      ], [
        AC_MSG_RESULT([no])
      ])
    ],[
      AS_IF([test -n "$FORD"],[
        AC_MSG_CHECKING([whether the Ford executable exists])
        AC_MSG_RESULT([no])
      ])
    ])
  ])

AS_IF([test x"$with_ford" = x"yes"], [
  ifelse([$3],,[AC_DEFINE(HAVE_FORD,1,[Define if using Ford])],[$3])
  :
  ], [
  $4
  :
  ])
])dnl AQM_PROG_FORD

!> Intention:
!> pass the command line provided string with a fixed length, into a string of deferred length,
!> after adjusting characters to left, removing all spaces (trailing/leading), and turn characters
!> to upper case.
!> Test cases include strings of various lengths, which include spaces in various positions.

program test
use routines, only: buffer_to_string

implicit none
integer :: error
character(LEN=:), allocatable :: test_string   !! output

! ------ Invalid input -------------
call buffer_to_string('', test_string, error)
if (error.ne.1) stop 1


! -------- Valid Input -------------
!test_input = 'a' ! character(LEN=1)
call buffer_to_string('a', test_string, error)
if (error.ne.0) stop 2
if (test_string.ne.'A') stop 2

!test_input = ' a' ! character(LEN=2) w/ leading space
call buffer_to_string(' a', test_string, error)
if (error.ne.0) stop 3
if (test_string.ne.'A') stop 3

!test_input = 'a ' ! character(LEN=2) w/trailing space
call buffer_to_string('a ', test_string, error)
if (error.ne.0) stop 4
if (test_string.ne.'A') stop 4

!test_input = ' I ' ! character(LEN=3) w/trailing and leading spaces
call buffer_to_string(' I ', test_string, error)
if (error.ne.0) stop 5
if (test_string.ne.'I') stop 5

!test_input = 'a123456789123456789' ! character(LEN=19)
call buffer_to_string('a123456789123456789', test_string, error)
if (error.ne.0) stop 6
if (test_string.ne.'A123456789123456789') stop 6

!test_input = 'a123456789  123' ! character(LEN=15) w/ spaces injected in the middle
call buffer_to_string('a123456789  123', test_string, error)
if (error.ne.0) stop 7
if (test_string.ne.'A123456789  123') stop 7

!test_input = 'a/12516adg`+]][' ! various ascii characters (LEN=15)
call buffer_to_string('a/12516adg`+]][', test_string, error)
if (error.ne.0) stop 8
if (test_string.ne.'A/12516ADG`+]][') stop 8

end program test

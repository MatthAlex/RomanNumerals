!> Intention:
!> Test that a number from 1-3999 can be represented as a Roman Numeral.

program test_number_to_numeral
use routines, only: number_to_numeral

implicit none
integer :: i, ios
character(LEN=20) :: test_input_raw   !! input string with enough space for any Roman Numeral from 1-3999
character(LEN=:), allocatable :: test_input !! input string with enough space for any Roman Numeral from 1-3999
character(LEN=:), allocatable :: test_output  !! output
integer, parameter :: read_unit = 99

i = 0

open(unit=read_unit, file='ronsPlain.txt', iostat=ios)
if ( ios /= 0 ) stop 77

do
    read(read_unit, '(A20)', iostat=ios) test_input_raw
    if (ios /= 0) exit

    test_input = TRIM(ADJUSTL(test_input_raw))

    i = i + 1
    call number_to_numeral(i, test_output)
    if (test_output.ne.test_input) stop 1

end do

close(read_unit)

end program test_number_to_numeral

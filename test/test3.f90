! Intention:
! Validate that a string of roman numeral characters has the correct
! numeral structure, and stop if incorrect.

program test_numeral_to_number
use routines, only: numeral_to_number

implicit none
integer :: ios
character(LEN=20) :: buffer !! large enough buffer to hold string
character(LEN=:), allocatable :: test_input   !! input string with enough space for any Roman Numeral from 1-3999
integer, parameter :: read_unit = 99

open(unit=read_unit, file='ronsPlain.txt', iostat=ios)
if ( ios /= 0 ) stop 77 !"Error opening file ronsPlain.txt"

do
    read(read_unit, '(A20)', iostat=ios) buffer
    if (ios /= 0) exit

    test_input = TRIM(buffer)
    if (numeral_to_number(test_input).eq.-1) stop 21

end do

close(read_unit)

! Invalid numerals test cases

! invalid (Two double value numerals in succession)
if (numeral_to_number('IXIV').ne.-1) stop 1

! invalid (Two double value numerals in succession)
if (numeral_to_number('XCXL').ne.-1) stop 2

! invalid (Two double value numerals in succession)
if (numeral_to_number('CMCD').ne.-1) stop 3

! invalid (INVALID NUMERAL SUCCESSION. CM and C found together)
if (numeral_to_number('CMCX').ne.-1) stop 5

! invalid (INVALID NUMERAL SUCCESSION. CD and C found together)
if (numeral_to_number('CDCX').ne.-1) stop 6

! invalid (INVALID NUMERAL SUCCESSION. XC and X found together)
if (numeral_to_number('XCX').ne.-1) stop 7

! invalid (INVALID NUMERAL SUCCESSION. XL and X found together)
if (numeral_to_number('XLX').ne.-1) stop 8

! invalid (INVALID NUMERAL SUCCESSION. I and IX found together)
if (numeral_to_number('IXI').ne.-1) stop 9

! invalid (INVALID NUMERAL SUCCESSION. I and IV found together)
if (numeral_to_number('IVI').ne.-1) stop 10

! invalid number of consecutive numerals
if (numeral_to_number('IIII').ne.-1) stop 11

! invalid number of consecutive numerals
if (numeral_to_number('VV').ne.-1) stop 12

! invalid number of consecutive numerals
if (numeral_to_number('XXXX').ne.-1) stop 13

! invalid number of consecutive numerals
if (numeral_to_number('LL').ne.-1) stop 14

! invalid number of consecutive numerals
if (numeral_to_number('CCCC').ne.-1) stop 15

! invalid number of consecutive numerals
if (numeral_to_number('MMMM').ne.-1) stop 16

! invalid (Double value numeral along with smaller numeral of the same order)
if (numeral_to_number('IXV').ne.-1) stop 17

! invalid (Double value numeral along with smaller numeral of the same order)
if (numeral_to_number('XCL').ne.-1) stop 18

! invalid (Double value numeral along with smaller numeral of the same order)
if (numeral_to_number('CMD').ne.-1) stop 19

! invalid (invalid numeral succession)
if (numeral_to_number('XIXC').ne.-1) stop 20

end program test_numeral_to_number

program test_numeral_to_number
use routines

implicit none
integer :: error, number, ios
character(LEN=20) :: buffer   !! input
character(LEN=:), allocatable   :: StringValue   !! output
integer, parameter :: read_unit = 99

open(unit=read_unit, file='ronsPlain.txt', iostat=ios)
if ( ios /= 0 ) stop "Error opening file data.dat"

do
    read(read_unit, '(A20)', iostat=ios) buffer
    if (ios /= 0) exit

    call buffer_to_string(buffer, StringValue, error)
    call verify_string(StringValue, error) 
    number = numeral_to_number(StringValue)
    if (number.eq.-1) stop 1

end do

close(read_unit)

! Invalid numerals test cases

! invalid (Two double value numerals in succession)
number = numeral_to_number('IXIV')
    if (number.ne.-1) stop 1

! invalid (Two double value numerals in succession)
number = numeral_to_number('XCXL')
    if (number.ne.-1) stop 2

! invalid (Two double value numerals in succession)
number = numeral_to_number('CMCD')
    if (number.ne.-1) stop 3

! invalid (Two double value numerals in succession)
!number = numeral_to_number('IXIV')
!    if (number.ne.-1) stop 4

! invalid (INVALID NUMERAL SUCCESSION. CM and C found together)
number = numeral_to_number('CMCX')
    if (number.ne.-1) stop 5

! invalid (INVALID NUMERAL SUCCESSION. CD and C found together)
number = numeral_to_number('CDCX')
    if (number.ne.-1) stop 6

! invalid (INVALID NUMERAL SUCCESSION. XC and X found together)
number = numeral_to_number('XCX')
    if (number.ne.-1) stop 7

! invalid (INVALID NUMERAL SUCCESSION. XL and X found together)
number = numeral_to_number('XLX')
    if (number.ne.-1) stop 8

! invalid (INVALID NUMERAL SUCCESSION. I and IX found together)
number = numeral_to_number('IXI')
    if (number.ne.-1) stop 9

! invalid (INVALID NUMERAL SUCCESSION. I and IV found together)
number = numeral_to_number('IVI')
    if (number.ne.-1) stop 10

! invalid number of consecutive numerals
number = numeral_to_number('IIII')
    if (number.ne.-1) stop 11

! invalid number of consecutive numerals
number = numeral_to_number('VV')
    if (number.ne.-1) stop 12

! invalid number of consecutive numerals
number = numeral_to_number('XXXX')
    if (number.ne.-1) stop 13

! invalid number of consecutive numerals
number = numeral_to_number('LL')
    if (number.ne.-1) stop 14

! invalid number of consecutive numerals
number = numeral_to_number('CCCC')
    if (number.ne.-1) stop 15

! invalid number of consecutive numerals
number = numeral_to_number('MMMM')
    if (number.ne.-1) stop 16

! invalid (Double value numeral along with smaller numeral of the same order)
number = numeral_to_number('IXV')
    if (number.ne.-1) stop 17

! invalid (Double value numeral along with smaller numeral of the same order)
number = numeral_to_number('XCL')
    if (number.ne.-1) stop 18

! invalid (Double value numeral along with smaller numeral of the same order)
number = numeral_to_number('CMD')
    if (number.ne.-1) stop 19

! invalid (invalid numeral succession)
number = numeral_to_number('XIXC')
    if (number.ne.-1) stop 20

!write(*,*) "Ignore 'ERROR' messages. Don't ignore warnings by the compiler."
write(*,'(A16)') "Test successful!"

end program test_numeral_to_number

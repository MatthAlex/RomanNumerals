! Intention:
! Verify that the string passed contains only valid Numeral characters.
! Inclusion of any other character should invalidate the input.

program test_verifyString
use routines, only: verify_string

implicit none
integer :: error, i
character(LEN=:), allocatable :: test_string   !! input

do i = 1, 127
    test_string = ACHAR(i)

    call verify_string(test_string, error)
    if (error.ne.1) then
        if (VERIFY(test_string, "IVXLCDM").ne.0) stop 20
    end if
end do

!-------------- List of valid inputs -----------------
! valid character
call verify_string('I', error)
if (error.ne.0) stop 1

! valid string
call verify_string('IXC', error)
if (error.ne.0) stop 2

! valid string
call verify_string('CCCCCMCMCMCMMXMXMMII', error)
if (error.ne.0) stop 3

!-------------- List of invalid inputs ----------------
! invalid alphanumeric input
call verify_string('a15', error)
if (error.ne.1) stop 4

! invalid characters
call verify_string('bb', error)
if (error.ne.1) stop 5

! invalid string
call verify_string('CCCCCMCMCMCMMXCaas2d', error)
if (error.ne.1) stop 6

! invalid string
call verify_string('I_op', error)
if (error.ne.1) stop 7

! invalid string due to lower case
call verify_string('IcvXMmXcxCiIXCMDLld', error)
if (error.ne.1) stop 8

! invalid string w/ spaces injected
call verify_string('CCCCCM  CMCMCM', error)
if (error.ne.1) stop 9

end program test_verifyString

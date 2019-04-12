program test_verifyString
use routines

implicit none
integer :: error, ios, i
character(LEN=20) :: buffer   !! input
character(LEN=:), allocatable   :: StringValue   !! output
integer, parameter :: read_unit = 99

open(unit=read_unit, file='asciiPlain.txt', iostat=ios)
if ( ios /= 0 ) stop "Error opening file asciiPlain.txt"

do
    read(read_unit, '(A20)', iostat=ios) buffer
    if (ios /= 0) exit

    call buffer_to_string(buffer, StringValue, error)
    call verify_string(StringValue, error)
    if (error.ne.1) then
        i = VERIFY(StringValue, "IVXLCDM")
        if (i.ne.0) stop 20
    end if
end do

close(read_unit)

! invalid
buffer = 'a15'
call buffer_to_string(buffer, StringValue, error)
call verify_string(StringValue, error)
if (error.ne.1) stop 1

! invalid
buffer = 'bb'
call buffer_to_string(buffer, StringValue, error)
call verify_string(StringValue, error)
if (error.ne.1) stop 2

! valid
buffer = 'I'
call buffer_to_string(buffer, StringValue, error)
call verify_string(StringValue, error)
if (error.ne.0) stop 3

! valid
buffer = 'IXC'
call buffer_to_string(buffer, StringValue, error)
call verify_string(StringValue, error)
if (error.ne.0) stop 4

! valid & truncated
buffer = 'CCCCCMCMCMCMMXMXMMIIIIIVXXCCC'
call buffer_to_string(buffer, StringValue, error)
call verify_string(StringValue, error)
if (error.ne.0) stop 5

! invalid string, but is truncated after LEN=20, turns to valid
buffer = 'CCCCCMCMCMCMMXMXMMIIIIIVXXCCC12512515'
call buffer_to_string(buffer, StringValue, error)
call verify_string(StringValue, error)
if (error.ne.0) stop 6

! invalid string
buffer = 'CCCCCMCMCMCMMXCaas2db'
call buffer_to_string(buffer, StringValue, error)
call verify_string(StringValue, error)
if (error.ne.1) stop 7

! invalid
buffer = 'I_op'
call buffer_to_string(buffer, StringValue, error)
call verify_string(StringValue, error)
if (error.ne.1) stop 8

! valid
buffer = 'IcvXMmXcxCiIXCMDLld'
call buffer_to_string(buffer, StringValue, error)
call verify_string(StringValue, error)
if (error.ne.0) stop 9

! invalid string & truncated @ ca^
buffer = 'CCCCCMCMCMCMMXCLdDcaas2db'
call buffer_to_string(buffer, StringValue, error)
call verify_string(StringValue, error)
if (error.ne.1) stop 10

!write(*,*) "Ignore 'ERROR' messages. Don't ignore warnings by the compiler."
write(*,'(A16)') "Test successful!"

end program test_verifyString

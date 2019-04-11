program test_verifyString
use routines

implicit none
integer :: error
character(LEN=20) :: buffer   !! input
character(LEN=:), allocatable   :: StringValue   !! output

! invalid
buffer = 'a'
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

write(*,*) "Ignore 'ERROR' messages. Don't ignore warnings by the compiler."
write(*,*) "Test successful!"

end program test_verifyString

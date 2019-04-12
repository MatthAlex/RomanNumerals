program test
use routines, only: buffer_to_string

implicit none
integer :: error
character(LEN=20) :: buffer   !! input
character(LEN=:), allocatable   :: StringValue   !! output

! input: buffer, output: StringValue, error
buffer = '' ! null string
call buffer_to_string(buffer, StringValue, error)
if (error.ne.1) stop 1

buffer = 'a' ! invalid character(LEN=1)
call buffer_to_string(buffer, StringValue, error)
if (error.ne.0) stop 2

buffer = ' a' ! invalid char(LEN=2) w/ leading space
call buffer_to_string(buffer, StringValue, error)
if (error.ne.0) stop 3

buffer = 'a ' ! invalid character(LEN=2) w/trailing space
call buffer_to_string(buffer, StringValue, error)
if (error.ne.0) stop 4

buffer = 'I' ! valid string(LEN=1)
call buffer_to_string(buffer, StringValue, error)
if (error.ne.0) stop 5

buffer = 'a123456789123456789' ! invalid character(LEN=27), Warned by Compiler
call buffer_to_string(buffer, StringValue, error)
if (error.ne.0) stop 6

buffer = 'a123456789  123' ! invalid character(LEN=27), Warned by Compiler
call buffer_to_string(buffer, StringValue, error)
if (error.ne.0) stop 7

buffer = 'a/12516adg`+]][' ! invalid characters
call buffer_to_string(buffer, StringValue, error)
if (error.ne.0) stop 8

!write(*,*) "Ignore 'ERROR' messages. Don't ignore warnings by the compiler."
write(*,'(A16)') "Test successful!"

end program test

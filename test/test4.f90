program test_number_to_numeral
    use routines, only: number_to_numeral, numeral_to_number
    
    implicit none
    integer :: i, number
    character(LEN=:), allocatable :: StringValue   !! output
    
    do i = 1, 3999
    
        call number_to_numeral(i, StringValue)
        number = numeral_to_number(StringValue)
        if (number.ne.i) stop 1

    end do

    write(*,'(A16)') "Test successful!"
    
end program test_number_to_numeral

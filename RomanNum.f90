	! This program takes the input of two Roman Numerals and produces and outputs the sum as a Numeral.
	! The valid Numeral values are I, V, X, L, C, D, and M and their lowercase.
	! Invalid input includes all other ASCII characters.
	  
	program numerals
	use parameters, only: debugMode
	use routines
	implicit none
	
	integer	:: Number_1, Number_2, Number_Sum, Number_Check_Sum, argument_count
	character(LEN=20)	:: command_line_argument_1, command_line_argument_2
	character(LEN=:), allocatable	:: Numeral_1, Numeral_2, Numeral_Sum

	! Check argument count and abort if different than 2
	argument_count = COMMAND_ARGUMENT_COUNT()
	if (argument_count .ne. 2) then
		write(*,*) 'ERROR: Only two command line arguments allowed.'
		stop
	end if 
	
	! Read arguments #1 and #2 from the command line 
	call GET_COMMAND_ARGUMENT(1, command_line_argument_1) 
	call GET_COMMAND_ARGUMENT(2, command_line_argument_2) 
	
	! Adjust to the left and trim trailing spaces of the strings, and then check for invalid ASCII characters
	call buffer_to_string(command_line_argument_1, Numeral_1) 
	call check_string_valid(Numeral_1) 
	
	call buffer_to_string(command_line_argument_2, Numeral_2) 
	call check_string_valid(Numeral_2) 
	
	! Translate numerals to numbers
	Number_1 = numeral_to_number(Numeral_1)
	Number_2 = numeral_to_number(Numeral_2)

	! Add the numbers 
	Number_Sum = Number_1 + Number_2

	! debug output 
	if (debugMode) write(*,*) Number_1, Number_2, Number_Sum

	! Translate numeral to number
	!Numeral_Sum = number_to_numeral(Number_Sum)
	call number_to_numeral(Number_Sum, Numeral_Sum)
	if (debugMode) write(*,*) 'Numeral_Sum=',Number_Sum, ' and LEN=',LEN(Numeral_Sum)

	! Check that the conversion holds true
	Number_Check_Sum = numeral_to_number(Numeral_Sum)
	if (Number_Check_Sum.ne.Number_Sum) then
		write(*,*) 'ERROR: The sum and numeral are not equal.'
		stop
	end if 
	
	write(*,*) Numeral_Sum
	

	end program numerals

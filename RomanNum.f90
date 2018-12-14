      program numerals
      use parameters, only: debugMode
      use routines
      implicit none
      
      integer(kind=4)               :: Number_1, Number_2, NumberSum
      character(LEN=20)             :: command_line_argument_1, command_line_argument_2
      character(LEN=:), allocatable :: Numeral_1, Numeral_2, NumeralSum

      

      ! This program takes the input of two Roman Numerals and produces and outputs the sum as a Numeral.
      ! The valid Numeral values are I, V, X, L, C, D, and M and their lowercase.
      ! Invalid input includes all other ASCII characters.
      
      call GET_COMMAND_ARGUMENT(1, command_line_argument_1)   ! Read argument #1 
      call GET_COMMAND_ARGUMENT(2, command_line_argument_2)   ! and #2 from the command line, and assign it to command_line_argument#

      call BUFFER_TO_STRING(command_line_argument_1, Numeral_1)   ! Adjust to the left and trim trailing spaces from the string 
      call CHECK_STRING_VALID(Numeral_1)   ! Check the input for invalid ASCII characters, return true or STOP the program
      
      call BUFFER_TO_STRING(command_line_argument_2, Numeral_2)   ! Adjust to the left and trim trailing spaces from the string
      call CHECK_STRING_VALID(Numeral_2)   ! Check the input for invalid ASCII characters, return true or STOP the program
      
      Number_1 = NUMERAL_TO_NUMBER(Numeral_1)
      Number_2 = NUMERAL_TO_NUMBER(Numeral_2)

      NumberSum = Number_1 + Number_2

      if (debugMode) write(*,*) Number_1, Number_2, NumberSum

      NumeralSum = NUMBER_TO_NUMERAL(NumberSum)

      write(*,*) NumeralSum
      

      end program numerals

      

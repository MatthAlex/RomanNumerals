      Program RomanNumerals
      implicit none

      integer(kind=1) :: Numeral1_Integer, Numeral2_Integer
      character(LEN=5) :: buffer, comm_arg1, comm_arg2
      character(LEN=:), allocatable :: Numeral1_String, Numeral2_String
      character(LEN=1) :: ValidArray(7)
      logical :: LogicalResult
      
      DATA ValidArray / 'I', 'V', 'X', 'L', 'C', 'D', 'M' /

      ! -----------------------  Initializations
      !Numeral1_Integer = 0; Numeral2_Integer = 0
      LogicalResult = .FALSE.
      ! -------------------------------------------^^^

      CALL GET_COMMAND_ARGUMENT(1, comm_arg1)
      CALL GET_COMMAND_ARGUMENT(2, comm_arg2)

      WRITE(*,*) 'string1 = ', comm_arg1
      WRITE(*,*) 'string2 = ', comm_arg2

      ! -----------------------                                                                                 Handle Input
      WRITE(*,*) 'This program takes the user input of two Roman Numerals and produces and outputs the sum as a Numeral'
      WRITE(*,*) 'The valid Numeral values are I, V, X, L, C, D, and M'
      WRITE(*,*) 'Invalid input include other ASCII characters, including lowercase'
      
      
      DO WHILE (LogicalResult.eq..FALSE.)                  ! If the input contains invalid characters, return an error
            WRITE(*,*) 'Input first Roman Numeral'
            READ(*,*) buffer
            CALL BUFFER_TO_STRING(buffer,Numeral1_String)
            CALL CHECK_VALIDITY(Numeral1_String, LogicalResult)   ! Check the input for invalid ASCII characters, return true/false 
            IF (LogicalResult.eq..FALSE.) WRITE(*,*) Numeral1_String, ' contains invalid ASCII characters. Press CTRL-C to hang the program or input a valid value'
      END DO 

      LogicalResult = .FALSE.       ! Reset the Logical result to restart the process for Numeral#2
      
      DO WHILE (LogicalResult.eq..FALSE.)                  ! If the input contains invalid characters, return an error
            WRITE(*,*) 'Input second Roman Numeral'
            READ(*,*) buffer
            CALL BUFFER_TO_STRING(buffer,Numeral2_String)
            CALL CHECK_VALIDITY(Numeral2_String, LogicalResult)   ! Check the input for invalid ASCII characters, return true/false 
            IF (LogicalResult.eq..FALSE.) WRITE(*,*) Numeral2_String, ' contains invalid ASCII characters. Press CTRL-C to hang the program or input a valid value'
      END DO
      ! ----------------------------------------^^^



      ! -------------------------  Check input for incorrect Roman Numeral decimal patterns


      WRITE(*,*) 'End of program - DEBUG line'
      
      CONTAINS
      !==================================================================================================================
      SUBROUTINE CHECK_VALIDITY(NumString, LogicalVal)
      implicit none
      character(LEN=*), intent(in) :: NumString
      logical, intent(out) :: LogicalVal
      integer :: i

      !WRITE(*,*) 'DEBUG:     ', NumString, LEN(NumString)   ! debug line

      DO i = 1, LEN(NumString)      
           IF (NumString(i:i).ne.'I') THEN
                 LogicalVal = .FALSE. 
           ELSE 
                 LogicalVal = .TRUE.
           END IF
      END DO
      end SUBROUTINE CHECK_VALIDITY
      !==================================================================================================================

      !==================================================================================================================
      SUBROUTINE BUFFER_TO_STRING(buffer, StringVal)
      implicit none
      character(LEN=*), intent(in) :: buffer
      character(LEN=:), allocatable, intent(out) :: StringVal

      !WRITE(*,*) buffer, len(buffer), 'trim(buffer)=', trim(buffer), '  ||   trim(adjustL(buffer))=', trim(adjustL(buffer))  !, StringVal, len(StringVal)

      StringVal = trim(adjustL(buffer))

      !WRITE(*,*) 'buffer =', buffer, len(buffer)
      !WRITE(*,*) 'StringVal =', StringVal, len(StringVal)

      end SUBROUTINE BUFFER_TO_STRING
      !==================================================================================================================
      
     
      end program RomanNumerals
      Program RomanNumerals
      implicit none

      integer(kind=2), dimension(1) :: Numeral1_Integer, Numeral2_Integer
      character(:), allocatable :: string
      character(LEN=1) :: Numeral1_String, Numeral2_String
      logical :: LogicalResult

      ! -----------------------  Initializations
      !Numeral1_Integer = 0; Numeral2_Integer = 0
      LogicalResult = .FALSE.
      ! -------------------------------------------^^^



      ! ----------------------- Handle Input
      WRITE(*,*) 'This program takes the user input of two Roman Numerals and produces and outputs the sum as a Numeral'
      WRITE(*,*) 'The valid Numeral values are I, V, X, L, C, D, and M'
      WRITE(*,*) 'Invalid input include other ASCII characters, including lowercase'
      
      
      DO WHILE (LogicalResult.eq..FALSE.)                  ! If the input contains invalid characters, return an error
            WRITE(*,*) 'Input first Roman Numeral'
            READ(*,*) Numeral1_String
            CALL CHECK_VALIDITY(Numeral1_String, LogicalResult)   ! Check the input for invalid ASCII characters, return true/false 
            IF (LogicalResult.eq..FALSE.) WRITE(*,*) Numeral1_String, ' contains invalid ASCII characters. Press CTRL-C to hang the program or input a valid value'
      END DO 

      LogicalResult = .FALSE.       ! Reset the Logical result to restart the process for Numeral#2
      
      DO WHILE (LogicalResult.eq..FALSE.)                  ! If the input contains invalid characters, return an error
            WRITE(*,*) 'Input second Roman Numeral'
            READ(*,*) Numeral2_String
            CALL CHECK_VALIDITY(Numeral1_String, LogicalResult)   ! Check the input for invalid ASCII characters, return true/false 
            IF (LogicalResult.eq..FALSE.) WRITE(*,*) Numeral2_String, ' contains invalid ASCII characters. Press CTRL-C to hang the program or input a valid value'
      END DO
      ! ----------------------------------------^^^
      
      CONTAINS
      !==================================================================================================================
      SUBROUTINE CHECK_VALIDITY(NumString, LogicalVal)
      implicit none
      character(LEN=*), intent(in) :: NumString
      logical, intent(out) :: LogicalVal
      integer :: i

      WRITE(*,*) 'DEBUG:     ', NumString, LEN(NumString)   ! debug line

      DO i = 1, LEN(NumString)      
           IF (NumString(i:i).ne.'I') THEN
                 LogicalVal = .FALSE. 
           ELSE 
                 LogicalVal = .TRUE.
           END IF
      END DO
      
      !Logical = .TRUE.

      end SUBROUTINE CHECK_VALIDITY
      !==================================================================================================================
      
     
      end program RomanNumerals
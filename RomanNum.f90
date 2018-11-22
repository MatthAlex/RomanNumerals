      Program numerals
      USE parameters, only: debugMode
      implicit none
      
      integer(kind=4)               :: Number_1, Number_2, NumberSum
      character(LEN=20)             :: command_line_argument_1, command_line_argument_2
      character(LEN=:), allocatable :: Numeral_1, Numeral_2, NumeralSum

      ! -----------------------  Initializations
      Number_1 = 0; Number_2 = 0
      ! -------------------------------------------^^^

      ! -----------------------
      ! This program takes the input of two Roman Numerals and produces and outputs the sum as a Numeral.
      ! The valid Numeral values are I, V, X, L, C, D, and M and their lowercase.
      ! Invalid input includes all other ASCII characters.
      
      CALL GET_COMMAND_ARGUMENT(1, command_line_argument_1)   ! Read argument #1 
      CALL GET_COMMAND_ARGUMENT(2, command_line_argument_2)   ! and #2 from the command line, and assign it to command_line_argument#

      CALL BUFFER_TO_STRING(command_line_argument_1, Numeral_1)   ! Adjust to the left and trim trailing spaces from the string 
      CALL CHECK_STRING_VALID(Numeral_1)   ! Check the input for invalid ASCII characters, return true or STOP the program
      
      CALL BUFFER_TO_STRING(command_line_argument_2, Numeral_2)   ! Adjust to the left and trim trailing spaces from the string
      CALL CHECK_STRING_VALID(Numeral_2)   ! Check the input for invalid ASCII characters, return true or STOP the program
      
      Number_1 = NUMERAL_TO_NUMBER(Numeral_1)
      Number_2 = NUMERAL_TO_NUMBER(Numeral_2)

      NumberSum = Number_1 + Number_2

      IF (debugMode.eqv..TRUE.) WRITE(*,*) Number_1, Number_2, NumberSum

      NumeralSum = NUMBER_TO_NUMERAL(NumberSum)

      WRITE(*,*) NumeralSum
      



      CONTAINS
      
      
      !==================================================================================================================
      ! This routine tests whether the input string contains any invalid ASCII characters. If a null or invalid input is
      ! detected, the routine STOPs the program.

      SUBROUTINE CHECK_STRING_VALID(StringValue)
      USE PARAMETERS, only: ValidCharacters
      implicit none
      character(LEN=*), intent(in) :: StringValue
      logical                      :: isCharacterValid
      integer                      :: i, j

      DO i = 1, LEN(StringValue)                             ! for each character in the input string
         DO j = 1, SIZE(ValidCharacters)                     ! for each element in ValidCharacters
           IF (StringValue(i:i).eq.ValidCharacters(j)) THEN  ! when a match is detected, skip this valid character and start the next loop.
              isCharacterValid = .TRUE. 
              EXIT
           ELSE 
              isCharacterValid = .FALSE.
           END IF
         END DO
      
         IF (isCharacterValid.eqv..FALSE.) THEN
           WRITE(*,*) 'ERROR: ',StringValue, ' contains invalid ASCII characters.'
           STOP
         END IF 
      
      END DO
      
      end SUBROUTINE CHECK_STRING_VALID
      !==================================================================================================================

      !==================================================================================================================
      ! This routine adjusts the string to the left, removes any trailing spaces, and allocates the new length.
      ! The resulting string cannot be empty.

      SUBROUTINE BUFFER_TO_STRING(buffer, StringValue)
      implicit none
      character(LEN=*)            , intent(in)  :: buffer
      character(LEN=:), allocatable, intent(out) :: StringValue

      StringValue = trim(adjustL(buffer))
      
      IF (StringValue.eq.'') THEN
        WRITE(*,*) 'ERROR: command line argument empty.'
        STOP
      ENDIF
      
      end SUBROUTINE BUFFER_TO_STRING
      !==================================================================================================================
      
      !==================================================================================================================
      !SOURCE: https://rosettacode.org/wiki/Roman_numerals/Encode#Fortran
      ! This function takes the input of a number and translates it in a valid Roman Numeral

      FUNCTION NUMBER_TO_NUMERAL(numberArabic) RESULT (numeral)
      USE PARAMETERS, only: RomanNumberValues, numeralCharacters
      implicit none
      integer(kind=4), intent(in) :: numberArabic
      integer                     :: i, numberDummy, integerDivision
      character(32)               :: numeral
      
      numeral = ''                                      ! initialise the string to null
      numberDummy = numberArabic                        ! use numberDummy to store the changing number instead of numberArabic(intent(in))
      
      DO i = 1, SIZE(RomanNumberValues)                 ! for every distinct roman numeral number, descending order (high to low)
        ! divide number over roman number value(i). As the values are integers, the remainder is ignored for this loop.
        integerDivision = numberDummy / RomanNumberValues(i)                   
        
        ! collate the numeral character, integerDivision times 
        numeral = trim(numeral) // repeat(trim(numeralCharacters(i)), integerDivision)     
        
        ! To account for the remainder from the integer division, subtract the value of the numeral character 
        ! integerDivision times, and update 
        numberDummy = numberDummy - RomanNumberValues(i) * integerDivision    
      END DO
 
      END FUNCTION NUMBER_TO_NUMERAL
      !==================================================================================================================
      
      
      !==================================================================================================================
      !SOURCE: https://www.rosettacode.org/wiki/Roman_numerals/DECODE#Fortran
      !The original version cannot understand invalid numeral progression. F.e. it can be fooled by IVIVIVI, IXI-IVI, CMCMDIXVIXVID, etc
      !
      FUNCTION NUMERAL_TO_NUMBER(numeral) RESULT(numberArabic)
      implicit none
      character(LEN=*), intent(in)   :: numeral
      integer                        :: i, newValue, previousValue, numberArabic
      integer(kind=4), dimension(13) :: counters
      logical                        :: isDoubleValueNumeral
 
      !----- Initialisation --------------------------------
      numberArabic = 0
      previousValue = 0
      isDoubleValueNumeral = .FALSE.
      newValue = 0
      counters = 0
      !-----------------------------------------------------

      DO i = LEN(numeral), 1, -1                                          ! Start from the end and iterate the opposite way
        IF (isDoubleValueNumeral.eqv..TRUE.) THEN                          ! IF the previous iteration numeral is part of double value numeral
          isDoubleValueNumeral = .FALSE.                                  ! RESET the trigger AND
          CYCLE                                                           ! skip the current loop
        END IF
        
        SELECT CASE(numeral(i:i)) 
        CASE ('M','m')                                                    ! IF the numeral is M
          newValue = 1000                                                 ! update newValue 
          CALL COUNTING(13,counters)                                      ! increase M counter by 1
          IF (i.ne.1) THEN                                                ! IF the numeral is not the last one
            IF (numeral(i-1:i-1).eq.'C'.or.numeral(i-1:i-1).eq.'c') THEN  ! IF the numeral before CAN form a double value numeral, e.g. CM
              newValue = 900                                              ! update newValue by overwriting the previous
              isDoubleValueNumeral = .TRUE.                               ! the two numerals of i and i-1 form a double value numeral
              CALL COUNTING(12,counters)                                  ! increase CM counter by 1
              IF (counters(9).ge.counters(12)) THEN                       ! IF the i-1 component of the numeral coexists with the individual numeral character
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. CM and C found together.'
                STOP                                                      ! STOP the program 
              END IF
              counters(13) = counters(13) - 1                             ! IF the numeral was a double value numeral, decrease M counter by 1
            END IF
          END IF
          
        CASE ('D','d')
          newValue = 500
          CALL COUNTING(11,counters)                                      ! increase counter of D
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'C'.or.numeral(i-1:i-1).eq.'c') THEN  ! distinguish CD as 400
              newValue = 400 
              isDoubleValueNumeral = .TRUE.
              CALL COUNTING(10,counters)                                  ! increase counter of CD
              IF (counters(9).ge.counters(10)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. CD and C found together.'
                STOP
              END IF
              counters(11) = counters(11) - 1
            END IF
          END IF
          
        CASE ('C','c')
          newValue = 100
          CALL COUNTING(9,counters)                                       ! increase counter of C
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'X'.or.numeral(i-1:i-1).eq.'x') THEN  ! distinguish XC as 90
              newValue = 90 
              isDoubleValueNumeral = .TRUE.
              CALL COUNTING(8,counters)                                   ! increase counter of XC
              IF (counters(5).ge.counters(8)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. XC and X found together.'
                STOP
              END IF
              counters(9) = counters(9) - 1
            END IF
          END IF
          
        CASE ('L','l')
          newValue = 50
          CALL COUNTING(7,counters)                                       ! increase counter of L
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'X'.or.numeral(i-1:i-1).eq.'x') THEN  ! distinguish XL as 40
              newValue = 40 
              isDoubleValueNumeral = .TRUE.
              CALL COUNTING(6,counters)                                   ! increase counter of XL
              IF (counters(5).ge.counters(6)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. XL and X found together.'
                STOP
              END IF
              counters(7) = counters(7) - 1
            END IF
          END IF
          
        CASE ('X','x')
          newValue = 10
          CALL COUNTING(5,counters)                                       ! increase counter of X
          
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'I'.or.numeral(i-1:i-1).eq.'i') THEN  ! distinguish IX as 9
              newValue = 9 
              isDoubleValueNumeral = .TRUE.
              CALL COUNTING(4,counters)                                   ! increase counter of IX
              IF (counters(1).ge.counters(4)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. I and IX found together.'
                STOP
              END IF
              counters(5) = counters(5) - 1
            END IF
          END IF
          
        CASE ('V','v')
          newValue = 5
          CALL COUNTING(3,counters)                                       ! increase counter of V(lad)

          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'I'.or.numeral(i-1:i-1).eq.'i') THEN  ! distinguish IV as 4
              newValue = 4 
              isDoubleValueNumeral = .TRUE.
              CALL COUNTING(2, counters)                                  ! increase counter of IV
              IF (counters(1).ge.counters(2)) THEN
                WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. I and IV found together.'
                STOP
              END IF
              counters(3) = counters(3) - 1
            END IF
          END IF
          
        CASE ('I','i')
          newValue = 1
          CALL COUNTING(1,counters)                                       ! increase counter of I
          
        CASE default                                                      ! this will not detect invalid characters
          WRITE(*,*) 'ERROR: SELECT CASE selector wrong value...'
          newValue = 0
        END SELECT
        
        
        ! Update the number by newValue, if it's equal or larger than the previousValue.
        IF (newValue >= previousValue) THEN                               
          numberArabic = numberArabic + newValue
          
          ! This traps successive double value numerals of the same order. (eg IXIV, XCXL, etc)
          ! IF the new value is the second double Value Numeral in succession, STOP the program. 
          IF (real(previousValue).eq.real(newValue)*4/9.and.isDoubleValueNumeral.eqv..TRUE.) THEN             
            WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. Two double value numerals in succession'    
            STOP
          END IF

          ! This traps double value numerals followed by smaller numerals, which their sum would be assigned a larger value numeral 
          ! (eg IXV = 9+5 = 14 = XIV)
          IF (real(newValue).lt.real(previousValue*2).and.isDoubleValueNumeral.eqv..TRUE.) THEN  
            WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. Double value numeral along with smaller numeral of the same order'
            STOP  
          END IF
        
        ELSE   ! IF newValue < previousValue
          WRITE(*,*) 'ERROR: INVALID NUMERAL SUCCESSION' 
          STOP
        END IF
        
        ! update the previous value in preparation of next iteration
        previousValue = newValue                                          
      END DO
      END FUNCTION NUMERAL_TO_NUMBER
      !==================================================================================================================
      
      !==================================================================================================================
      ! This routine iterates the counters for each roman numeral, and compares the amount to the valid count. If the 
      ! count is larger than the valid count, the program STOPs.
      
      SUBROUTINE COUNTING(order,counters)
      USE PARAMETERS, only: validCount            ! total valid amount of counts for each numeral
      implicit none
      integer(kind=4), intent(in) :: order       ! the order is given by the numeral succession: I, IV, V, IX, X, .. etc
      integer(kind=4), dimension(13), intent(inout) :: counters    ! how many counts of each numeral are triggered

      counters(order) = counters(order) + 1     ! iterate for every call
      IF (counters(order).gt.validCount(order)) THEN  ! if more counts than permitted are found, abort the program with an error.
        WRITE(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
        STOP
      END IF
      
      end SUBROUTINE COUNTING
      !==================================================================================================================

      
      end program numerals

      

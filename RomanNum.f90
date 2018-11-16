      Program RomanNumerals
      USE PARAMETERS    !, only: SingleRomanValues, DoubleRomanValues  ! DEBUG
      implicit none
      
      integer(kind=2) :: Numeral1_Integer, Numeral2_Integer, Sum_Integer
      character(LEN=20) :: comm_arg1, comm_arg2
      character(LEN=:), allocatable :: Numeral1_String, Numeral2_String, NumeralFinal_String

      ! -----------------------  Initializations
      Numeral1_Integer = 0; Numeral2_Integer = 0
      ! -------------------------------------------^^^

      CALL GET_COMMAND_ARGUMENT(1, comm_arg1)   ! Read argument #1 and #2 from the command line, and assign it to comm_arg#
      CALL GET_COMMAND_ARGUMENT(2, comm_arg2)

      ! -----------------------  Handle Input
      !WRITE(*,*) 'This program takes the user input of two Roman Numerals and produces and outputs the sum as a Numeral'
      !WRITE(*,*) 'The valid Numeral values are I, V, X, L, C, D, and M and their lowercase'
      !WRITE(*,*) 'Invalid input include other ASCII characters.'
      
      CALL BUFFER_TO_STRING(comm_arg1, Numeral1_String)   ! Adjust to the left and trim trailing spaces from the string 
      CALL CHECK_VALIDITY(Numeral1_String)   ! Check the input for invalid ASCII characters, return true/false 
      
      CALL BUFFER_TO_STRING(comm_arg2, Numeral2_String)   ! Adjust to the left and trim trailing spaces from the string
      CALL CHECK_VALIDITY(Numeral2_String)   ! Check the input for invalid ASCII characters, return true/false 
      
    
      !WRITE(*,'(A)') ROMAN(1998)

      Numeral1_Integer = DECODER(Numeral1_String)
      Numeral2_Integer = DECODER(Numeral2_String)

      Sum_Integer = Numeral1_Integer + Numeral2_Integer

      WRITE(*,*) Numeral1_Integer, Numeral2_Integer, Sum_Integer

      
      !NumeralFinal_String = ROMAN(Sum_Integer)
      !WRITE(*,'(A)') NumeralFinal_String

      





      !DO WHILE (LogicalResult.eq..FALSE.)                  ! If the input contains invalid characters, return an error
      !      WRITE(*,*) 'Input first Roman Numeral'
      !      READ(*,*) buffer
      !      CALL BUFFER_TO_STRING(buffer,Numeral1_String)
      !      CALL CHECK_VALIDITY(Numeral1_String, LogicalResult)   ! Check the input for invalid ASCII characters, return true/false 
      !      IF (LogicalResult.eq..FALSE.) WRITE(*,*) Numeral1_String, ' contains invalid ASCII characters. Press CTRL-C to hang the program or input a valid value'
      !END DO 

      !LogicalResult = .FALSE.       ! Reset the Logical result to restart the process for Numeral#2
      
      !DO WHILE (LogicalResult.eq..FALSE.)                  ! If the input contains invalid characters, return an error
      !      WRITE(*,*) 'Input second Roman Numeral'
      !      READ(*,*) buffer
      !      CALL BUFFER_TO_STRING(buffer,Numeral2_String)
      !      CALL CHECK_VALIDITY(Numeral2_String, LogicalResult)   ! Check the input for invalid ASCII characters, return true/false 
      !      IF (LogicalResult.eq..FALSE.) WRITE(*,*) Numeral2_String, ' contains invalid ASCII characters. Press CTRL-C to hang the program or input a valid value'
      !END DO
      ! ----------------------------------------^^^



      ! -------------------------  Check input for incorrect Roman Numeral decimal patterns


      !WRITE(*,*) 'End of program - DEBUG line'
      
      CONTAINS
      !==================================================================================================================
      SUBROUTINE CHECK_VALIDITY(NumString)
      USE PARAMETERS, only: ValidArray
      implicit none
      character(LEN=*), intent(in) :: NumString
      logical :: LogicalVal
      integer :: i, j

      DO i = 1, LEN(NumString)     
         DO j = 1, 7
           IF (NumString(i:i).eq.ValidArray(j)) THEN
              LogicalVal = .TRUE. 
              EXIT
           ELSE 
              LogicalVal = .FALSE.
           END IF
         END DO
      END DO
      
      IF (LogicalVal.eq..FALSE.) WRITE(*,*) 'ERROR: ',NumString, ' contains invalid ASCII characters. Press CTRL-C to hang the program'

      end SUBROUTINE CHECK_VALIDITY
      !==================================================================================================================

      !==================================================================================================================
      SUBROUTINE BUFFER_TO_STRING(buffer, StringVal)
      implicit none
      character(LEN=20)            , intent(in)  :: buffer
      character(LEN=:), allocatable, intent(out) :: StringVal

      StringVal = trim(adjustL(buffer))
      
      !WRITE(*,*) 'buffer =', buffer, len(buffer)
      !WRITE(*,*) 'StringVal =', StringVal, len(StringVal)

      end SUBROUTINE BUFFER_TO_STRING
      !==================================================================================================================
      
      !==================================================================================================================
      !SOURCE: https://rosettacode.org/wiki/Roman_numerals/Encode#Fortran
      FUNCTION ROMAN(numeral) RESULT (r)  
      implicit none
      integer, intent(in) :: numeral
      integer, parameter  :: d_max = 13
      integer             :: d
      integer             :: m
      integer             :: m_div
      character(32)       :: r
      integer,       dimension(d_max), parameter :: d_dec = (/1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1/)
      character(32), dimension(d_max), parameter :: d_rom = (/'M ','CM','D ','CD','C ','XC','L ','XL','X ','IX','V ','IV','I '/)
 
      r = ''
      m = numeral
      
      DO d = 1, d_max
        m_div = m / d_dec(d)
        r = trim(r) // repeat(trim(d_rom(d)), m_div)
        m = m - d_dec(d) * m_div
      END DO
 
      END FUNCTION ROMAN
      !==================================================================================================================
      
      
      !==================================================================================================================
      !SOURCE: https://www.rosettacode.org/wiki/Roman_numerals/DECODE#Fortran
      !The original version cannot understand invalid numeral progression. F.e. it can be fooled by IVIVIVI
      !
      FUNCTION DECODER(numeral) RESULT(numberArabic)
      implicit none
      character(LEN=*), intent(in) :: numeral
      integer :: i, n, lastval, numberArabic
      integer(kind=2) :: counters(13)
      logical :: doubleVal
 
      numberArabic = 0
      lastval = 0
      doubleVal = .FALSE.
      n = 0
      counters = 0

      DO i = LEN(numeral), 1, -1    ! Start from the end and iterate the opposite way
        IF (doubleVal.eq..TRUE.) THEN 
          doubleVal = .FALSE. 
          CYCLE    !when a double valued numeral is detected, skip the current loop
        END IF
        
        WRITE(*,*) i, numeral(i:i)
        
        SELECT CASE(numeral(i:i)) ! case structure can be used to create a jump list from a selector, where the selector can be Int or Char
        CASE ('M','m')
          n = 1000
          CALL COUNTING(13,counters(13))
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'C'.or.numeral(i-1:i-1).eq.'c') THEN  ! distinguish CM as 900
              n = 900 
              doubleVal = .TRUE.
              CALL COUNTING(12,counters(12))
            END IF
          END IF
        CASE ('D','d')
          n = 500
          CALL COUNTING(11,counters(11))
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'C'.or.numeral(i-1:i-1).eq.'c') THEN  ! distinguish CD as 400
              n = 400 
              doubleVal = .TRUE.
              CALL COUNTING(10,counters(10))
            END IF
          END IF
        
        CASE ('C','c')
          n = 100
          CALL COUNTING(9,counters(9))
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'X'.or.numeral(i-1:i-1).eq.'x') THEN  ! distinguish XC as 90
              n = 90 
              doubleVal = .TRUE.
              CALL COUNTING(8,counters(8))
            END IF
          END IF
        
        CASE ('L','l')
          n = 50
          CALL COUNTING(7,counters(7))
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'X'.or.numeral(i-1:i-1).eq.'x') THEN  ! distinguish XL as 40
              n = 40 
              doubleVal = .TRUE.
              CALL COUNTING(6,counters(6))
            END IF
          END IF
        
        CASE ('X','x')
          n = 10
          CALL COUNTING(5,counters(5))
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'I'.or.numeral(i-1:i-1).eq.'i') THEN  ! distinguish IX as 9
              n = 9 
              doubleVal = .TRUE.
              CALL COUNTING(4,counters(4))

            END IF
          END IF
        
        CASE ('V','v')
          n = 5
          CALL COUNTING(3,counters(3))
          
          IF (i.ne.1) THEN
            IF (numeral(i-1:i-1).eq.'I'.or.numeral(i-1:i-1).eq.'i') THEN  ! distinguish IV as 4
              n = 4 
              doubleVal = .TRUE.
              CALL COUNTING(2,counters(2))
            END IF
          END IF

        CASE ('I','i')
          n = 1
          CALL COUNTING(1,counters(1))
          
        CASE default    ! this will not detect invalid characters
          n = 0
        END SELECT
          
        WRITE(*,*) i, n, lastval
        
        
        
        IF (n >= lastval) THEN
          numberArabic = numberArabic + n
          
          IF (real(lastval).eq.real(n)*4/9) THEN      !lastval>1.and.
            WRITE(*,*) 'INVALID NUMERAL SUCCESSION. 2 DOUBLEVAL NUMERALS in succession'    ! if the new value is the second doubleVal in succession, abort 
            STOP
          END IF
        
        ELSE !IF (n  lastval.and.lastval.gt.0) THEN
          WRITE(*,*) 'INVALID NUMERAL SUCCESSION' 
        !ELSE !IF (n > lastval.and.doubleVal.eq..FALSE.) THEN
        !  numberArabic = numberArabic + n
        END IF
        lastval = n
      END DO

      END FUNCTION DECODER
      !==================================================================================================================
      
      !==================================================================================================================
      SUBROUTINE COUNTING(order,counters)
      USE PARAMETERS, only: validCount
      implicit none
      integer(kind=2), intent(in) :: order 
      integer(kind=2), dimension(1), intent(inout) :: counters

      counters(order) = counters(order) + 1
      IF (counters(order).gt.validCount(order)) THEN
        WRITE(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
        STOP
      END IF
      
      end SUBROUTINE COUNTING
      !==================================================================================================================


      end program RomanNumerals

      

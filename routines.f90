      module routines
      !use parameters
      implicit none
      contains
	!==================================================================================================================
      ! This routine tests whether the input string contains any invalid ASCII characters. If a null or invalid input is
      ! detected, the routine stops the program.
	
      subroutine check_string_valid(StringValue)
      use parameters, only: ValidCharacters
      implicit none
      character(LEN=*), intent(in) :: StringValue
      logical                      :: isCharacterValid
      integer                      :: i, j

      do i = 1, LEN(StringValue)                             ! for each character in the input string
         do j = 1, SIZE(ValidCharacters)                     ! for each element in ValidCharacters
           if (StringValue(i:i).eq.ValidCharacters(j)) then  ! when a match is detected, skip this valid character and start the next loop.
              isCharacterValid = .TRUE. 
              exit
           else 
              isCharacterValid = .FALSE.
           end if
         end do
      
         if (isCharacterValid.eqv..FALSE.) then
           write(*,*) 'ERROR: ',StringValue, ' contains invalid ASCII characters.'
           stop
         end if 
      end do
      
      end subroutine check_string_valid
      !==================================================================================================================

      !==================================================================================================================
      ! This routine adjusts the string to the left, removes any trailing spaces, and allocates the new length.
      ! The resulting string cannot be empty.

      subroutine buffer_to_string(buffer, StringValue)
	  use parameters, only: debugMode
      implicit none
      character(LEN=*)   		         , intent(in)  :: buffer
      character(LEN=:), allocatable, intent(out) :: StringValue

      StringValue = trim(adjustL(buffer))
      
      if (StringValue.eq.'') then
        write(*,*) 'ERROR: command line argument empty.'
        stop
      end if
      
      end subroutine buffer_to_string
      !==================================================================================================================
      
      !==================================================================================================================
      !SOURCE: https://rosettacode.org/wiki/Roman_numerals/Encode#Fortran
      ! This function takes the input of a number and translates it in a valid Roman Numeral

      subroutine number_to_numeral(numberArabic, numeral)
      use parameters, only: RomanNumberValues, numeralCharacters
      implicit none
      integer(kind=4), intent(in) 			:: numberArabic
      integer                     					:: i, numberDummy, integerDivision
      character(LEN=:) , allocatable, intent(out)		:: numeral
      
      numeral = ''                                      ! initialise the string to null
      numberDummy = numberArabic                        ! use numberDummy to store the changing number instead of numberArabic(intent(in))
      
      do i = 1, SIZE(RomanNumberValues)                 ! for every distinct roman numeral number, descending order (high to low)
        ! divide number over roman number value(i). As the values are integers, the remainder is ignored for this loop.
        integerDivision = numberDummy / RomanNumberValues(i)                   
        
        ! collate the numeral character, integerDivision times 
        numeral = trim(numeral) // repeat(trim(numeralCharacters(i)), integerDivision)     
        
        ! To account for the remainder from the integer division, subtract the value of the numeral character 
        ! integerDivision times, and update 
        numberDummy = numberDummy - RomanNumberValues(i) * integerDivision    
      end do
 
      end subroutine number_to_numeral
      !==================================================================================================================
      
      
      !==================================================================================================================
      !SOURCE: https://www.rosettacode.org/wiki/Roman_numerals/DECODE#Fortran
      !The original version cannot understand invalid numeral progression. F.e. it can be fooled by IVIVIVI, IXI-IVI, CMCMDIXVIXVID, etc
      !
      function numeral_to_number(numeral) result(numberArabic)
	  use parameters, only: debugMode
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
	  if (debugMode) write(*,*) 'Numeral_to_number initial numeral =', numeral, ', and numeral length=', LEN(numeral)
      !-----------------------------------------------------

      do i = LEN(numeral), 1, -1                                          ! Start from the end and iterate the opposite way
        IF (isDoubleValueNumeral.eqv..TRUE.) then                          ! IF the previous iteration numeral is part of double value numeral
          isDoubleValueNumeral = .FALSE.                                  ! RESET the trigger AND
          cycle                                                           ! skip the current loop
        end if
        
        select case(numeral(i:i)) 
        CASE ('M','m')                                                    ! IF the numeral is M
          newValue = 1000                                                 ! update newValue 
          call counting(13,counters)                                      ! increase M counter by 1
          IF (i.ne.1) then                                                ! IF the numeral is not the last one
            IF (numeral(i-1:i-1).eq.'C'.or.numeral(i-1:i-1).eq.'c') then  ! IF the numeral before CAN form a double value numeral, e.g. CM
              newValue = 900                                              ! update newValue by overwriting the previous
              isDoubleValueNumeral = .TRUE.                               ! the two numerals of i and i-1 form a double value numeral
              call counting(12,counters)                                  ! increase CM counter by 1
              IF (counters(9).ge.counters(12)) then                       ! IF the i-1 component of the numeral coexists with the individual numeral character
                write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. CM and C found together.'
                stop                                                      ! stop the program 
              end if
              counters(13) = counters(13) - 1                             ! IF the numeral was a double value numeral, decrease M counter by 1
            end if
          end if
          
        CASE ('D','d')
          newValue = 500
          call counting(11,counters)                                      ! increase counter of D
          IF (i.ne.1) then
            IF (numeral(i-1:i-1).eq.'C'.or.numeral(i-1:i-1).eq.'c') then  ! distinguish CD as 400
              newValue = 400 
              isDoubleValueNumeral = .TRUE.
              call counting(10,counters)                                  ! increase counter of CD
              IF (counters(9).ge.counters(10)) then
                write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. CD and C found together.'
                stop
              end if
              counters(11) = counters(11) - 1
            end if
          end if
          
        CASE ('C','c')
          newValue = 100
          call counting(9,counters)                                       ! increase counter of C
          IF (i.ne.1) then
            IF (numeral(i-1:i-1).eq.'X'.or.numeral(i-1:i-1).eq.'x') then  ! distinguish XC as 90
              newValue = 90 
              isDoubleValueNumeral = .TRUE.
              call counting(8,counters)                                   ! increase counter of XC
              IF (counters(5).ge.counters(8)) then
                write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. XC and X found together.'
                stop
              end if
              counters(9) = counters(9) - 1
            end if
          end if
          
        CASE ('L','l')
          newValue = 50
          call counting(7,counters)                                       ! increase counter of L
          IF (i.ne.1) then
            IF (numeral(i-1:i-1).eq.'X'.or.numeral(i-1:i-1).eq.'x') then  ! distinguish XL as 40
              newValue = 40 
              isDoubleValueNumeral = .TRUE.
              call counting(6,counters)                                   ! increase counter of XL
              IF (counters(5).ge.counters(6)) then
                write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. XL and X found together.'
                stop
              end if
              counters(7) = counters(7) - 1
            end if
          end if
          
        CASE ('X','x')
          newValue = 10
          call counting(5,counters)                                       ! increase counter of X
          
          IF (i.ne.1) then
            IF (numeral(i-1:i-1).eq.'I'.or.numeral(i-1:i-1).eq.'i') then  ! distinguish IX as 9
              newValue = 9 
              isDoubleValueNumeral = .TRUE.
              call counting(4,counters)                                   ! increase counter of IX
              IF (counters(1).ge.counters(4)) then
                write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. I and IX found together.'
                stop
              end if
              counters(5) = counters(5) - 1
            end if
          end if
          
        CASE ('V','v')
          newValue = 5
          call counting(3,counters)                                       ! increase counter of V(lad)

          IF (i.ne.1) then
            IF (numeral(i-1:i-1).eq.'I'.or.numeral(i-1:i-1).eq.'i') then  ! distinguish IV as 4
              newValue = 4 
              isDoubleValueNumeral = .TRUE.
              call counting(2, counters)                                  ! increase counter of IV
              IF (counters(1).ge.counters(2)) then
                write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. I and IV found together.'
                stop
              end if
              counters(3) = counters(3) - 1
            end if
          end if
          
        CASE ('I','i')
          newValue = 1
          call counting(1,counters)                                       ! increase counter of I
          
        CASE default                                                      ! this will not detect invalid characters
			write(*,*) 'ERROR: SELECT CASE selector wrong value...'
			if (debugMode) write(*,*) 'CASE default selector value =', numeral(i:i), 'for i =', i
          newValue = 0
        end select
        
        
        ! Update the number by newValue, if it's equal or larger than the previousValue.
        IF (newValue >= previousValue) then                               
          numberArabic = numberArabic + newValue
          
          ! This traps successive double value numerals of the same order. (eg IXIV, XCXL, etc)
          ! IF the new value is the second double Value Numeral in succession, stop the program. 
          IF (real(previousValue).eq.real(newValue)*4/9.and.isDoubleValueNumeral.eqv..TRUE.) then             
            write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. Two double value numerals in succession'    
            stop
          end if

          ! This traps double value numerals followed by smaller numerals, which their sum would be assigned a larger value numeral 
          ! (eg IXV = 9+5 = 14 = XIV)
          IF (real(newValue).lt.real(previousValue*2).and.isDoubleValueNumeral.eqv..TRUE.) then  
            write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. Double value numeral along with smaller numeral of the same order'
            stop  
          end if
        
        else   ! IF newValue < previousValue
          write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION' 
          stop
        end if
        
        ! update the previous value in preparation of next iteration
        previousValue = newValue                                          
      end do
      end function numeral_to_number
      !==================================================================================================================
      
      !==================================================================================================================
      ! This routine iterates the counters for each roman numeral, and compares the amount to the valid count. If the 
      ! count is larger than the valid count, the program stops.
      
      subroutine counting(order,counters)
      use parameters, only: validCount            ! total valid amount of counts for each numeral
      implicit none
      integer(kind=4), intent(in) :: order       ! the order is given by the numeral succession: I, IV, V, IX, X, .. etc
      integer(kind=4), dimension(13), intent(inout) :: counters    ! how many counts of each numeral are triggered

      counters(order) = counters(order) + 1     ! iterate for every call
      IF (counters(order).gt.validCount(order)) then  ! if more counts than permitted are found, abort the program with an error.
        write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
        stop
      end if
      
      end subroutine counting
      !==================================================================================================================

      
     
      end module routines

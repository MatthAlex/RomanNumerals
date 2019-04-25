   !> Contains all routines used in the main program.
   module routines
   implicit none
   contains


   !==================================================================================================================
   !> This routine tests whether the input string contains any invalid ASCII characters. If a null or invalid input is
   !> detected, the routine stops the program.

   subroutine check_string_valid(StringValue, error)
   use parameters, only: ValidCharacters
   implicit none
   character(LEN=*), intent(in) :: StringValue
   logical :: isCharacterValid   !! Logical flag to check whether a character is valid
   integer, intent(out) :: error
   integer :: i, j

   error = 0
   ! scan each character in the input string and compare to the ValidCharacters elements
   ! when a match is detected, exit and start the next loop.
   do i = 1, LEN(StringValue)
      do j = 1, SIZE(ValidCharacters)
         if (StringValue(i:i).eq.ValidCharacters(j)) then
            isCharacterValid = .TRUE.
            exit
         else
            isCharacterValid = .FALSE.
         end if
      end do

      if (.NOT.isCharacterValid) then
         write(*,*) 'ERROR: ',StringValue, ' contains invalid ASCII characters.'
         error = 1
         return
      end if
   end do

   end subroutine check_string_valid
   !==================================================================================================================



   !==================================================================================================================
   !> This routine tests whether the input string contains any invalid ASCII characters. If a null or invalid input is
   !> detected, the routine stops the program.

   subroutine verify_string(StringValue, error)
   implicit none
   character(LEN=*), intent(in) :: StringValue
   logical :: isCharacterValid = .TRUE.
   integer, intent(out) :: error
   integer :: Sresult

   Sresult = verify(StringValue, "IVXLCDM",isCharacterValid)
   error = 0

   if (Sresult.ne.0) then
      write(*,*) 'ERROR: ',StringValue, ' contains invalid ASCII characters.'
      error = 1
      return
   end if

   end subroutine verify_string
   !==================================================================================================================

   !==================================================================================================================
   !> This routine turns lowerc: ase to uppercase, adjusts the string to the left, removes any trailing spaces, and allocates the new length.
   !> The resulting string cannot be empty.

   subroutine buffer_to_string(buffer, StringValue, error)
   implicit none
   integer, intent(out) :: error
   character(LEN=*), intent(in)   :: buffer
   character(LEN=:), allocatable, intent(out) :: StringValue

   error = 0
   StringValue = to_upper( trim(adjustL(buffer)) )

   if (StringValue.eq.'') then
      write(*,*) 'ERROR: command line argument empty.'
      error = 1
      return
   end if

   end subroutine buffer_to_string
   !==================================================================================================================

   !==================================================================================================================
   !> source: rosettacode.org/wiki/Roman_numerals/Encode#Fortran.
   !> This routine takes the input of a number and translates it in a valid Roman Numeral.

   subroutine number_to_numeral(numberArabic, numeral)
   use parameters, only: RomanNumberValues, numeralCharacters
   implicit none
   integer, value :: numberArabic   !! dummy value initialized to numberArabic
   integer   :: i, integerDivision
   character(LEN=:) , allocatable, intent(out) :: numeral

   numeral = ''

   ! for every distinct roman numeral number, descending order (high to low)
   do i = SIZE(RomanNumberValues), 1, -1

   ! divide number over roman number value(i). As the values are integers, the remainder is ignored for this loop.
      integerDivision = numberArabic / RomanNumberValues(i)

   ! collate the numeral character, integerDivision times
      numeral = trim(numeral) // repeat(trim(numeralCharacters(i)), integerDivision)

   ! To account for the remainder from the integer division, subtract the value of the numeral character
   ! integerDivision times, and update
      numberArabic = numberArabic - RomanNumberValues(i) * integerDivision
   end do

   end subroutine number_to_numeral
   !==================================================================================================================


   !==================================================================================================================
   !> source: rosettacode.org/wiki/Roman_numerals/DECODE#Fortran. This function translates a Roman numeral character into its equivalent numerical value.
   !> The original version cannot understand invalid numeral progression. F.e. it can be fooled by IVIVIVI, IXI-IVI, CMCMDIXVIXVID, etc

   function numeral_to_number(numeral) result(numberArabic)
   use parameters, only: debugMode, RomanNumberValues, NumeralCharacters, validCount
   implicit none
   character(LEN=*), intent(in) :: numeral
   integer :: i, newValue, previousValue, numberArabic
   integer, dimension(13) :: counters
   logical :: isDoubleValueNumeral

   !----- Initialisation --------------------------------
   numberArabic = 0
   previousValue = 0
   isDoubleValueNumeral = .FALSE.
   newValue = 0
   counters = 0
   !-----------------------------------------------------

   if (debugMode) write(*,*) 'Numeral_to_number initial numeral =', numeral, ', and numeral length=', LEN(numeral)

   ! The loop starts from the end of the numeral and iterates on the opposite way.
   do i = LEN(numeral), 1, -1

   ! When a double value numeral has already been detected, skip the current loop and reset the flag
      if (isDoubleValueNumeral) then
         isDoubleValueNumeral = .FALSE.
         cycle
      end if

      select case(numeral(i:i))
      CASE (NumeralCharacters(13))
         newValue = RomanNumberValues(13)                     ! M = 1000

         counters(13) = counters(13) + 1
         if (counters(13).gt.validCount(13)) then
            write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
            numberArabic = -1
            return
         end if

         if (i.ne.1) then
            if (numeral(i-1:i-1).eq.NumeralCharacters(9)) then   ! if C
               newValue = RomanNumberValues(12)               ! CM = 900
               isDoubleValueNumeral = .TRUE.

               counters(12) = counters(12) + 1
               if (counters(12).gt.validCount(12)) then
                  write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
                  numberArabic = -1
                  return
               end if

               if (counters(9).ge.counters(12)) then
                  write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. CM and C found together.'
                  numberArabic = -1
                  return
               end if
            counters(13) = counters(13) - 1
            end if
         end if

      CASE (NumeralCharacters(11))
         newValue = RomanNumberValues(11)                     ! D = 500

         counters(11) = counters(11) + 1
         if (counters(11).gt.validCount(11)) then
            write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
            numberArabic = -1
            return
         end if

         if (i.ne.1) then
            if (numeral(i-1:i-1).eq.NumeralCharacters(9)) then   ! if C
               newValue = RomanNumberValues(10)               ! CD = 400
               isDoubleValueNumeral = .TRUE.

               counters(10) = counters(10) + 1
               if (counters(10).gt.validCount(10)) then
                  write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
                  numberArabic = -1
                  return
               end if

               if (counters(9).ge.counters(10)) then
                  write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. CD and C found together.'
                  numberArabic = -1
                  return
               end if
            counters(11) = counters(11) - 1
            end if
         end if

      CASE (NumeralCharacters(9))
         newValue = RomanNumberValues(9)                     ! C = 100

         counters(9) = counters(9) + 1
         if (counters(9).gt.validCount(9)) then
            write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
            numberArabic = -1
            return
         end if

         if (i.ne.1) then
            if (numeral(i-1:i-1).eq.NumeralCharacters(5)) then   ! if X
               newValue = RomanNumberValues(8)               ! XC = 90
               isDoubleValueNumeral = .TRUE.

               counters(8) = counters(8) + 1
               if (counters(8).gt.validCount(8)) then
                  write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
                  numberArabic = -1
                  return
               end if

               if (counters(5).ge.counters(8)) then
                  write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. XC and X found together.'
                  numberArabic = -1
                  return
               end if
            counters(9) = counters(9) - 1
            end if
         end if

      CASE (NumeralCharacters(7))
         newValue = RomanNumberValues(7)                     ! L = 50

         counters(7) = counters(7) + 1
         if (counters(7).gt.validCount(7)) then
            write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
            numberArabic = -1
            return
         end if

         if (i.ne.1) then
            if (numeral(i-1:i-1).eq.NumeralCharacters(5)) then   ! if X
               newValue = RomanNumberValues(6)               ! XL = 40
               isDoubleValueNumeral = .TRUE.

               counters(6) = counters(6) + 1
               if (counters(6).gt.validCount(6)) then
                  write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
                  numberArabic = -1
                  return
               end if

               if (counters(5).ge.counters(6)) then
                  write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. XL and X found together.'
                  numberArabic = -1
                  return
               end if
            counters(7) = counters(7) - 1
            end if
         end if

      CASE (NumeralCharacters(5))
         newValue = RomanNumberValues(5)                     ! X = 10

         counters(5) = counters(5) + 1
         if (counters(5).gt.validCount(5)) then
            write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
            numberArabic = -1
            return
         end if

         if (i.ne.1) then
            if (numeral(i-1:i-1).eq.NumeralCharacters(1)) then   ! if I
               newValue = RomanNumberValues(4)                  ! IX = 9
               isDoubleValueNumeral = .TRUE.

               counters(4) = counters(4) + 1
               if (counters(4).gt.validCount(4)) then
                  write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
                  numberArabic = -1
                  return
               end if

               if (counters(1).ge.counters(4)) then
                  write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. I and IX found together.'
                  numberArabic = -1
                  return
               end if
               counters(5) = counters(5) - 1
            end if
         end if

      CASE (NumeralCharacters(3))
         newValue = RomanNumberValues(3)                     ! V = 5

         counters(3) = counters(3) + 1
         if (counters(3).gt.validCount(3)) then
            write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
            numberArabic = -1
            return
         end if

         if (i.ne.1) then
            if (numeral(i-1:i-1).eq.NumeralCharacters(1)) then   ! if I
               newValue = RomanNumberValues(2)                  ! IV = 4
               isDoubleValueNumeral = .TRUE.

               counters(2) = counters(2) + 1
               if (counters(2).gt.validCount(2)) then
                  write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
                  numberArabic = -1
                  return
               end if

               if (counters(1).ge.counters(2)) then
                  write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. I and IV found together.'
                  numberArabic = -1
                  return
               end if
               counters(3) = counters(3) - 1
            end if
         end if

      CASE (NumeralCharacters(1))
         newValue = RomanNumberValues(1)         ! I = 1

         counters(1) = counters(1) + 1
         if (counters(1).gt.validCount(1)) then
            write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
            numberArabic = -1
            return
         end if

      CASE default
         write(*,*) 'ERROR: SELECT CASE selector wrong value...'
         if (debugMode) write(*,*) 'CASE default selector value =', numeral(i:i), 'for i =', i
      newValue = 0

   end select


   ! Update the number by newValue, if it's equal or larger than the previousValue.
      if (newValue >= previousValue) then
         numberArabic = numberArabic + newValue

   ! This traps successive double value numerals of the same order. (eg IXIV, XCXL, etc)
   ! if the new value is the second double Value Numeral in succession, stop the program.
         if (real(previousValue).eq.real(newValue)*(4./9.).and.isDoubleValueNumeral) then
            write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. Two double value numerals in succession'
            numberArabic = -1
            return
         end if

   ! This traps double value numerals followed by smaller numerals, which their sum would be assigned a larger value numeral
   ! (eg IXV = 9+5 = 14 = XIV)
         if (real(newValue).lt.real(previousValue*2).and.isDoubleValueNumeral) then
            write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. Double value numeral along with smaller numeral of the same order'
            numberArabic = -1
            return
         end if

      else    ! if newValue < previousValue
         write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION'
         numberArabic = -1
         return
      end if

   ! update the previous value in preparation of next iteration
      previousValue = newValue
   end do
   end function numeral_to_number
   !==================================================================================================================


   !==================================================================================================================
   !> Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
   !> Original author: Clive Page

   function to_upper(strIn) result(strOut)
   implicit none

   character(len=*), intent(in) :: strIn
   character(len=len(strIn)) :: strOut
   integer :: i,j

   do i = 1, len(strIn)
      j = iachar(strIn(i:i))
      if (j>= iachar("a") .and. j<=iachar("z") ) then
         strOut(i:i) = achar( iachar( strIn(i:i) ) - 32 )
      else
         strOut(i:i) = strIn(i:i)
      end if
   end do

   end function to_upper
   !==================================================================================================================



   end module routines

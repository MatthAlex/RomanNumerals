	module routines
	implicit none
	contains
	!==================================================================================================================
	! This routine tests whether the input string contains any invalid ASCII characters. If a null or invalid input is
	! detected, the routine stops the program.
	
	subroutine check_string_valid(StringValue)
	use parameters, only: ValidCharacters
	implicit none
	character(LEN=*), intent(in) :: StringValue
	logical			:: isCharacterValid
	integer			:: i, j

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
	
		if (isCharacterValid.eqv..FALSE.) then
			write(*,*) 'ERROR: ',StringValue, ' contains invalid ASCII characters.'
			stop
		end if 
	end do
	
	end subroutine check_string_valid
	!==================================================================================================================
	
	
	
	!==================================================================================================================
	! This routine tests whether the input string contains any invalid ASCII characters. If a null or invalid input is
	! detected, the routine stops the program.
	
	subroutine verify_string(StringValue)
	implicit none
	character(LEN=*), intent(in) :: StringValue
	logical			:: isCharacterValid
	integer 		:: Sresult

	Sresult = verify(StringValue, "IVXLCDM",isCharacterValid)
	if (Sresult.ne.0) then
		write(*,*) 'ERROR: ',StringValue, ' contains invalid ASCII characters.'
		stop
	end if 
		
	end subroutine verify_string
	!==================================================================================================================

    !==================================================================================================================
    ! This routine turns lowercase to uppercase, adjusts the string to the left, removes any trailing spaces, and allocates the new length.
    ! The resulting string cannot be empty.

    subroutine buffer_to_string(buffer, StringValue)
	use parameters, only: debugMode
    implicit none
    character(LEN=*)   			 , intent(in)  :: buffer
    character(LEN=:), allocatable, intent(out) :: StringValue

    StringValue = to_upper( trim(adjustL(buffer)) )
	
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
	integer, value 			:: numberArabic	! dummy value initialized to numberArabic
	integer			 					:: i, integerDivision
	character(LEN=:) , allocatable, intent(out)		:: numeral
	
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
	!SOURCE: https://www.rosettacode.org/wiki/Roman_numerals/DECODE#Fortran
	!The original version cannot understand invalid numeral progression. F.e. it can be fooled by IVIVIVI, IXI-IVI, CMCMDIXVIXVID, etc
	
	function numeral_to_number(numeral) result(numberArabic)
	use parameters, only: debugMode, RomanNumberValues, NumeralCharacters
	implicit none
	character(LEN=*), intent(in)   :: numeral
	integer				:: i, newValue, previousValue, numberArabic
	integer, dimension(13) :: counters
	logical				:: isDoubleValueNumeral
 
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
		if (isDoubleValueNumeral.eqv..TRUE.) then
			isDoubleValueNumeral = .FALSE.
			cycle
		end if
	
		select case(numeral(i:i))
		CASE (NumeralCharacters(13))
			newValue = RomanNumberValues(13)							! M = 1000
			call counting(13,counters)
			if (i.ne.1) then
				if (numeral(i-1:i-1).eq.NumeralCharacters(9)) then	! if C
					newValue = RomanNumberValues(12)					! CM = 900
					isDoubleValueNumeral = .TRUE.
					call counting(12,counters)
					if (counters(9).ge.counters(12)) then
						write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. CM and C found together.'
						stop
					end if
				counters(13) = counters(13) - 1
				end if
			end if
		
		CASE (NumeralCharacters(11))
			newValue = RomanNumberValues(11)							! D = 500
			call counting(11,counters)
			if (i.ne.1) then
				if (numeral(i-1:i-1).eq.NumeralCharacters(9)) then	! if C
					newValue = RomanNumberValues(10)					! CD = 400
					isDoubleValueNumeral = .TRUE.
					call counting(10,counters)
					if (counters(9).ge.counters(10)) then
						write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. CD and C found together.'
						stop
					end if
				counters(11) = counters(11) - 1
				end if
			end if
		
		CASE (NumeralCharacters(9))
			newValue = RomanNumberValues(9)							! C = 100
			call counting(9,counters)
			if (i.ne.1) then
				if (numeral(i-1:i-1).eq.NumeralCharacters(5)) then	! if X
					newValue = RomanNumberValues(8)					! XC = 90
					isDoubleValueNumeral = .TRUE.
					call counting(8,counters)
					if (counters(5).ge.counters(8)) then
						write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. XC and X found together.'
						stop
					end if
				counters(9) = counters(9) - 1
				end if
			end if
		
		CASE (NumeralCharacters(7))
			newValue = RomanNumberValues(7)							! L = 50
			call counting(7,counters)
			if (i.ne.1) then
				if (numeral(i-1:i-1).eq.NumeralCharacters(5)) then	! if X
					newValue = RomanNumberValues(6)					! XL = 40
					isDoubleValueNumeral = .TRUE.
					call counting(6,counters)
					if (counters(5).ge.counters(6)) then
						write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. XL and X found together.'
						stop
					end if
				counters(7) = counters(7) - 1
				end if
			end if
		
		CASE (NumeralCharacters(5))
			newValue = RomanNumberValues(5)							! X = 10
			call counting(5,counters)
			if (i.ne.1) then
				if (numeral(i-1:i-1).eq.NumeralCharacters(1)) then	! if I
				newValue = RomanNumberValues(4)						! IX = 9
				isDoubleValueNumeral = .TRUE.
				call counting(4,counters)
					if (counters(1).ge.counters(4)) then
						write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. I and IX found together.'
						stop
					end if
				counters(5) = counters(5) - 1
				end if
			end if
		
		CASE (NumeralCharacters(3))
			newValue = RomanNumberValues(3)							! V = 5
			call counting(3,counters)
			if (i.ne.1) then
				if (numeral(i-1:i-1).eq.NumeralCharacters(1)) then	! if I
				newValue = RomanNumberValues(2)						! IV = 4
				isDoubleValueNumeral = .TRUE.
				call counting(2, counters)
					if (counters(1).ge.counters(2)) then
						write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. I and IV found together.'
						stop
					end if
				counters(3) = counters(3) - 1
				end if
			end if
		
		CASE (NumeralCharacters(1))
			newValue = RomanNumberValues(1)			! I = 1
			call counting(1,counters)

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
			if (real(previousValue).eq.real(newValue)*4/9.and.isDoubleValueNumeral.eqv..TRUE.) then
				write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. Two double value numerals in succession'
				stop
			end if

	! This traps double value numerals followed by smaller numerals, which their sum would be assigned a larger value numeral
	! (eg IXV = 9+5 = 14 = XIV)
			if (real(newValue).lt.real(previousValue*2).and.isDoubleValueNumeral.eqv..TRUE.) then
				write(*,*) 'ERROR: INVALID NUMERAL SUCCESSION. Double value numeral along with smaller numeral of the same order'
				stop
			end if
	
		else   ! if newValue < previousValue
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
	use parameters, only: validCount
	implicit none
	integer, intent(in) :: order
	integer, dimension(13), intent(inout) :: counters

	counters(order) = counters(order) + 1
	if (counters(order).gt.validCount(order)) then
		write(*,*) 'ERROR: INVALID NUMERAL COUNT. MULTIPLES FOUND.'
		stop
	end if
	
	end subroutine counting
	!==================================================================================================================
    
	
	
	!==================================================================================================================  
	function to_upper(strIn) result(strOut)
	! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
	! Original author: Clive Page
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

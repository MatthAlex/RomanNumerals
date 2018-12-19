	!> Contains all constants used in the program
	module parameters
	implicit none

	!> populate character array of 7 1-length characters, uppercase only
	character(LEN=1), parameter :: ValidCharacters(7) = [ 'I', 'V', 'X', 'L', 'C', 'D', 'M' ]

	!> populate all roman numerals in one array by descending order - will need trim
	character(LEN=2), parameter :: NumeralCharacters(13) = ['I ','IV','V ','IX','X ','XL','L ','XC','C ','CD','D ','CM','M ']

	!> equivalent numerical values to NumeralCharacters
	integer, parameter :: RomanNumberValues(13)= [1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000]

	!> the valid numeral count corresponds to Roman Numerals up to 3999 in value.
	integer, parameter :: validCount(13)= [3,1,1,1,3,1,1,1,3,1,1,1,3]

	!> enable to print debug information
	logical	 :: debugMode = .FALSE.

	end module parameters

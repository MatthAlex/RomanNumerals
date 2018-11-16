      MODULE PARAMETERS
      implicit none
      
      character(LEN=1) :: ValidArray(7)
      character(LEN=2) :: ValidArrayPlus(6)
      character(LEN=2) :: ValidArrayAll(13)

      integer(kind=2) :: SingleRomanValues(7)= (/ 1, 5, 10, 50, 100, 500, 1000 /)
      integer(kind=2) :: DoubleRomanValues(6)= (/ 4, 9, 40, 90, 400, 900 /)
      integer(kind=2) :: RomanValues(13)= (/1,4,5,9,10,40,50,90,100,400,500,900,1000/)
      integer(kind=2) :: validCount(13)= (/3,1,1,1,3,1,1,1,3,1,1,1,3/)

      DATA ValidArray / 'I', 'V', 'X', 'L', 'C', 'D', 'M' / ! populate character array of 7 1-length characters
      DATA ValidArrayPlus / 'IV', 'IX', 'XL', 'XC', 'CD', 'CM' / ! populate character array of 6 2-length characters
      DATA ValidArrayAll /'I','IV','V','IX','X','XL','L','XC','C','CD','D','CM','M' / ! populate all roman numerals in one array - will need trim

      !SingleRomanValues 
      !DoubleRomanValues 

      END MODULE
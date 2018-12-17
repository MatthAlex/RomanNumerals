#! /bin/bash

filename="$1"


while read -r line

do
#echo $line
#numeral=$line | `sed 's/[[:space:]]//g'`
#numeral= $line | sed 's/[[:space:]]//g'

#echo $numeral
./RomanNum I $line

done < "$filename"


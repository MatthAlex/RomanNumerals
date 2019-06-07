#! /bin/bash

filename="$1"
mydir="$(dirname "${0}")"

while read -r line

do
#numeral=$line | `sed 's/[[:space:]]//g'`
#numeral= $line | sed 's/[[:space:]]//g'

$mydir/../src/RomanNum I $line

done < "$filename"


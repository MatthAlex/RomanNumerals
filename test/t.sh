#! /bin/bash

make clean
make test
make test2
make test3
make test4

echo -e "\nRunning test..."
./test | awk '/Test/ {printf $0} /stop/ {printf $0}'

echo -e "\nRunning test2..."
./test2 | awk '/Test/ {printf $0} /stop/ {printf $0}'

echo -e "\nRunning test3..."
./test3 | awk '/Test/ {printf $0} /stop/ {printf $0}'

echo -e "\nRunning test4..."
./test4 | awk '/Test/ {printf $0} /stop/ {printf $0}'

echo

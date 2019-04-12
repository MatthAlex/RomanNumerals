#! /bin/bash

make clean
make test
make test2
make test3
make test4

echo "Running test..."
./test

echo "Running test2..."
./test2

echo "Running test3..."
./test3

echo "Running test4..."
./test4
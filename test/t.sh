#! /bin/bash

make clean
make test
make test2

echo "Running test"
./test

echo "Running test2"
./test2
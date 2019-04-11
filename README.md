# Task Revisited

## Building and Running the program

To clean the directory of objects and modules:

```
cd TaskRevisited/src/
make clean
```

To build the debug version of the program: `make debug`.

To build the prod version: `make build`.

## Testing

After building the prod version;
To output the nominal sum of all numerals from 1 to 3999 plus 1:

```
cd TaskRevisited/tests/
./qrun.sh ronsPlain.txt
```

To test the input of ASCII characters:

```
cd TaskRevisited/tests/
./qrun.sh asciiPlain.txt
```

### In-depth testing

Unit testing setup.

- [x] Source code readied

- [ ] Testing programs setup

Move into the `test` folder and run the tests:

```
cd TaskRevisited/test
. t.sh
```

The `t.sh` script will clean the test directory, compile all tests and run them sequentially. In the case of a failure, the tests will abort.

Use `. t.sh >> a.dat 2>&1` to pipe the output to a file, and `cat a.dat` to view the contents.
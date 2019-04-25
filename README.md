# Task Revisited

## Building and Running the program

To clean the directory of objects and modules, executables, and logs: `make clean`.

To build the debug version of the program: `make debug`.

To build the production version: `make all`.

## Testing

After building the production version;
To output the nominal sum of all numerals from 1 to 3999 plus 1:

```bash
cd TaskRevisited/tests/
./qrun.sh ronsPlain.txt
```

To test the input of ASCII characters:

```bash
cd TaskRevisited/tests/
./qrun.sh asciiPlain.txt
```

### In-depth testing

Unit testing setup.

- [x] Source code readied

- [x] Testing programs setup

- [x] Makefile included functionality

- [x] Jenkins set to run tests

At the root directory run:

```bash
make check
```

To propagate through all tests despite STOP errors run `make check -k` instead. Jenkins *should* have this enabled.

This will compile all tests and run them. Any invalid exit codes will be picked up, Jenkins included.
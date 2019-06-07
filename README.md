# Task Revisited

## process

- unzip
`gzip -cd taskrevisited-<version>.tar.gz | tar xvf -`

- configure
`./configure`

- build
`make`

- test
`make check`

- install
`sudo make install`

- uninstall
`sudo make uninstall`

- build outside of source folder
`mkdir /path/to/<directory>`
`./relative/path/to/configure`
Then follow previous steps.

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

This will compile all tests and run them. Any invalid exit codes will be picked up, Jenkins as well.

## Building documentation with FORD

Installation instructions can be found [here](https://github.com/Fortran-FOSS-Programmers/ford).

For graphs, install [Graphviz](https://packages.ubuntu.com/cosmic/graphviz), and graphviz: `pip install graphviz`.

The relevant project file is `ford.md`.

To produce documentation run: `ford ford.md`.

Documentation will be built under `/doc/`.

Read `index.html` with any browser.

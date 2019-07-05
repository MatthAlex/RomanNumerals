# Project overview

The program `RomanNum` takes two arguments of Roman Numerals, in the range of 1 to 3999, and outputs their sum in Numeral form.

## "Toy" project

Initially the task for the interview, this project has been the testbed for the implementation of many different tools and application of processes.

* Used as the basis of the first Code Review.
* Good code practices implemented. This includes several things.
* Correct directory structure implemented.
* Applied automated documentation (Ford).
* Implemented a series of tests.
* Coupling with Jenkins.
* Used in a Singularity container.
* Implementation of Autotools build system.
* Implementation of meson build system. WIP

### Current structure

The code has been segregated in the main program and a convenience library. Main development branch is `convenienceLibrary` (possible merge with master).

```bash
AUTHORS
common/
    Makefile.am
    parameters.f90
    routines.f90
configure.ac
doc/
    ford/
    Makefile.am
.gitignore
m4/
    aqm_prog_ford.m4
    .gitignore
Makefile.am
README.md
src/
    .libs/
    Makefile.am
    RomanNum.f90
taskrev-ford.md.in
test/
    .libs/
    Makefile.am
    ronsPlain.txt
    test.f90
    test2.f90
    test3.f90
    test4.f90
```

## Building, running, and testing the program

The repository can be cloned (SSH key):

`git clone git@github.research.its.qmul.ac.uk:[REDACTED]/TaskRevisited.git`

Change directory into the root of the program:

`cd TaskRevisited`

To compile it, two prerequisites must be cleared. A modern Fortran compiler and an installation of GNU Autotools. On Apocrita, instead, use:

```bash
module load gcc autoconf-archive
<or>
module load intel autoconf-archive
```

Running the autotools will install all needed programs and scripts, and will produce the `configure` executable from `configure.ac`.

`autoreconf -if`

Next step is to run `configure`, which will produce `Makefile.in` and `Makefile` scripts for each `Makefile.am` script, recursively. If no documentation is needed, run:

`./configure -with-ford=no`

At the end of the configuration, the script offers useful information, including some often-used `make` targets.

```bash
Now type `make <target>`
 where the optional <target> is:
 all                - build all binaries
 install            - install executable RomanNum
 clean              - clean everything
 dist               - create a distribution package
 check              - build and run tests
 ```

To install/uninstall the executable sudo access will be requested

```bash
sudo make install
which RomanNum
sudo make uninstall
```

To propagate through all tests despite STOP errors run `make check -k` instead. Jenkins *should* have this enabled. This will compile all tests and run them. Any invalid exit codes will be picked up, resulting in a failed build, including Jenkins.

A lot of useful information can be accessed by running the configuration script with help:

`./configuration -help`

### Building and installing the program in non-default locations

To build the program in an out of source directory [1](https://softwareengineering.stackexchange.com/questions/88792/automake-how-to-keep-the-source-tree-clean) first run `autoreconf (-if)` in the root source directory. Then choose and create another build directory: `mkdir -p /path/to/custom/builddir/`. Change directory and run the configuration script using a relative or absolute path. For example:

```bash
cd TaskRevisited
autoreconf -if
mkdir builddir
cd builddir
../configuration --<options>
make all
```

By default, `make install` will install all the files in `/usr/local/bin`. To install the program in (or uninstall from) a chosen location, use option `prefix` for `make` and `--prefix` for configure. This will install the executable in `/prefix/bin/`.

```bash
make prefix=$HOME install
./configuration --prefix=$HOME
```

This will change the install directory of `RomanNum` to `/home/<usr>/bin/`.

### Changing compiler and flags

The `configure` script will automaticall detect and choose one of the **available** Fortran compilers. All make targets will use the chosen compiler. To force the use of another compiler, either load the correct module and rerun the autotools and configuration, or force the script to accept the compiler of choice:

`./configure --with-ford=no FC=<Fortran compiler>`

where Fortran compiler can be gfortran, ifort, nagfor, pgfortran, etc...

The `FC` compiler flag can also be hijacked during building, but inconsistent use may lead to issues with different compiled versions of modules and object files:

`make FC=<Fortran compiler> <target>`

To change the compiler flags, use `FCFLAGS`.

`make FCFLAGS='-g -O2 -fbacktrace -Wall' check`

### Building documentation with Ford

Ford is an automatic documentation program for Fortran, similar to Doxygen.

#### Installation

Installation instructions can be found [here](https://github.com/Fortran-FOSS-Programmers/ford).

For graphs, install graphviz: `pip install graphviz`.

The relevant project file is `taskrev-ford.md`, which is expanded from `taskrev-ford.md.in` during the execution of `configuration`. The documentation files will be built under `doc/ford/`.

To produce documentation run the program using the project file: `ford taskrev-ford.md`.

Read `index.html` with any browser. For example, `firefox ./path/to/index.html`, or drag and drop into a new tab.

#### Building documentation as part of building the program

This requires Ford to be installed, or the ford container on Apocrita located.

`./configure --with-ford=`which ford`

### Building with Meson build system

Located on `mesonTry` branch, this is a WIP.

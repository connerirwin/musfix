# MUSFix
General purpose version of Synquid's Horn clause solver.

# Building MUSFix
1. Install [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

2. Install the Z3 Theorem Prover v4.7.1

        git clone --branch z3-4.7.1 https://github.com/Z3Prover/z3.git
        cd z3
        python scripts/mk_make.py
        cd build; make
        sudo make install

   After building Z3 it may be necessary to update the system's library cache:

        sudo ldconfig

3. Install MUSFix

        git clone https://github.com/connerirwin/musfix.git
        cd musfix
        stack build

4. Run MUSFix

        stack exec -- musfix

# Command Line Options

Usage:

    musfix INPUT_FILES [-o|--output FILE] [-a|--append] [-s|--silent] [--verbose] [-l|--least-fixpoint] [--version]

Run a fixpoint solver on INPUT_FILES to find all solutions satisfying the constraints

Available options:

    -h,--help                Show this help text
    -o,--output FILE         Prints results to the specified file
    -a,--append              Append file output
    -s,--silent              Supresses command-line output
    --verbose                Output additional logging
    -l,--least-fixpoint      Use a least-fixpoint solver (default is greatest)
    --version                Show current version

# .msmt File Format

Uninterpreted Sorts

    (declare-sort NAME NUM_ARGS)

Constants

    (declare-const NAME SORT)

Distinct Constants

    (assert (distinct NAMES))

Uninterpreted Functions

    (declare-fun NAME (ARGUMENT_SORTS) RETURN_SORT)

Qualifiers

    (qualif NAME ((VAR_NAME VAR_SORT)...) (EXPRESSION))

Well-formedness Constraints

    (wf $NAME (VARS))

Horn Constraints

    (constraint (forall ((VARS)) (=> EXPRESSION EXPRESSION)))

Example Files

[Sorts, Functions, Qualifiers](test/sample/nadia.msmt)

[Distinct Constants](test/pos/lit00.msmt)


# Using Development Scripts

To run the program quickly

    run.sh [musfix args]

To run the program in profiling mode.

    profile.sh [musfix args] +RTS [ghc args]

Example ghc arguments:
* -p &nbsp;&nbsp;&nbsp; Generates musfix.prof containing time and allocation statistics
* -xc &nbsp;&nbsp; Dumps a stack trace when an exception is raised

A full list can be found [here](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html)

# Integration Testing

To run the integration tests

    stack test

To accept changes to the test suite and regenerate the template files

    TASTY_ACCEPT=true stack test

To verify changes to the tests

    git diff --word-diff --patience --color

# Debugging in GHCi

To set a breakpoint

    :break identifier
    :break [module] line [column]

To set program arguments

    :set args arguments_to_main

Then run with tracing enabled

    :trace main

More details [here](https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/ghci-debugger.html)

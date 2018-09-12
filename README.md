# Musfix

General purpose version of Synquid's Horn clause solver.

# Building Musfix

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

2. Install the Z3 Theorem Prover v4.7.1

        git clone --branch z3-4.7.1 https://github.com/Z3Prover/z3.git
        cd z3
        python scripts/mk_make.py
        cd build; make
        sudo make install

   After building Z3 it may be necessary to update the system's library cache:

        sudo ldconfig

3. Build Musfix

        git clone https://github.com/connerirwin/musfix.git
        cd musfix
        stack build

4. Run Musfix

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

Check out the [wiki](https://github.com/connerirwin/musfix/wiki/MSMT-File-Format) for more details

# Using Development Scripts

To run the program quickly

    run.sh [musfix_args]

To run the program in profiling mode (using -xc by default)

    profile.sh [musfix_args] +RTS [ghc_args]

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
    
# Dependencies

For the most part, Musfix relies on dependencies available on Hackage. The lone exception at this time is `atto-lisp` which has been forked and updated to use the `scientific` package to avoid deprecation warnings.

The repository and commit used by musfix are referenced in `stack.yaml`. If a different build system is used, it should be directed to the same repo and branch as the stack file.

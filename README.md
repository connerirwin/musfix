# MUSFix
General purpose version of Synquid's greatest-fixpoint Horn clause solver.

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

        stack exec -- musfix --help

# Using development scripts

To run the program quickly

    run.sh [musfix args]

To run the program in profiling mode.

    profile.sh [musfix args] +RTS [ghc args]

Example ghc arguments:
* -p &nbsp;&nbsp; Generates musfix.prof containing time and allocation statistics

A full list can be found [here](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html)

To run rudimentary integration testing (this will diff your output against rubric)

    verify.sh

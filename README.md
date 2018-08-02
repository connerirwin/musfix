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

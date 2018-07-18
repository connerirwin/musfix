# MUSFix
General purpose version of Synquid's greatest-fixpoint Horn clause solver.

# Building MUSFix
1. Install the [Haskell Platform](https://www.haskell.org/platform/) and [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) *TODO: Is Haskell Platform needed with Stack?*
2. *TODO: Finish (for now, follow steps in Building Synquid)

# Building Synquid
1. Install the [Haskell Platform](https://www.haskell.org/platform/) and [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2. Install the Z3 Theorem Prover v4.7.1

        git clone --branch z3-4.7.1 https://github.com/Z3Prover/z3.git
        cd z3
        python scripts/mk_make.py
        cd build; make
        sudo make install
    
3. Install Synquid

        hg clone https://bitbucket.org/nadiapolikarpova/synquid
        cd synquid
   
   Change the version of haskell z3 bindings from 4.1.2 to 4.3.1 in stack.yaml under `extra-deps`
   
        stack setup && stack build

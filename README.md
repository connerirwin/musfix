# musfix
General purpose version of Synquid's greatest-fixpoint Horn clause solver.

# Install Instructions
1. Install the Haskell Platform
   `sudo apt install haskell-platform`
   and Stack </br>
   `apt install haskell-platform` </br>
   `stack update` </br>
   (alternatively `curl -sSL https://get.haskellstack.org/ | sh`)
2. Install the Z3 Theorem Prover v4.3.2 </br>
   `git clone --branch z3-4.3.2 https://github.com/Z3Prover/z3.git` </br>
   `python scripts/mk_make.py` </br>
   `cd build; make` </br>
   `sudo make install`
3. Install Z3 Bindings for Haskell </br>
   download z3-4.1.2.tar.gz from http://hackage.haskell.org/package/z3 </br>
   `tar -xf z3-4.1.2.tar.gz` </br>
   `cd z3-4.1.2/` </br>
   `cabal install --extra-include-dirs=ABSOLUTEPATH/z3/src/api --extra-lib-dirs=ABSOLUTEPATH/z3/build`

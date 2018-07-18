# musfix
General purpose version of Synquid's greatest-fixpoint Horn clause solver.

# Install Instructions
1. Install the Haskell Platform </br>
   `sudo apt install haskell-platform`
   and Stack </br>
   `apt install haskell-stack` </br>
   `stack update` </br>
   (alternatively `curl -sSL https://get.haskellstack.org/ | sh`)
2. Install the Z3 Theorem Prover v4.7.1 </br>
   `git clone --branch z3-4.7.1 https://github.com/Z3Prover/z3.git` </br>
   `python scripts/mk_make.py` </br>
   `cd build; make` </br>
   `sudo make install`
3. Install Synquid </br>
   `hg clone https://bitbucket.org/nadiapolikarpova/synquid` </br>
   `cd synquid` </br>
   change the version of haskell z3 bindings from 4.1.2 to 4.3.1 in stack.yaml under extra-deps: </br>
   `stack setup && stack build` </br>

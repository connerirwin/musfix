-- | Interface to Synquid solvers
module Language.Synquid.SolverMonad where

import Language.SMT.Syntax
import Language.Synquid.Logic
import Language.Synquid.Program
import Data.Map
import Data.Set
import Control.Applicative

data Preamble = Preamble {
  preambleConstants :: [(Id, Sort)],
  preambleDistinctAssertions :: [[Id]]
}

class (Monad s, Applicative s) => MonadSMT s where
  initSolver :: Environment -> Preamble ->  s ()                     -- ^ Initialize solver
  isSat :: Formula -> s Bool                                              -- ^ 'isSat' @fml@: is @fml@ satisfiable?
  allUnsatCores :: Formula -> Formula -> [Formula] -> s [[Formula]]       -- ^ 'allUnsatCores' @assumption@ @mustHave@ @fmls@: all minimal unsatisfiable subsets of @fmls@ with @mustHave@, which contain @mustHave@, assuming @assumption@

class (Monad s, Applicative s) => MonadHorn s where
  initHornSolver :: Environment -> Preamble -> s Candidate                               -- ^ Initial candidate solution
  preprocessConstraint :: Formula -> s [Formula]                                              -- ^ Convert a Horn clause to the format this solver can handle
  checkCandidates :: Bool -> [Formula] -> ExtractAssumptions ->[Candidate] -> s [Candidate]   -- ^ Check validity or consistency of constraints under current candidates
  refineCandidates :: [Formula] -> QMap -> ExtractAssumptions -> [Candidate] -> s [Candidate] -- ^ Refine current candidates to satisfy new constraints
  pruneQualifiers :: QSpace -> s QSpace                                                       -- ^ Prune redundant qualifiers

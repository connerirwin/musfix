-- | This is the entry point into the fixpoint solver. Currently, not all horn
-- solver parameters are configureable through command-line arguments, but if
-- necessary that could be added fairly easily by adding parameters to
-- SolverInputs and ProgramOptions in Main.hs

module Language.SMT.Solve (
  findFixPoint,
  SolverInputs (..),
) where

import Language.SMT.Syntax

import Language.Synquid.HornSolver
import Language.Synquid.Logic
import Language.Synquid.Program
import Language.Synquid.SolverMonad
import Language.Synquid.Util
import Language.Synquid.Z3

import Control.Monad.Reader

import qualified Data.Set as Set

type HornSolver = FixPointSolver Z3State

defaultHornSolverParams = HornSolverParams {
  pruneQuals = True,
  isLeastFixpoint = False,
  optimalValuationsStrategy = MarcoValuations,
  -- These have been modified for precondition testing but should probably be left on otherwise
  semanticPrune = True,
  agressivePrune = True,
  candidatePickStrategy = InitializedWeakCandidate,
  constraintPickStrategy = SmallSpaceConstraint,
  solverLogLevel = 0
  }

data SolverInputs = SolverInputs {
  useLeastFixpoint :: Bool,
  constraints :: [Formula],
  qualifierMap :: QMap,
  inConsts :: [(Id, Sort)],
  inDistinctConsts :: [[Id]],
  inSolverLogLevel :: Int,
  inPruning :: Bool
}

-- | Finds fix point canidates
findFixPoint :: SolverInputs -> IO [Candidate]
findFixPoint inputs = evalZ3State $ evalFixPointSolver (computeFixPoints inputs) params
  where
    params = defaultHornSolverParams {
        isLeastFixpoint = (useLeastFixpoint inputs),
        solverLogLevel = (inSolverLogLevel inputs),
        semanticPrune = (inPruning inputs),
        agressivePrune = (inPruning inputs),
        pruneQuals = (inPruning inputs)
      }

-- | Compute the fix points
computeFixPoints :: SolverInputs -> HornSolver [Candidate]
computeFixPoints ins = do
    let pre = Preamble {
      preambleConstants = inConsts ins,
      preambleDistinctAssertions = inDistinctConsts ins
    }
    initCand <- initHornSolver emptyEnv pre
    procCons <- mapM preprocessConstraint $ constraints ins
    let procCons' = concat procCons
    let qmap = qualifierMap ins
    let extractAssumptions = \_ -> Set.empty -- instantiateConsAxioms
    allCandidates <- refineCandidates procCons' qmap extractAssumptions [initCand]
    return allCandidates

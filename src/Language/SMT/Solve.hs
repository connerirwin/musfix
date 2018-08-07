module Language.SMT.Solve (
  findFixPoint,
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

import Debug.Trace

type HornSolver = FixPointSolver Z3State

defaultHornSolverParams = HornSolverParams {
  pruneQuals = True,
  isLeastFixpoint = False,
  --isLeastFixpoint = True,
  optimalValuationsStrategy = MarcoValuations,
  semanticPrune = True,
  agressivePrune = True,
  candidatePickStrategy = InitializedWeakCandidate,
  constraintPickStrategy = SmallSpaceConstraint,
  solverLogLevel = 0
  }

-- | Finds fix point canidates
findFixPoint :: Bool -> [Formula] -> QMap -> IO [Candidate]
findFixPoint useLeastFixpoint constraints qmap = evalZ3State $ evalFixPointSolver (computeFixPoints constraints qmap) params
  where
    params = defaultHornSolverParams { isLeastFixpoint = useLeastFixpoint } -- TODO: Make this more elegant


-- | Compute the fix points
computeFixPoints :: [Formula] -> QMap -> HornSolver [Candidate]
computeFixPoints constraints qmap = do
    initCand <- initHornSolver emptyEnv
    procCons <- mapM preprocessConstraint constraints
    let procCons' = foldl (++) [] procCons
      in do
        allCandidates <- refineCandidates procCons' qmap nothing [initCand]
        return allCandidates
  where
    nothing = \_ -> Set.empty

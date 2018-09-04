module Language.SMT.Solve (
  findFixPoint,
  SolverInputs (..)
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
  optimalValuationsStrategy = MarcoValuations,
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
  constants :: [(Id, Sort)],
  distincts :: [[Id]]
}

-- | Finds fix point canidates
findFixPoint :: SolverInputs -> IO [Candidate]
findFixPoint inputs = evalZ3State $ evalFixPointSolver (computeFixPoints inputs) params
  where
    params = defaultHornSolverParams { isLeastFixpoint = (useLeastFixpoint inputs) }
    
prepareEnv :: SolverInputs -> Environment
prepareEnv ins = emptyEnv

-- | Compute the fix points
computeFixPoints :: SolverInputs -> HornSolver [Candidate]
computeFixPoints ins = do
    initCand <- initHornSolver emptyEnv
    procCons <- mapM preprocessConstraint $ constraints ins
    let procCons' = foldl (++) [] procCons
    let qmap = qualifierMap ins
    allCandidates <- refineCandidates procCons' qmap nothing [initCand]
    return allCandidates
  where
    nothing = \_ -> Set.empty

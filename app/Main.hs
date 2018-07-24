{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString as A
import qualified Data.AttoLisp as L
import qualified Data.Map as M

import Control.Monad.State

import Language.SMT.HornSolver
import Language.SMT.Logic
import Language.SMT.Parser
import Language.SMT.Resolver
import Language.SMT.Util
import Language.SMT.Z3

main :: IO ()
main = do
        print $ f
    where
        l = case A.parseOnly L.lisp "(+ (v1 int) (+ 2 (- 5)))" of
            (Left err) -> error $ "bad input: " ++ err
            (Right lexpr) -> lexpr
        f = case L.parseMaybe parseFormula l of
            (Just formula) -> formula
            Nothing -> error "bad formula"


type HornSolver = FixPointSolver Z3State

-- | Solves for the least fixpoint(s) and greatest fixpoint of a constraint
-- with the given qualifiers
musfixpoint :: Id -> Sort -> Formula -> IO ()
musfixpoint var sort fml = print "resolvedTypes eventually..."
  where
    resolvedTypes = resolveTypeRefinement sort fml

-- | Create a qualifier (refinement) map from a set of possible qualifiers
-- This
-- TODO ID field, currently just v
generateQualifiers :: [Formula] -> QMap
generateQualifiers fmls = M.singleton "v" qualifSpace
  where
    qualifSpace = toSpace Nothing fmls

{-
-- | Cleans up formulas
resolveTypeRefinement :: Sort -> Formula -> Resolver Formula
we are trying to infer function pre-conditions (ie refinements on the params)

type Resolver a = StateT ResolverState (Except ErrorMessage) a

data ResolverState = ResolverState {
  _environment :: Environment,
  _goals :: [(Id, (UProgram, SourcePos))],
  _checkingGoals :: [(Id, (UProgram, SourcePos))],
  _condQualifiers :: [Formula],
  _typeQualifiers :: [Formula],
  _mutuals :: Map Id [Id],
  _inlines :: Map Id ([Id], Formula),
  _sortConstraints :: [SortConstraint],
  _currentPosition :: SourcePos,
  _idCount :: Int
}

toSpace :: Maybe Int -> [Formula] -> QSpace

type HornSolver = FixPointSolver Z3State

-- | Parameters for constraint solving
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

initHornSolver :: Environment -> s Candidate
preprocessConstraint :: Formula -> s [Formula]

-- remove extract assumptions
refineCandidates :: [Formula] -> QMap -> ExtractAssumptions -> [Candidate] -> s [Candidate]

data Candidate = Candidate {
    solution :: Solution,
    validConstraints :: Set Formula,
    invalidConstraints :: Set Formula,
    label :: String
  } deriving (Show)
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString as A
import qualified Data.AttoLisp as L
import qualified Data.Map as M

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

type HornSolver = FixPointSolver Z3State

solveForConstraints :: Id -> Sort -> Formula -> IO ()
solveForConstraints var sort formula = print "resolvedTypes eventually..."
  where
    -- TODO figure out how resolveTypeRefinement works, is it even what we want?
    resolvedTypes = resolveTypeRefinement sort formula
    --Resolver s = resolvedTypes

    qualifSpace = toSpace Nothing [formula]
    qualifMap = M.singleton var qualifSpace

{-
resolveTypeRefinement :: Sort -> Formula -> Resolver Formula
toSpace :: Maybe Int -> [Formula] -> QSpace

type HornSolver = FixPointSolver Z3State

initHornSolver :: Environment -> s Candidate
preprocessConstraint :: Formula -> s [Formula]

-- remove extract assumptions
refineCandidates :: [Formula] -> QMap -> ExtractAssumptions -> [Candidate] -> s [Candidate]
-}

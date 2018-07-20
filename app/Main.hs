{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString as A
import Language.SMT.Parser
import qualified Data.AttoLisp as L
import Language.SMT.HornSolver

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

main :: IO ()
main = do
        print $ f
    where
        l = case A.parseOnly L.lisp "(qualif Neg ((v1 int) (v2 int)) (+ v1 23))" of
                Left err -> error $ "bad input: " ++ err
                Right lexpr -> lexpr
        f = case L.parse parseInputExpr l of
                L.Success input -> input
                L.Error reason -> error $ "bad input: " ++ reason ++ " \n attempting to parse lisp: " ++ (show l)
    
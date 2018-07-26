module Language.SMT.Resolver where

import Language.SMT.Syntax

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

import Control.Lens
import Control.Monad
import Control.Monad.State

import Language.Synquid.HornSolver
import Language.Synquid.Logic
import Language.Synquid.Program
import Language.Synquid.Util
import Language.Synquid.Z3
import Language.Synquid.ResolverSynq

{- Util -}
debugOut a = traceShow a a

unifyWith f = foldr (union . f) []
unify = unifyWith id

{- Debug Testing -}
-- | If the qualifiers reuse variable names, they must be the same type

input = [
  Qualifier "Pos" [Var IntS "v"] (Binary Le (IntLit 0) (Var AnyS "v")),
  Qualifier "Neg" [Var IntS "v"] (Binary Le (Var AnyS "v") (IntLit 0)),
  Qualifier "NeqZ" [Var IntS "v"] (Unary Not (Binary Eq (Var AnyS "v") (IntLit 0))),
  Qualifier "False" [] (Binary Eq (IntLit 66) (IntLit 77)),
  -- | if the qualifier contains a type not present in the wf constraint, it breaks
  -- Qualifier "('^')" [Var BoolS "x"] (Binary Eq (Var AnyS "x") (Var AnyS "x")),
  WFConstraint "$k0" [Var IntS "v0"]
  --HornConstraint (All (Var IntS "v1") (Binary Implies (Pred AnyS "$k0" []) (Binary Lt (IntLit 0) (Binary Plus (Var AnyS "v1") (IntLit 1))))),
  --HornConstraint (All (Var IntS "v2") (Binary Implies (Binary Eq (Var AnyS "v2") (IntLit 10)) (Pred AnyS "$k0" [])))
  ]

qmap = generateQualifiers $ map resolveInputSorts input

-- | A debug printing function designed to be as unobtrusive as possible
resolverDebug :: IO ()
resolverDebug = do
  print qmap

-- | Create a qualifier (refinement) map from a set of possible qualifiers for a
-- given unknown
generateQualifiers :: [InputExpr] -> QMap
generateQualifiers input = Map.fromList qualifiers
  where
    wfconstraints = allWFConstraints input
    rawQualifiers = map qualifEq $ allQualifiers input
    qualifiers = zip (map wfName wfconstraints) (map substituteQualifiers wfconstraints)

    -- | Substitute the wfconstraint formal parameters into the qualifiers
    substituteQualifiers :: InputExpr -> QSpace
    substituteQualifiers wfconstraint = toSpace Nothing subbedQualifiers
      where
        subbedQualifiers = foldr (union . \qualif -> allSubstitutions qualif formals actuals) [] rawQualifiers
        formals = foldr (union . extractVars) [] rawQualifiers -- ^ vars in the rawQualifiers
        actuals = wfFormals wfconstraint -- ^ vars in the wfconstraint

extractVars :: Formula -> [Formula]
extractVars (SetLit _ fs)  = unify $ map extractVars fs
extractVars v@(Var _ _)    = [v]
extractVars (Unary _ f)    = extractVars f
extractVars (Binary _ x y) = unify $ map extractVars [x,y]
extractVars (Ite x y z)    = unify $ map extractVars [x,y,z]
extractVars (Pred _ _ fs)  = unify $ map extractVars fs
extractVars (Cons _ _ fs)  = unify $ map extractVars fs
extractVars (All x y)      = unify $ map extractVars [x,y]
extractVars _              = []

-- | 'allSubstitutions' @env qual formals actuals@:
-- all well-typed substitutions of @actuals@ for @formals@ in a qualifier @qual@
allSubstitutions :: Formula -> [Formula] -> [Formula] -> [Formula]
allSubstitutions (BoolLit True) _ _ = []
allSubstitutions qual formals actuals = do
  (sortSubst, subst, _) <- foldM (go Set.empty) (Map.empty, Map.empty, actuals) formals
  return $ substitute subst $ sortSubstituteFml sortSubst qual
  where
    go tvs (sortSubst, subst, actuals) formal = do
      let formal' = sortSubstituteFml sortSubst formal
      actual <- actuals
      case unifySorts tvs [sortOf formal'] [sortOf actual] of
        -- if a qualifier can't be fully instantiated, it should just disregard the qualifier
        -- Left _ -> trace "oh god why" mzero
        Left _ -> trace "why doesn't this fix it" $ return (sortSubst, subst, delete actual actuals)
        Right sortSubst' -> return (sortSubst `Map.union` sortSubst', Map.insert (varName formal) actual subst, delete actual actuals)

-- | Resolves sorts for a given qualifier, returns the resolved qualifier formula
resolveInputSorts :: InputExpr -> InputExpr
resolveInputSorts (Qualifier n xs f) = Qualifier n xs $ resolveSorts xs f
resolveInputSorts (HornConstraint xs f) = HornConstraint xs $ resolveSorts xs f
resolveInputSorts i = i

-- | Updates the sorts in a formula using the given variables
resolveSorts :: [Formula] -> Formula -> Formula
resolveSorts xs (Var s n)
  | s == AnyS   = Var s' n
  | otherwise   = error "qualifier already contains sorts (this shouldn't happen)"
  where
    s' = varSort xs n
    varSort :: [Formula] -> Id -> Sort
    varSort ((Var s n):xs) x
      | n == x      = s
      | otherwise   = varSort xs x
    varSort _ x     = error $ "no sort found for " ++ x ++ " in qualifier (variable not declared)"
resolveSorts xs (Unary op f)      = Unary op $ resolveSorts xs f
resolveSorts xs (Binary op f1 f2) = Binary op f1' f2'
  where
    f1' = resolveSorts xs f1
    f2' = resolveSorts xs f2
resolveSorts xs (Ite f1 f2 f3)    = Ite f1' f2' f3'
  where
    f1' = resolveSorts xs f1
    f2' = resolveSorts xs f2
    f3' = resolveSorts xs f3
resolveSorts _ f = f


{-
type HornSolver = FixPointSolver Z3State

-- | Solves for the least fixpoint(s) and greatest fixpoint of a constraint
-- with the given qualifiers
musfixpoint :: Id -> Sort -> Formula -> IO ()
musfixpoint var sort fml = print "resolvedTypes eventually..."
  where
    resolvedTypes = resolveTypeRefinement sort fml

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

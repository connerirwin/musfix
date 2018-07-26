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
  WFConstraint "$k0" [Var IntS "v0"],
  HornConstraint (All (Var IntS "v1") (Binary Implies (Pred AnyS "$k0" []) (Binary Lt (IntLit 0) (Binary Plus (Var AnyS "v1") (IntLit 1))))),
  HornConstraint (All (Var IntS "v2") (Binary Implies (Binary Eq (Var AnyS "v2") (IntLit 10)) (Pred AnyS "$k0" [])))
  ]

qmap = generateQualifiers $ resolveSorts input

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
        subbedQualifiers = concat $ map substituteQualifier rawQualifiers
        actuals = wfFormals wfconstraint

        -- | The actuals being substituted into the formal parameters of the
        -- qualifier are the formal parameters of the wfConstraint
        substituteQualifier :: Formula -> [Formula]
        substituteQualifier qualifier = map (flip substitute qualifier) substitutions
          where
            formals = extractVars qualifier
            substitutions = generateSubstitutions formals actuals

generateSubstitutions :: [Formula] -> [Formula] -> [Substitution]
generateSubstitutions formals actuals = if length singleMappings /= length formals then [] else validMappings -- ^ unnecessary optimization?
  where
    singleMappings = groupBy keysMatch $ [[(fName, a)] | f@(Var _ fName) <- formals, a <- actuals, sameSort f a]
    fullMappings = map Map.fromList $ foldAp (++) [[]] singleMappings
    validMappings = filter (isSet . Map.elems) fullMappings -- TODO should they be one-to-one

    sameSort :: Formula -> Formula -> Bool
    sameSort (Var s1 _) (Var s2 _) = s1 == s2
    sameSort _          _          = False

    keysMatch :: Eq a => [(a, b)] -> [(a, c)] -> Bool
    keysMatch [(x, _)] [(y, _)] = x == y

    foldAp :: Applicative f => (a -> b -> a) -> f a -> [f b] -> f a
    foldAp f acc [] = acc
    foldAp f acc (x:xs) = foldAp f (f <$> acc <*> x) xs

    isSet :: Eq a => [a] -> Bool
    isSet a = nub a == a

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

-- | Resolves the sorts of all given inputs
resolveSorts :: [InputExpr] -> [InputExpr]
resolveSorts ins = map f ins
  where
    f (Qualifier n xs f) = resolveQualifierSorts $ Qualifier n xs f
    f a = a

-- | Resolves sorts for a given qualifier, returns the resolved qualifier formula
resolveQualifierSorts :: InputExpr -> InputExpr
resolveQualifierSorts (Qualifier n xs f) = Qualifier n xs $ updateSort xs f
  where
    updateSort :: [Formula] -> Formula -> Formula
    updateSort xs (Var s n)
      | s == AnyS   = Var s' n
      | otherwise   = error "qualifier already contains sorts (this shouldn't happen)"
      where
        s' = varSort xs n
    updateSort xs (Unary op f)      = Unary op $ updateSort xs f
    updateSort xs (Binary op f1 f2) = Binary op f1' f2'
      where
        f1' = updateSort xs f1
        f2' = updateSort xs f2
    updateSort xs (Ite f1 f2 f3)    = Ite f1' f2' f3'
      where
        f1' = updateSort xs f1
        f2' = updateSort xs f2
        f3' = updateSort xs f3
    updateSort _ f = f
    varSort :: [Formula] -> Id -> Sort
    varSort ((Var s n):xs) x
      | n == x      = s
      | otherwise   = varSort xs x
    varSort _ x     = error $ "no sort found for " ++ x ++ " in qualifier (variable not declared)"
resolveQualifierSorts _ = error "cannot resolve qualifier sorts: input expression is different type"

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

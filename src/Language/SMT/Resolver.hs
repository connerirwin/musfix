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

{- Debug Testing -}
debugOut a = traceShow a a

-- | If the qualifiers reuse variable names, they must be the same type
pos   = Binary Le (IntLit 0) (Var IntS "v")
neg   = Binary Le (Var IntS "v") (IntLit 0)
neqZ  = Unary Not (Binary Eq (Var IntS "v") (IntLit 0))
false = Binary Eq (IntLit 66) (IntLit 77)
func  = WFConstraint "$k0" [(Var IntS "v0")]

qmap = generateQualifiers [func] [pos, neg, neqZ, false]

-- | A debug printing function designed to be as unobtrusive as possible
resolverDebug :: IO ()
resolverDebug = do
  print qmap

functionName :: InputExpr -> Id
functionName (WFConstraint name _) = name

functionFormals :: InputExpr -> [Formula]
functionFormals (WFConstraint _ formals) = formals

-- | Create a qualifier (refinement) map from a set of possible qualifiers for a
-- given unknown
generateQualifiers :: [InputExpr] -> [Formula] -> QMap
generateQualifiers wfconstraints rawQualifiers = Map.fromList $ qualifiers
  where
    qualifiers = zip (map functionName wfconstraints) (map substituteQualifiers wfconstraints)

    -- | Substitute the wfconstraint formal parameters into the qualifiers
    substituteQualifiers :: InputExpr -> QSpace
    substituteQualifiers formalsMap = toSpace Nothing subbedQualifiers
      where
        subbedQualifiers = foldr (union . \qualif -> allSubstitutions env qualif formals actuals) [] rawQualifiers
        formals = foldr (union . extractVars) [] rawQualifiers  -- vars in the rawQualifiers
        actuals = functionFormals formalsMap -- vars in the wfconstraint
        -- TODO figure out what the environment is used for, populate enough so that it will work ('^')
        env = emptyEnv {
          _boundTypeVars = nub $ map extractId formals `union` map extractId actuals
        }

unifyWith f = foldr (union . f) []
unify = unifyWith id

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

extractId :: Formula -> Id
extractId (Var _ x) = x

-- | 'allSubstitutions' @env qual formals actuals@:
-- all well-typed substitutions of @actuals@ for @formals@ in a qualifier @qual@
allSubstitutions :: Environment -> Formula -> [Formula] -> [Formula] -> [Formula]
allSubstitutions env qual formals actuals = do
  qual' <- allRawSubstitutions env qual formals actuals
  return qual'
  -- case resolveRefinement env (debugOut qual') of
  --   Left _ -> [Var AnyS "variable sort mismatch"] -- Variable sort mismatch
  --   Right resolved -> return resolved

allRawSubstitutions :: Environment -> Formula -> [Formula] -> [Formula] -> [Formula]
allRawSubstitutions _ (BoolLit True) _ _ = []
allRawSubstitutions env qual formals actuals = do
  let tvs = Set.fromList (env ^. boundTypeVars)
  (sortSubst, subst, _) <- foldM (go tvs) (Map.empty, Map.empty, actuals) formals
  return $ substitute subst $ sortSubstituteFml sortSubst qual
  where
    go tvs (sortSubst, subst, actuals) formal = do
      let formal' = sortSubstituteFml sortSubst formal
      actual <- actuals
      case unifySorts tvs [sortOf formal'] [sortOf actual] of
        Left _ -> mzero
        Right sortSubst' -> return (sortSubst `Map.union` sortSubst', Map.insert (varName formal) actual subst, delete actual actuals)

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
data Environment = Environment {
  -- | Variable part:
  _symbols :: Map Int (Map Id RSchema),    -- ^ Variables and constants (with their refinement types), indexed by arity
  _boundTypeVars :: [Id],                  -- ^ Bound type variables
  _boundPredicates :: [PredSig],           -- ^ Argument sorts of bound abstract refinements
  _assumptions :: Set Formula,             -- ^ Unknown assumptions
  _shapeConstraints :: Map Id SType,       -- ^ For polymorphic recursive calls, the shape their types must have
  _usedScrutinees :: [RProgram],           -- ^ Program terms that has already been scrutinized
  _unfoldedVars :: Set Id,                 -- ^ In eager match mode, datatype variables that can be scrutinized
  _letBound :: Set Id,                     -- ^ Subset of symbols that are let-bound
  -- | Constant part:
  _constants :: Set Id,                    -- ^ Subset of symbols that are constants
  _datatypes :: Map Id DatatypeDef,        -- ^ Datatype definitions
  _globalPredicates :: Map Id [Sort],      -- ^ Signatures (resSort:argSorts) of module-level logic functions (measures, predicates)
  _measures :: Map Id MeasureDef,          -- ^ Measure definitions
  _typeSynonyms :: Map Id ([Id], RType),   -- ^ Type synonym definitions
  _unresolvedConstants :: Map Id RSchema   -- ^ Unresolved types of components (used for reporting specifications with macros)
} deriving (Show)

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

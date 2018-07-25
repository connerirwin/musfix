module Language.SMT.Resolver where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Lens
import Control.Monad
import Control.Monad.State

import Language.SMT.HornSolver
import Language.SMT.Logic
import Language.SMT.Program
import Language.SMT.Util
import Language.SMT.Z3
import Language.SMT.ResolverSynq

{-
pos   = Binary Le (IntLit 0) (Var IntS "v")
neg   = Binary Le (Var IntS "v") (IntLit 0)
neqZ  = Unary Not (Binary Eq (Var BoolS "v") (IntLit 0))
false = Binary Eq (IntLit 66) (IntLit 77)
func  = Unknown [(Var IntS "v0"), (Var IntS "v1")] "$k0"

qmap = generateQualifiers func [pos, neg, neqZ, false]
-}

-- | Hopefully the InputExpr can look like this
data InputExpr = WFConstraint Id [Formula]

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
        -- TODO figure out what the environment is used for
        subbedQualifiers = foldr (union . \qualif -> allSubstitutions emptyEnv qualif formals actuals) [] rawQualifiers
        formals = foldr (union . extractVars) [] rawQualifiers  -- vars in the rawQualifiers ALMOST THERE.... ('^')
        actuals = functionFormals formalsMap -- vars in the wfconstraint

unify = foldl union []

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
-- Generate a list of all substitutions of qualifiers with the actuals, this will be put into the qmap
allSubstitutions :: Environment -> Formula -> [Formula] -> [Formula] -> [Formula]
allSubstitutions env qual formals actuals = do
  qual' <- allRawSubstitutions env qual formals actuals [] [] -- TODO cleanly remove the fixed formals and actuals from these
  case resolveRefinement env qual' of
    Left _ -> [] -- Variable sort mismatch
    Right resolved -> return resolved

allRawSubstitutions :: Environment -> Formula -> [Formula] -> [Formula] -> [Formula] -> [Formula] -> [Formula]
allRawSubstitutions _ (BoolLit True) _ _ _ _ = []
allRawSubstitutions env qual formals actuals fixedFormals fixedActuals = do
  let tvs = Set.fromList (env ^. boundTypeVars)
  case unifySorts tvs (map sortOf fixedFormals) (map sortOf fixedActuals) of
    Left _ -> []
    Right fixedSortSubst -> do
      let fixedSubst = Map.fromList $ zip (map varName fixedFormals) fixedActuals
      let qual' = substitute fixedSubst qual
      (sortSubst, subst, _) <- foldM (go tvs) (fixedSortSubst, Map.empty, actuals) formals
      return $ substitute subst $ sortSubstituteFml sortSubst qual'
  where
    go tvs (sortSubst, subst, actuals) formal = do
      let formal' = sortSubstituteFml sortSubst formal
      actual <- actuals
      case unifySorts tvs [sortOf formal'] [sortOf actual] of
        Left _ -> mzero
        Right sortSubst' -> return (sortSubst `Map.union` sortSubst', Map.insert (varName formal) actual subst, delete actual actuals)

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

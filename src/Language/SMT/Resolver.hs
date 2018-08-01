module Language.SMT.Resolver where

import Language.SMT.Syntax

import Data.List
import Data.Map (Map, (!))
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

{- Util -}
debugOut a = traceShow a a

unifyWith f = foldr (union . f) []
unify = unifyWith id

{- Debug Testing -}


-- | A debug printing function designed to be as unobtrusive as possible
resolverDebug :: IO ()
resolverDebug = do
  print "nothing for now"

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
generateSubstitutions formals actuals = if length singleMappings /= length formals then [] else validMappings -- ^ rejects incomplete qualifier mappings
  where
    singleMappings = groupBy keysMatch $ [[(fName, a)] | f@(Var _ fName) <- formals, a <- actuals, sameSort f a]
    fullMappings = map Map.fromList $ foldAp (++) [[]] singleMappings
    -- TODO convert to map at end, make a set at beginning
    validMappings = filter (isSet . Map.elems) fullMappings

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

-- TODO replace with sets, convert to list at end
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

-- | Resolve
prepareInputs :: [InputExpr] -> [InputExpr]
prepareInputs ins = resolveUnknownParameters $ map resolveInputSorts ins

-- | Resolves parameter substitutions for unknowns
resolveUnknownParameters :: [InputExpr] -> [InputExpr]
resolveUnknownParameters ins = map update ins
  where
    allFormals :: Map Id [Formula]
    allFormals = Map.fromList $ map varMap (allWFConstraints ins)

    varMap :: InputExpr -> (Id, [Formula])
    varMap (WFConstraint k fmls) = (k, fmls)

    update :: InputExpr -> InputExpr
    update (HornConstraint xs f) = HornConstraint xs $ updateSubs allFormals f
    update a = a

    updateSubs :: Map Id [Formula] -> Formula -> Formula
    updateSubs formalMap (Unknown sub name) = Unknown sub' name
      where
        sub' = Map.fromList $ renameVar 0 sub (formalMap ! name)

        -- | Takes an accumulator, call-site substitution map, and a list of formals, then outputs pairs of new variable names and their variable objects
        renameVar :: Int -> Map Id Formula -> [Formula] -> [(Id, Formula)]
        renameVar n s ((Var fmlSort fmlName):xs) = (fmlName, Var fmlSort actlName):(renameVar (n + 1) s xs)
          where
            (Var actlSort actlName) = s ! ("a" ++ (show n))
        renameVar _ _ [] = []

    updateSubs m (Unary op f)      = Unary op $ updateSubs m f
    updateSubs m (Binary op f1 f2) = Binary op f1' f2'
      where
        f1' = updateSubs m f1
        f2' = updateSubs m f2
    updateSubs m (Ite f1 f2 f3)    = Ite f1' f2' f3'
      where
        f1' = updateSubs m f1
        f2' = updateSubs m f2
        f3' = updateSubs m f3
    updateSubs _ f = f


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
resolveSorts xs (Pred s n args) = Pred s n $ map (resolveSorts xs) args
resolveSorts _ f = f

-- | Gets the sort of a var from a list of vars
varSort :: [Formula] -> Id -> Sort
varSort ((Var s n):xs) x
    | n == x      = s
    | otherwise   = varSort xs x
varSort _ x     = error $ "no sort found for " ++ x ++ " in qualifier (variable not declared)"


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

module Language.SMT.Resolver where

import Language.SMT.Syntax

import Data.List
import qualified Data.Map as Map
import Data.Map (Map, (!))
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

{- Debug Testing -}
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
            formals = Set.toList $ varsOf qualifier
            substitutions = generateSubstitutions formals actuals

generateSubstitutions :: [Formula] -> [Formula] -> [Substitution]
generateSubstitutions formals actuals = if length singleMappings /= length formals then [] else validMappings -- ^ rejects incomplete qualifier mappings
  where
    -- | Generate all individual mappings that type check
    singleMappings = groupBy keysMatch $ [[(fName, a)] | f@(Var _ fName) <- formals, a <- actuals, sameSort f a]
    -- | Create all complete combinations of variable mappings
    fullMappings = map Map.fromList $ foldAp (++) [[]] singleMappings
    -- | ensures that all mappings are one-to-one
    validMappings = filter (isSet . Map.elems) fullMappings

    sameSort :: Formula -> Formula -> Bool
    -- | TODO This is a hack that allows for polymorphic qualifiers in the worst
    -- way possible. This should actually use a map
    sameSort (Var s1 _) (Var s2 _) = s1 == s2 || s1 == AnyS || s2 == AnyS
    sameSort _          _          = False

    keysMatch :: Eq a => [(a, b)] -> [(a, c)] -> Bool
    keysMatch [(x, _)] [(y, _)] = x == y

    foldAp :: Applicative f => (a -> b -> a) -> f a -> [f b] -> f a
    foldAp f acc [] = acc
    foldAp f acc (x:xs) = foldAp f (f <$> acc <*> x) xs

    isSet :: Eq a => [a] -> Bool
    isSet a = nub a == a

-- | Resolve
-- TODO add error checking if passed in types are wrong
-- enforceSame

prepareInputs :: [InputExpr] -> [InputExpr]
prepareInputs ins = map update ins
  where
    -- | These maps are of the totally raw input. In the future, they might break
    -- if updates vars actually cleans them. Replace calls to ins with sortResolvedIns.
    -- updateSorts :: InputExpr -> InputExpr
    -- updateSorts (Qualifier n xs f)    = Qualifier n xs f'
    --   where
    --     f' = mapFormula (updateVar xs) f
    -- updateSorts (HornConstraint xs f) = HornConstraint xs f'
    --   where
    --     f' = mapFormula (updatePred . updateUnknown . (updateVar xs)) f
    -- updateSorts a = a
    --
    -- sortResolvedIns = map updateSorts ins
    varMap :: Map Id [Formula]
    varMap = Map.fromList $ map boxWF $ allWFConstraints ins
      where
        boxWF :: InputExpr -> (Id, [Formula])
        boxWF (WFConstraint k fmls) = (k, fmls)

    sortMap :: Map Id Sort
    sortMap = Map.fromList $ map boxUf $ allUninterpFunction ins
      where
        boxUf :: InputExpr -> (Id, Sort)
        boxUf (UninterpFunction name formals result) = (name, result)

    -- | Target specific input expressions for updates
    update :: InputExpr -> InputExpr
    update (Qualifier n xs f)    = Qualifier n xs f'
      where
        f' = mapFormula (updateVar xs) f
    update (HornConstraint xs f) = HornConstraint xs f'
      where
        f' = mapFormula (updatePred . updateUnknown . (updateVar xs)) f
    update a = a

    -- | Updates the sorts in a formula using the given variables
    updateVar :: [Formula] -> Formula -> Formula
    updateVar xs (Var s n)
      | s == AnyS   = Var s' n
      | otherwise   = error "qualifier already contains sorts (this shouldn't happen)"
      where
        s' = varSort xs n
    updateVar _ a = a

    -- | Resolves parameter substitutions for unknowns
    updateUnknown :: Formula -> Formula
    updateUnknown (Unknown sub name) = Unknown sub' name
      where
        sub' = Map.fromList $ renameVar 0 sub (varMap ! name)

        -- | Takes an accumulator, call-site substitution map, and a list of formals, then outputs pairs of new variable names and their variable objects
        renameVar :: Int -> Map Id Formula -> [Formula] -> [(Id, Formula)]
        renameVar n s ((Var fmlSort fmlName):xs) = (fmlName, Var fmlSort actlName):(renameVar (n + 1) s xs)
          where
            (Var actlSort actlName) = s ! ("a" ++ (show n))
        renameVar _ _ [] = []
    updateUnknown a = a

    -- TODO look into Map.mapKeys
    -- | Takes a call-site substitution map and a list of formals
    -- renameVar :: Map Id Formula -> [Formula] -> [(Id, Formula)]
    -- renameVar m xs = zip fmlNames substitutions
    --   where
    --     substitutions = zipWith (\s n -> Var s n) fmlSorts actlNames
    --     fmlSorts = map varType xs
    --     fmlNames = map varName xs
    --     actlNames = map (varName . (m !)) keys
    --     keys = map (("a" ++) . show) [0..]

    -- | Substitute in actual types for uninterpreted functions
    updatePred :: Formula -> Formula
    updatePred (Pred s p fs) = Pred s' p fs
      where s' = sortMap ! p
    updatePred a = a

-- | Gets the sort of a var from a list of vars
varSort :: [Formula] -> Id -> Sort
varSort ((Var s n):xs) x
    | n == x      = s
    | otherwise   = varSort xs x
varSort _ x     = error $ "no sort found for " ++ x ++ " in qualifier (variable not declared)"

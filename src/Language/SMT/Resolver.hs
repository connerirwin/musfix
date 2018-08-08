module Language.SMT.Resolver (
  generateQualifiers,
  prepareInputs,
  resolverDebug,
) where

import Language.SMT.Syntax

import Language.Synquid.HornSolver
import Language.Synquid.Logic
import Language.Synquid.Program
import Language.Synquid.Util
import Language.Synquid.Z3

import Control.Lens
import Control.Monad
import Control.Monad.State

import Data.List
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.Set as Set

import Debug.Trace

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

prepareInputs :: [InputExpr] -> [InputExpr]
prepareInputs ins = (resolveSorts . preprocessInput) ins

-- |
preprocessInput :: [InputExpr] -> [InputExpr]
preprocessInput ins = map targetUpdate ins
  where
    varMap :: Map Id [Formula]
    varMap = Map.fromList $ map boxWF $ allWFConstraints ins
      where
        boxWF :: InputExpr -> (Id, [Formula])
        boxWF (WFConstraint k fmls) = (k, fmls)

    predSortMap :: Map Id Sort
    predSortMap = Map.fromList $ map boxUf $ allUninterpFunction ins
      where
        boxUf :: InputExpr -> (Id, Sort)
        boxUf (UninterpFunction name formals result) = (name, result)

    -- | Target specific input expressions for updates
    targetUpdate :: InputExpr -> InputExpr
    targetUpdate (Qualifier n xs f)    = Qualifier n xs f'
      where
        f' = mapFormula (distributeSort m) f
        m  = formalSortMap xs
    targetUpdate (HornConstraint xs f) = HornConstraint xs f'
      where
        f' = mapFormula (updatePred . updateUnknown . (distributeSort m)) f
        m = formalSortMap xs
    targetUpdate a = a

    formalSortMap :: [Formula] -> Map Id Sort
    formalSortMap formals = Map.fromList $ map boxVar formals
      where
        boxVar :: Formula -> (Id, Sort)
        boxVar (Var sort name) = (name, sort)

    -- | Applies the sort of formal variables to their actual occurances
    distributeSort :: Map Id Sort -> Formula -> Formula
    distributeSort m (Var s n)
      | s == AnyS   = Var s' n
      | otherwise   = error "qualifier already contains sorts (this shouldn't happen)"
      where
        s' = case Map.lookup n m of
          Nothing -> error $ "no sort found for " ++ n ++ " in qualifier (variable not declared)"
          Just sort -> sort
    distributeSort _ a = a

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
      where s' = predSortMap ! p
    updatePred a = a

-- | make sure that the sorts of arguments match expressions
-- make sure that sorts of binops eq are the same
-- For the most part, this should only perform checking, not substitution
-- However, for map literals and set literals, it should perform the substitution
-- Goal, output all sort errors instead of stopping at the first one
resolveSorts :: [InputExpr] -> [InputExpr]
resolveSorts ins = map targetUpdate ins
  where
    targetUpdate :: InputExpr -> InputExpr
    targetUpdate (Qualifier name vars eq) = Qualifier name vars eq'
      where
        eq' = resolveSorts' vars eq
    targetUpdate (HornConstraint vars eq) = HornConstraint vars eq'
      where
        eq' = resolveSorts' vars eq
    targetUpdate a = a

    -- | Resolve the sorts of
    resolveSorts' :: [Formula] -> Formula -> Formula
    resolveSorts' vars eq = mapFormula checkOp eq

    -- | This is currently really weird. Basically, it will perform a check, and
    -- if it passes, continue evaluation. Otherwise it spits out an error message.
    -- There has to be a better way of doing this.
    -- TODO This should use the writer monad?
    checkOp :: Formula -> Formula
    checkOp u@(Unary op f) = applySort sort u
      where
        (formalSort, _) = unOpSort op
        argSort = sortOf f
        sort = case unifySortsM formalSort argSort of
          Nothing -> error $ "Sort mismatch:  Unary op " ++ show op ++ " expected an input of sort " ++ show formalSort ++ ", but received " ++ show argSort ++ " in expression:  " ++ show u
          Just s  -> s
    -- checkOp (Binary op f1 f2) =
    checkOp a = a

    -- checkOp :: Formula -> Maybe Formula
    -- checkOp u@(Unary op f) = do
    --     let (formalSort, _) = unOpSort op
    --     let argSort = sortOf f
    --     f' <- unifySortsM formalSort argSort >>= flip applySort f
    --     when (isNothing f') $ error $ "Sort mismatch:  Unary op " ++ show op ++ " expected an input of sort " ++ show formalSort ++ ", but received " ++ show argSort ++ " in expression:  " ++ show u
    --     return $ Unary op f'
    -- checkOp b@(Binary op f1 f2) = do
    --     let (formalSort1, formalSort2, _) = binOpSort op
    --     let argSort1 = sortOf f1
    --     let argSort2 = sortOf f2
    --     f1' <- unifySortsM formalSort1 argSort1 >>= flip applySort f1
    --     f2' <- unifySortsM formalSort2 argSort2 >>= flip applySort f2
    --     when (isNothing f1' || isNothing f2') $ error $ "Sort mismatch:  Binary op " ++ show op ++ " expected inputs of sort " ++ show (formalSort1, formalSort2) ++ ", but recived " ++ show (argSort1, argSort2) ++ " in expression:  " ++ show b
    --     return $ Binary op f1' f2'
    -- checkOp a = a

-- If possible, replace the sorts in the formula so that it evaluates to having sort s
applySort :: Sort -> Formula -> Formula
applySort AnyS f = f
applySort s f = f

-- | Unifies the sorts of a and b if possible, otherwise fails
unifySortsM :: Sort -> Sort -> Maybe Sort
unifySortsM a b
  | a == b           = pure a
unifySortsM (DataS n1 args1) (DataS n2 args2)
  | n1 == n2   = do
      args' <- zipWithM unifySortsM args1 args2
      return $ DataS n1 args'
unifySortsM (SetS e1) (SetS e2) = do
      e' <- unifySortsM e1 e2
      return $ SetS e'
unifySortsM (MapS k1 v1) (MapS k2 v2) = do
      k' <- unifySortsM k1 k2
      v' <- unifySortsM v1 v2
      return $ MapS k' v'
unifySortsM a    AnyS = pure a
unifySortsM AnyS b    = pure b
unifySortsM _    _    = fail "sort mismatch"

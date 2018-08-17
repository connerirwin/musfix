module Language.SMT.Resolver (
  generateQualifiers,
  prepareInputs,
  resolverDebug,
) where

import Language.SMT.Syntax

import Language.Synquid.HornSolver
import Language.Synquid.Logic hiding (unifySorts)
import Language.Synquid.Program hiding (Environment)
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
debugOutMsg m a = trace (m ++ show a) a

data FunctionApplication = FunctionApplication {
  funcName   :: String,
  signature  :: [Sort],
  arguments  :: [Formula],
  expression :: Formula
}

data Environment = Environment {
  wfMap   :: Map Id [Formula],
  predMap :: Map Id [Sort]
}

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

-- | TODO I bet that it is possible to combine these into a single pass.
-- What if we switch from Maybe to either? something like this:
-- resolveRefinement :: Environment -> Formula -> Either ErrorMessage Formula
prepareInputs :: [InputExpr] -> [InputExpr]
prepareInputs ins = resolve ins
  where
    env = createEnvironment ins
    resolve = resolveSorts env

-- | TODO make this a single pass
createEnvironment :: [InputExpr] -> Environment
createEnvironment ins = env
  where
    boxWF :: InputExpr -> (Id, [Formula])
    boxWF (WFConstraint k formals) = (k, formals)

    boxUf :: InputExpr -> (Id, [Sort])
    boxUf (UninterpFunction name formals result) = (name, formals ++ [result])

    env = Environment {
      wfMap   = Map.fromList $ map boxWF $ allWFConstraints ins,
      predMap = Map.fromList $ map boxUf $ allUninterpFunction ins
    }

-- | make sure that the sorts of arguments match expressions
-- make sure that sorts of binops eq are the same
-- For the most part, this should only perform checking, not substitution
-- However, for map literals and set literals, it should perform the substitution
-- Goal, output all sort errors instead of stopping at the first one
resolveSorts :: Environment -> [InputExpr] -> [InputExpr]
resolveSorts env ins = map targetUpdate ins
  where
    targetUpdate :: InputExpr -> InputExpr
    targetUpdate (Qualifier name vars eq) = Qualifier name vars $ resolveSorts' vars eq
    targetUpdate (HornConstraint vars eq) = HornConstraint vars $ resolveSorts' vars eq
    targetUpdate a = a

    resolveSorts' :: [Formula] -> Formula -> Formula
    resolveSorts' vars eq = mapFormula (resolve vars) eq

    -- | Checks sorts, resolving them if possible
    resolve :: [Formula] -> Formula -> Formula
    resolve _  b@(BoolLit bool)      = b
    resolve _  i@(IntLit val)        = i
    resolve _  s@(SetLit sort elems) = resolveSet s -- ^ check sort of elems
    resolve _  m@(MapLit ksort val)  = m
    resolve _  ms@(MapSel m k)       = ms
    resolve _  mu@(MapUpd m k v)     = mu
    resolve vs v@(Var sort name)     = lookupVar v vs
    resolve _  wf@(Unknown sub name) = resolveUnknown wf
    resolve _  u@(Unary op f)        = resolveAp u
    resolve _  b@(Binary op f1 f2)   = resolveAp b
    resolve _  ite@(Ite i t e)       = ite
    resolve _  f@(Func sort name fs) = lookupFunc $ resolveAp f
    resolve _  c@(Cons sort name fs) = c
    resolve _  a@(All f1 f2)         = a

    {- The more complicated resolving has been factored out -}
    lookupVar :: Formula -> [Formula] -> Formula
    lookupVar (Var sort name) vs
      | sort == AnyS  = Var sort' name
      | otherwise     = error "qualifier already contains sorts (this shouldn't happen)"
      where
        sort' = case Map.lookup name (formalSortMap vs) of
          Nothing -> error $ "no sort found for " ++ name ++ " in qualifier (variable not declared)"
          Just sort -> sort

        formalSortMap :: [Formula] -> Map Id Sort
        formalSortMap formals = Map.fromList $ map boxVar formals
          where
            boxVar :: Formula -> (Id, Sort)
            boxVar (Var sort name) = (name, sort)

    -- | TODO make this check the sorts of the elements against eachother, so that the applied sort is continually updated
    resolveSet :: Formula -> Formula
    resolveSet (SetLit sort elems) = SetLit sort elems'
      where
        elems' = map (applySort sort) elems

    -- | Replaces the key with the correct parameter, updates the variable sorts
    resolveUnknown :: Formula -> Formula
    resolveUnknown (Unknown sub name) = Unknown sub' name
      where
        sub' = Map.fromList $ renameVar 0 sub $ (wfMap env) ! name
        -- | Takes an accumulator, call-site substitution map, and a list of formals, then outputs pairs of new variable names and their variable objects
        -- TODO switch to unifying the Sort of the vars
        renameVar :: Int -> Map Id Formula -> [Formula] -> [(Id, Formula)]
        renameVar n s ((Var fmlSort fmlName):xs) = (fmlName, Var fmlSort actlName):(renameVar (n + 1) s xs)
          where
            (Var actlSort actlName) = s ! ("a" ++ (show n))
        renameVar _ _ [] = []

    -- | Checks the sorts of the formals
    resolveAp :: Formula -> Formula
    resolveAp u@(Unary op f)        = Unary op f'
      where
        ap = FunctionApplication (show op) (init $ unOpSort op) [f] u
        (FunctionApplication _ _ [f'] _) = checkAp ap
    resolveAp b@(Binary op f1 f2)   = Binary op f1' f2'
      where
        ap = FunctionApplication (show op) (init $ binOpSort op) [f1, f2] b
        (FunctionApplication _ _ [f1', f2'] _) = checkAp ap
    resolveAp f@(Func sort name fs) = Func sort name fs'
      where
        ap = FunctionApplication name (init $ (predMap env) ! name) fs f
        (FunctionApplication _ _ fs' _) = checkAp ap

    -- | Lookup the sort of an uninterpreted func
    lookupFunc :: Formula -> Formula
    lookupFunc (Func _ name fs) = Func sort name fs
      where
        sort = last $ (predMap env) ! name

-- | TODO add support for polymorphic sort unification
-- basically, some of the arguments might need to have their sorts unified with eachother, not the signature
checkAp :: FunctionApplication -> FunctionApplication
checkAp ap = case checkApM ap of
    Nothing -> error $ "Sort mismatch:  " ++ name ++ " expects " ++ formalSorts ++ ", but received " ++ argSorts ++ " in expression:  " ++ expr
    Just a  -> a
  where
    name = funcName ap
    formalSorts = show $ signature ap
    argSorts = show $ map sortOf $ arguments ap
    expr = show $ expression ap

-- | Check var sorts against eachother
-- TODO add some sort of Environment to clean up the two pass polymorphic unification
checkApM :: FunctionApplication -> Maybe FunctionApplication
checkApM ap = do
    let formalSorts = signature ap
    let args = arguments ap
    partialFormals <- zipWithM applySortM formalSorts args

    let partialSorts = map sortOf partialFormals
    let unifiedSorts = unifyPolymorphic formalSorts partialSorts
    formals' <- zipWithM applySortM unifiedSorts args

    return $ ap { arguments = formals' }

unifyPolymorphic :: [Sort] -> [Sort] -> [Sort]
unifyPolymorphic fs ps = zipWith applyMap fs ps
  where
    buildMap :: Map Id Sort -> [Sort] -> [Sort] -> Map Id Sort
    buildMap m ((VarS name):fs) (p:ps) = buildMap (Map.insertWith unifySorts name p m) fs ps
    buildMap m (f:fs) (p:ps) = buildMap m fs ps
    buildMap m [] [] = m

    polymorphicMap = buildMap Map.empty fs ps

    applyMap :: Sort -> Sort -> Sort
    applyMap (VarS name) p = polymorphicMap ! name
    applyMap _ p           = p

-- Error reporting
applySort :: Sort -> Formula -> Formula
applySort s f = case applySortM s f of
    Nothing -> error $ "Sort mismatch:  Cannot apply sort " ++ sort ++ " to " ++ formula
    Just a  -> a
  where
    sort = show s
    formula = show f

-- | Applies the sort to formula if possible, otherwise fails
applySortM :: Sort -> Formula -> Maybe Formula
applySortM s f = do
    let assumedSort = sortOf f
    unifiedSort <- unifySortsM s assumedSort
    pure $ applySort' unifiedSort f
  where
    -- | Replaces the sorts in the formula so that its new sort is s
    -- this cannot be called in a context where it can fail
    applySort' :: Sort -> Formula -> Formula
    applySort' AnyS f     = f
    applySort' (VarS _) f = f
    applySort' BoolS (BoolLit b)             = BoolLit b
    applySort' IntS (IntLit i)               = IntLit i
    applySort' (SetS s) (SetLit _ elems)     = SetLit s elems
    applySort' (MapS ksort _) (MapLit _ val) = MapLit ksort val
    applySort' s (Var _ name)                = Var s name
    applySort' s (Func _ name fs)            = Func s name fs
    applySort' s (Cons _ name fs)            = Cons s name fs
    applySort' _ f                           = f

-- | Error reporting
unifySorts :: Sort -> Sort -> Sort
unifySorts a b = case unifySortsM a b of
    Nothing -> error $ "Sort mismatch:  Cannot unify sorts " ++ show a ++ " and " ++ show b
    Just a  -> a

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
unifySortsM a AnyS     = pure a
unifySortsM AnyS b     = pure b
unifySortsM a (VarS _) = pure a
unifySortsM (VarS _) b = pure b
unifySortsM _ _        = fail "sort mismatch"

module Language.SMT.Resolver (
  generateQualifiers,
  prepareInputs,
) where

import qualified Language.SMT.MultiKeyMap as MultiKeyMap
import Language.SMT.MultiKeyMap (MultiKeyMap)
import Language.SMT.Syntax

import Language.Synquid.HornSolver
import Language.Synquid.Logic hiding (unifySorts)
import Language.Synquid.Program hiding (Environment)
import Language.Synquid.Util hiding (constMap)
import Language.Synquid.Z3

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
  wfMap    :: Map Id [Formula],
  predMap  :: Map Id [Sort],
  sortMap  :: Map Id Int,        -- Keeps track of the number of parameters that a new sort takes
  constMap :: Map Id Sort        -- Record the sorts of constants
}

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
    sameSort (Var s1 _) (Var s2 _) = s1 == s2 || isAnyPoly s1 || isAnyPoly s2
    sameSort _          _          = False

    keysMatch :: Eq a => [(a, b)] -> [(a, c)] -> Bool
    keysMatch [(x, _)] [(y, _)] = x == y

    foldAp :: Applicative f => (a -> b -> a) -> f a -> [f b] -> f a
    foldAp f acc [] = acc
    foldAp f acc (x:xs) = foldAp f (f <$> acc <*> x) xs

    isSet :: Eq a => [a] -> Bool
    isSet a = nub a == a

prepareInputs :: [InputExpr] -> [InputExpr]
prepareInputs ins = resolve ins
  where
    env = createEnvironment ins
    resolve = resolveSorts env

createEnvironment :: [InputExpr] -> Environment
createEnvironment ins = env
  where
    boxWF :: InputExpr -> (Id, [Formula])
    boxWF (WFConstraint k formals) = (k, formals)

    boxUf :: InputExpr -> (Id, [Sort])
    boxUf (UninterpFunction name formals result) = (name, formals ++ [result])

    boxSD :: InputExpr -> (Id, Int)
    boxSD (SortDecl name numSorts) = (name, numSorts)

    boxCD :: InputExpr -> (Id, Sort)
    boxCD (ConstantDecl name sort) = (name, sort)

    env = Environment {
      wfMap    = Map.fromList $ map boxWF $ allWFConstraints ins,
      predMap  = Map.fromList $ map boxUf $ allUninterpFunction ins,
      sortMap  = Map.fromList $ map boxSD $ allSortDecl ins,
      constMap = Map.fromList $ map boxCD $ allConstants ins
    }

-- | Resolves the sorts of expression, or throws an error
resolveSorts :: Environment -> [InputExpr] -> [InputExpr]
resolveSorts env ins = map targetUpdate ins
  where
    targetUpdate :: InputExpr -> InputExpr
    targetUpdate (Qualifier name vars eq) = Qualifier name vars $ resolveSorts' vars eq
    targetUpdate (HornConstraint vars eq) = HornConstraint vars $ resolveSorts' vars eq
    targetUpdate a = a

    resolveSorts' :: [Formula] -> Formula -> Formula
    resolveSorts' vars eq = mapFormula (checkDataS . resolve vars) eq

    -- | Checks sorts, resolving them if possible
    resolve :: [Formula] -> Formula -> Formula
    resolve _  b@(BoolLit bool)      = b
    resolve _  i@(IntLit val)        = i
    resolve _  s@(SetLit sort elems) = resolveSet s
    resolve _  m@(MapLit ksort val)  = m
    resolve _  ms@(MapSel m k)       = resolveAp ms
    resolve _  mu@(MapUpd m k v)     = resolveAp mu
    resolve vs v@(Var sort name)     = lookupVar v vs
    resolve _  wf@(Unknown sub name) = resolveUnknown wf
    resolve _  u@(Unary op f)        = resolveAp u
    resolve _  b@(Binary op f1 f2)   = resolveAp b
    resolve _  ite@(Ite i t e)       = ite
    resolve _  f@(Func sort name fs) = lookupFunc $ resolveAp f
    resolve _  c@(Cons sort name fs) = lookupCons c
    resolve _  a@(All f1 f2)         = a

    -- | Check all elements in a set literal against eachother
    resolveSet :: Formula -> Formula
    resolveSet s@(SetLit sort elems) = case resolveSet' s of
      Nothing -> error $ "This should never fail"
      Just  f -> f
      where
        resolveSet' :: Formula -> Maybe Formula
        resolveSet' (SetLit sort elems) = do
          -- | Unify, then apply generated map (this is two pass unification)
          (partialElems, polymap) <- runStateT (mapM (applySortM sort) elems) MultiKeyMap.empty
          let elems' = map (applyMap polymap sort) partialElems

          let sort' = if length elems' > 0 then sortOf $ head elems' else AnyS
          return $ SetLit sort' elems'

    -- | Replaces the key with the correct parameter, updates the variable sorts
    resolveUnknown :: Formula -> Formula
    resolveUnknown (Unknown sub name) = Unknown sub' name
      where
        sub' = Map.fromList $ renameVar 0 sub $ (wfMap env) ! name
        -- | Takes an accumulator, call-site substitution map, and a list of formals, then outputs pairs of new variable names and their variable objects
        renameVar :: Int -> Map Id Formula -> [Formula] -> [(Id, Formula)]
        renameVar n s ((Var fmlSort fmlName):xs) = (fmlName, Var fmlSort actlName):(renameVar (n + 1) s xs)
          where
            (Var actlSort actlName) = s ! ("a" ++ (show n))
        renameVar _ _ [] = []

    -- | Checks the sorts of the formals
    resolveAp :: Formula -> Formula
    resolveAp u@(Unary op f)        = Unary op f'
      where
        ap = FunctionApplication (show op) (getFormalSorts $ unOpSort op) [f] u
        (FunctionApplication _ _ [f'] _) = checkAp ap
    resolveAp b@(Binary op f1 f2)   = Binary op f1' f2'
      where
        ap = FunctionApplication (show op) (getFormalSorts $ binOpSort op) [f1, f2] b
        (FunctionApplication _ _ [f1', f2'] _) = checkAp ap
    resolveAp f@(Func sort name fs) = Func sort name fs'
      where
        ap = FunctionApplication name (getFormalSorts $ (predMap env) ! name) fs f
        (FunctionApplication _ _ fs' _) = checkAp ap
    resolveAp ms@(MapSel m k) = MapSel m k'
      where
        ap = FunctionApplication "Map_select" [keySort $ sortOf m] [k] ms
        (FunctionApplication _ _ [k'] _) = checkAp ap
    resolveAp mu@(MapUpd m k v) = MapUpd m k' v'
      where
        ap = FunctionApplication "Map_update" (kvSort $ sortOf m) [k, v] mu
        (FunctionApplication _ _ [k', v'] _) = checkAp ap

    -- | Lookup the variable sort from the formals
    lookupVar :: Formula -> [Formula] -> Formula
    lookupVar (Var sort name) vs
      -- | Constants cannot be distinguished from vars when parsing, so they must be converted
      -- TODO this is a hack that uses a no-arg Func to represent a constant
      | Map.member name $ constMap env = Func ((constMap env) ! name) name []
      | sort == AnyS  = Var sort' name
      | otherwise     = error "qualifier already contains sorts (this shouldn't happen)"
      where
        sort' = case Map.lookup name (formalSortMap vs) of
          Nothing -> error $ "no sort found for " ++ name ++ " in qualifier (variable not declared)"
          Just s  -> s

        formalSortMap :: [Formula] -> Map Id Sort
        formalSortMap formals = Map.fromList $ map boxVar formals
          where
            boxVar :: Formula -> (Id, Sort)
            boxVar (Var sort name) = (name, sort)

    -- | Lookup the sort of an uninterpreted func
    lookupFunc :: Formula -> Formula
    lookupFunc (Func _ name fs) = Func sort name fs
      where
        sort = getReturnSort $ (predMap env) ! name

    -- | Ensure that the correct number of formals are passed
    lookupCons :: Formula -> Formula
    lookupCons c@(Cons sort name fs) = Cons sort' name fs'
      where
        (Cons _ _ fs') = verifySortDecl c name fs
        -- ^ Currently disregards sort, what if somehow the sort what previously unified?
        sort' = DataS name $ map sortOf fs


    -- | Ensure that all constructed sorts are passed the correct number of arguments
    checkDataS :: Formula -> Formula
    checkDataS f = seq (mapSort checkDataS' $ sortOf f) f
      where
        checkDataS' :: Sort -> Sort
        checkDataS' s@(DataS name ss) = verifySortDecl s name ss
        checkDataS' s = s

    -- | Determine if the proper number of arguments are passed to a declared sort/cons
    -- if so, simply forward the passed in argument
    verifySortDecl :: Show a => a -> Id -> [a] -> a
    verifySortDecl a name args = if numArgs == expectedArgs then a
      else
        error $ "Argument mismatch: " ++ name ++ " expects " ++ show expectedArgs ++ " arguments, but instead received " ++ show numArgs ++ " in sort:  " ++ show a
      where
        expectedArgs = case Map.lookup name (sortMap env) of
          Nothing -> error $ "Sort " ++ name ++ " has not been declared"
          Just n  -> n
        numArgs = length args

type PolyMap = MultiKeyMap Id Sort

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
checkApM :: FunctionApplication -> Maybe FunctionApplication
checkApM ap = do
    let formalSorts = signature ap
    let args = arguments ap
    (partialFormals, polymap) <- runStateT (zipWithM applySortM formalSorts args) MultiKeyMap.empty

    let formals' = zipWith (applyMap polymap) formalSorts partialFormals
    return $ ap { arguments = formals' }

-- Error reporting
applySort :: Sort -> Formula -> Formula
applySort s f = case evalStateT (applySortM s f) MultiKeyMap.empty of
    Nothing -> error $ "Sort mismatch:  Cannot apply sort " ++ show s ++ " to " ++ show f
    Just a  -> a

-- | Applies the sort to formula if possible, otherwise fails
applySortM :: Sort -> Formula -> StateT PolyMap Maybe Formula
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
    applySort' (SetS s) (SetLit _ elems)     = SetLit s elems
    applySort' (MapS ksort _) (MapLit _ val) = MapLit ksort val
    applySort' s (Var _ name)                = Var s name
    applySort' s (Func _ name fs)            = Func s name fs
    applySort' s@(DataS name _) (Cons _ name' fs)
      | name == name'  = Cons s name fs
    applySort' _ f                           = f

updateMap :: [Id] -> Sort -> StateT PolyMap Maybe ()
updateMap names s = do
    m <- get
    let m' = MultiKeyMap.insertWith (unifySortsMap m) names s m
    put m'

unifySortsMap :: PolyMap -> Sort -> Sort -> Sort
unifySortsMap m a b = case evalStateT (unifySortsM a b) m of
    Nothing -> error $ "Sort mismatch:  Cannot unify sorts " ++ show a ++ " and " ++ show b
    Just s  -> s

-- | This needs to check the old sorts of the formals
-- | If any of the formals used to be VarS, this should update them
applyMap :: PolyMap -> Sort -> Formula -> Formula
applyMap m (VarS name) f = case MultiKeyMap.lookup name m of
    Nothing -> error $ "Polymap lookup failed: " ++ show name ++ " is not present in map " ++ show m
    Just s  -> applySort s f
applyMap m _ f
  | (VarS name) <- sortOf f = case MultiKeyMap.lookup name m of
    Nothing -> error $ "Polymap lookup failed: " ++ show name ++ " is not present in map " ++ show m
    Just s  -> applySort s f
applyMap _ _ f = f

-- | Error reporting
unifySorts :: Sort -> Sort -> Sort
unifySorts a b = case evalStateT (unifySortsM a b) MultiKeyMap.empty of
    Nothing -> error $ "Sort mismatch:  Cannot unify sorts " ++ show a ++ " and " ++ show b
    Just s  -> s

-- | Unifies the sorts of a and b if possible, otherwise fails
-- this should somehow use StateT (Map Id Sort)
unifySortsM :: Sort -> Sort -> StateT PolyMap Maybe Sort
unifySortsM a b
  | a == b    = pure a
unifySortsM (DataS n1 args1) (DataS n2 args2)
  | (n1 == n2) || (n1 == "_any") || (n2 == "_any") = do
      let name = if n1 == "_any" then n2 else n1
      args' <- zipWithM unifySortsM args1 args2
      return $ DataS name args'
unifySortsM (SetS e1) (SetS e2) = do
      e' <- unifySortsM e1 e2
      return $ SetS e'
unifySortsM (MapS k1 v1) (MapS k2 v2) = do
      k' <- unifySortsM k1 k2
      v' <- unifySortsM v1 v2
      return $ MapS k' v'
unifySortsM AnyS b  = pure b
unifySortsM a AnyS  = pure a
unifySortsM a@(VarS na) b@(VarS nb) = do
      updateMap [na, nb] AnyS
      pure a
unifySortsM (VarS name) b = do
      updateMap [name] b
      pure b
unifySortsM a (VarS name) = do
      updateMap [name] a
      pure a
unifySortsM _ _        = fail "sort mismatch"

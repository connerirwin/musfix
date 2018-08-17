{-# LANGUAGE OverloadedStrings #-}

module Language.SMT.Syntax where

import qualified Data.Map as Map
import Data.Map (Map)

-- | Identifiers
type Id = String

-- | Top-level input expressions
--
-- These expressions are fed into the program via a constraints file and produced by the parser.
--
data InputExpr =
    Qualifier Id [Formula] Formula          -- ^ Qualifier with name, variables and equation
  | WFConstraint Id [Formula]               -- ^ Well-formed predicate constraint
  | HornConstraint [Formula] Formula        -- ^ Horn constraint
  | UninterpFunction Id [Sort] Sort         -- ^ Uninterpreted function with input types and a return type (this is the way that z3 does it)
  deriving (Show, Eq, Ord)

isQualifier (Qualifier _ _ _) = True
isQualifier _ = False
isWFConstraint (WFConstraint _ _) = True
isWFConstraint _ = False
isHornConstraint (HornConstraint _ _) = True
isHornConstraint _ = False
isUninterpFunction (UninterpFunction _ _ _) = True
isUninterpFunction _ = False

-- | Gets all the qualifiers in an input expression list
allQualifiers :: [InputExpr] -> [InputExpr]
allQualifiers = filter isQualifier

-- | Gets all the well-formed constraints in an input expression list
allWFConstraints :: [InputExpr] -> [InputExpr]
allWFConstraints = filter isWFConstraint

-- | Gets all the horn constraints in an input expression list
allHornConstraints :: [InputExpr] -> [InputExpr]
allHornConstraints = filter isHornConstraint

allUninterpFunction :: [InputExpr] -> [InputExpr]
allUninterpFunction = filter isUninterpFunction

wfName :: InputExpr -> Id
wfName (WFConstraint name _) = name

wfFormals :: InputExpr -> [Formula]
wfFormals (WFConstraint _ formals) = formals

qualifEq :: InputExpr -> Formula
qualifEq (Qualifier _ _ eq) = eq

-- | Sorts
data Sort = BoolS | IntS | VarS Id | DataS Id [Sort] | SetS Sort | MapS Sort Sort | AnyS
  deriving (Show, Eq, Ord)

isSetS (SetS _) = True
isSetS _ = False
elemSort (SetS s) = s

isMapS (MapS _ _) = True
isMapS _ = False
keySort (MapS s _) = s
valueSort (MapS _ s) = s
isData (DataS _ _) = True
isData _ = False
sortArgsOf (DataS _ sArgs) = sArgs
varSortName (VarS name) = name

isAnyPoly (VarS _) = True
isAnyPoly AnyS     = True
isAnyPoly _        = False

-- | Unary operators
data UnOp =
    Neg |                          -- ^ Int -> Int
    Not                            -- ^ Bool -> Bool
  deriving (Show, Eq, Ord)

unOpSort :: UnOp -> [Sort]
unOpSort op = case op of
  Neg -> [IntS,   IntS]
  Not -> [BoolS, BoolS]

-- | Binary operators
data BinOp =
    Times | Plus | Minus |          -- ^ Int -> Int -> Int
    Eq | Neq |                      -- ^ a -> a -> Bool
    Lt | Le | Gt | Ge |             -- ^ Int -> Int -> Bool
    And | Or | Implies | Iff |      -- ^ Bool -> Bool -> Bool
    Union | Intersect | Diff |      -- ^ Set -> Set -> Set
    Member |                        -- ^ a -> Set -> Bool
    Subset                          -- ^ Set -> Set -> Bool
  deriving (Show, Eq, Ord)

binOpSort :: BinOp -> [Sort]
binOpSort op
  | op `elem` [Times, Plus, Minus]     = [IntS, IntS, IntS]
  | op `elem` [Eq, Neq]                = [VarS "a", VarS "a", BoolS]
  | op `elem` [Lt, Le, Gt, Ge]         = [IntS, IntS, BoolS]
  | op `elem` [And, Or, Implies, Iff]  = [BoolS, BoolS, BoolS]
  | op `elem` [Union, Intersect, Diff] = [SetS $ VarS "a", SetS $ VarS "a", SetS $ VarS "a"]
  | op `elem` [Member]                 = [VarS "a", SetS $ VarS "a", BoolS]
  | op `elem` [Subset]                 = [SetS $ VarS "a", SetS $ VarS "a", BoolS]

-- | Variable substitution
type Substitution = Map Id Formula

-- | Formulas of the refinement logic
data Formula =
  BoolLit Bool |                      -- ^ Boolean literal
  IntLit Integer |                    -- ^ Integer literal
  SetLit Sort [Formula] |             -- ^ Set literal ([1, 2, 3])
  MapLit Sort Formula |               -- ^ Map literal; key sort, default value
  MapSel Formula Formula |            -- ^ Map select
  MapUpd Formula Formula Formula |    -- ^ Map update
  -- MapUni Formula Formula              -- ^ Map union
  Var Sort Id |                       -- ^ Input variable (universally quantified first-order variable)
  Unknown Substitution Id |           -- ^ Predicate unknown (with a pending substitution)
  Unary UnOp Formula |                -- ^ Unary expression
  Binary BinOp Formula Formula |      -- ^ Binary expression
  Ite Formula Formula Formula |       -- ^ If-then-else expression
  Func Sort Id [Formula] |            -- ^ Logic function application
  Cons Sort Id [Formula] |            -- ^ Constructor application
  All Formula Formula                 -- ^ Universal quantification
  deriving (Show, Eq, Ord)

-- | Perform recursive traversal of formulas
mapFormula :: (Formula -> Formula) -> Formula -> Formula
mapFormula func b@(BoolLit _)       = func b
mapFormula func i@(IntLit  _)       = func i
mapFormula func   (SetLit s fs)     = func $ SetLit s fs'
  where
    fs' = map (mapFormula func) fs
mapFormula func   (MapLit s f)      = func $ MapLit s f'
  where
    f' = mapFormula func f
mapFormula func   (MapSel f1 f2)    = func $ MapSel f1' f2'
  where
    f1' = mapFormula func f1
    f2' = mapFormula func f2
mapFormula func   (MapUpd f1 f2 f3) = func $ MapUpd f1' f2' f3'
  where
    f1' = mapFormula func f1
    f2' = mapFormula func f2
    f3' = mapFormula func f3
mapFormula func v@(Var _ _)         = func v
mapFormula func u@(Unknown _ _)     = func u
mapFormula func   (Unary op f)      = func $ Unary op f'
  where
    f' = mapFormula func f
mapFormula func   (Binary op f1 f2) = func $ Binary op f1' f2'
  where
    f1' = mapFormula func f1
    f2' = mapFormula func f2
mapFormula func   (Ite f1 f2 f3)    = func $ Ite f1' f2' f3'
  where
    f1' = mapFormula func f1
    f2' = mapFormula func f2
    f3' = mapFormula func f3
mapFormula func   (Func s p fs)     = func $ Func s p fs'
  where
    fs' = map (mapFormula func) fs
mapFormula func   (Cons s c fs)     = func $ Cons s c fs'
  where
    fs' = map (mapFormula func) fs
mapFormula func   (All f1 f2)       = func $ All f1' f2'
  where
    f1' = mapFormula func f1
    f2' = mapFormula func f2

-- | Base type of a term in the refinement logic
sortOf :: Formula -> Sort
sortOf (BoolLit _)                               = BoolS
sortOf (IntLit _)                                = IntS
sortOf (SetLit s _)                              = SetS s
sortOf (MapLit k v)                              = MapS k $ sortOf v
sortOf (MapSel m _)                              = valueSort $ sortOf m
sortOf (MapUpd m _ _)                            = sortOf m
sortOf (Var s _ )                                = s
sortOf (Unknown _ _)                             = BoolS
sortOf (Unary op _)
  | op == Neg                                    = IntS
  | otherwise                                    = BoolS
sortOf (Binary op e1 _)
  | op == Times || op == Plus || op == Minus     = IntS
  | op == Union || op == Intersect || op == Diff = sortOf e1
  | otherwise                                    = BoolS
sortOf (Ite _ e1 _)                              = sortOf e1
sortOf (Func s _ _)                              = s
sortOf (Cons s _ _)                              = s
sortOf (All _ _)                                 = BoolS

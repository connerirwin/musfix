{-# LANGUAGE OverloadedStrings #-}

module Language.SMT.Syntax where

import Data.Map (Map)  
import qualified Data.Map as Map

-- | Identifiers
type Id = String

-- | Top-level input expressions
--
-- These expressions are fed into the program via a constraints file and produced by the parser.
--
data InputExpr =
    Qualifier Id [Formula] Formula          -- ^ Qualifier with name, variables and equation
  | WFConstraint Id [Formula]               -- ^ Well-formed predicate constraint
  | HornConstraint Formula                  -- ^ Horn constraint
  deriving (Show, Eq, Ord)
  
-- | Sorts
data Sort = BoolS | IntS | VarS Id | DataS Id [Sort] | SetS Sort | AnyS
  deriving (Show, Eq, Ord)

-- | Unary operators
data UnOp = Neg | Not
  deriving (Show, Eq, Ord)

-- | Binary operators
data BinOp =
    Times | Plus | Minus |          -- ^ Int -> Int -> Int
    Eq | Neq |                      -- ^ a -> a -> Bool
    Lt | Le | Gt | Ge |             -- ^ Int -> Int -> Bool
    And | Or | Implies | Iff |      -- ^ Bool -> Bool -> Bool
    Union | Intersect | Diff |      -- ^ Set -> Set -> Set
    Member | Subset                 -- ^ Int/Set -> Set -> Bool
  deriving (Show, Eq, Ord)

-- | Variable substitution
type Substitution = Map Id Formula

-- | Formulas of the refinement logic
data Formula =
  BoolLit Bool |                      -- ^ Boolean literal
  IntLit Integer |                    -- ^ Integer literal
  SetLit Sort [Formula] |             -- ^ Set literal ([1, 2, 3])
  Var Sort Id |                       -- ^ Input variable (universally quantified first-order variable)
  Unknown Substitution Id |           -- ^ Predicate unknown (with a pending substitution)
  Unary UnOp Formula |                -- ^ Unary expression
  Binary BinOp Formula Formula |      -- ^ Binary expression
  Ite Formula Formula Formula |       -- ^ If-then-else expression
  Pred Sort Id [Formula] |            -- ^ Logic function application
  Cons Sort Id [Formula] |            -- ^ Constructor application
  All Formula Formula                 -- ^ Universal quantification
  deriving (Show, Eq, Ord)
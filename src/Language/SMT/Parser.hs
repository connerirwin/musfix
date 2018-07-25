{-# LANGUAGE OverloadedStrings #-}

module Language.SMT.Parser where

import Language.SMT.Syntax
import Language.Synquid.Logic
import Language.Synquid.Tokens

import Control.Monad
import qualified Data.Text as T
import Data.AttoLisp
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Scientific as S

-- | Unary operators
unaryOps :: Map T.Text UnOp
unaryOps = Map.fromList [ ("not",     Not)
                        , ("-",       Neg)
                        ]

-- | Binary operators
binaryOps :: Map T.Text BinOp
binaryOps = Map.fromList [ ("*",    Times)
                         , ("+",     Plus)
                         , ("-",    Minus)
                         , ("==",      Eq)
                         , ("=",       Eq) -- TODO: Do we want to support this?
                         , ("!=",     Neq)
                         , ("<",       Lt)
                         , ("<=",      Le)
                         , (">",       Gt)
                         , (">=",      Ge)
                         , ("&&",     And)
                         , ("||",      Or)
                         , ("==>",Implies)
                         , ("=>", Implies) -- TODO: Same as above
                         , ("<==>",   Iff)
                         ]

-- | Variable sorts
sorts :: Map T.Text Sort
sorts = Map.fromList [ ("int",   IntS),
                       ("bool", BoolS)
                     ]

{- Top-level Statements -}
checkVars :: [Formula] -> Bool
checkVars fs = foldr check True fs
  where
    check :: Formula -> Bool -> Bool
    check f r = case f of
        (Var s i) -> r
        _ -> False

instance FromLisp InputExpr where
  -- qualifiers
  parseLisp (List [(Symbol "qualif"), (Symbol n), List xs, y]) = do
      vars <- mapM parseFormula xs
      if not $ checkVars vars then do
          fail $ "invalid expressions in variable list"
      else do
          formula <- parseFormula y
          return $ Qualifier (T.unpack n) vars formula
  -- well-formed constraint
  parseLisp (List [(Symbol "wf"), (Symbol n), List xs]) = do
      vars <- mapM parseFormula xs
      if not $ checkVars vars then do
          fail $ "invalid expressions in variable list"
      else do
        return $ WFConstraint (T.unpack n) vars
  -- horn constraint
  parseLisp (List [(Symbol "constraint"), y]) = do
      formula <- parseFormula y
      return $ HornConstraint formula
  parseLisp _ = fail "unknown top-level construct"

parseInputExpr :: Lisp -> Parser InputExpr
parseInputExpr = parseLisp

{- Formulas -}
instance FromLisp Formula where
  -- basic literals
  parseLisp (Symbol "False")          = pure $ BoolLit False
  parseLisp (Symbol "True")           = pure $ BoolLit True
  parseLisp (Number n)                = case S.floatingOrInteger n of
      Left f -> fail $ "non-integral value not supported " ++ (show n)
      Right i -> pure $ IntLit i
  parseLisp (Symbol v)                = pure $ Var AnyS (T.unpack v)
  -- for all
  parseLisp (List [(Symbol "forall"), x, y])  = do
      xExpr <- parseFormula x
      yExpr <- parseFormula y
      return $ All xExpr yExpr
  -- unary operation
  parseLisp (List [(Symbol f), a])
      | Map.member f unaryOps     = do
          let op = unaryOps ! f
          expr <- parseFormula a
          return $ Unary op expr
  -- logical predicate application
  parseLisp (List [(Symbol p), x])
      | T.head p == '$'             = do
          arg <- parseFormula x
          return $ Pred AnyS (T.unpack p) [] -- should be unknown
  -- variable sort assignment
  parseLisp (List [(Symbol v), (Symbol s)])
      | Map.member s sorts        = do
          let sort = sorts ! s
          return $ Var sort (T.unpack v)
      | otherwise                 = fail $ "unrecognized sort " ++ (T.unpack s)
  -- binary operation
  parseLisp (List [(Symbol f), x, y]) = do
      let maybeOp = Map.lookup f binaryOps
      case maybeOp of
        Nothing -> fail $ "invalid binary op " ++ (T.unpack f)
        Just (op) -> do
            xExpr <- parseFormula x
            yExpr <- parseFormula y
            return $ Binary op xExpr yExpr

  parseLisp f = fail $ "cannot read formula: " ++ (show f)

parseFormula :: Lisp -> Parser Formula
parseFormula l = parseLisp l

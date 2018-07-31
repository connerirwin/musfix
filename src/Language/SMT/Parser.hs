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
import qualified Data.Set as Set
import Data.Set (Set)

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

reserved :: Set T.Text
reserved = collectKeys [Map.keys unaryOps, Map.keys binaryOps, Map.keys sorts]
  where
    collectKeys = foldr (Set.union . Set.fromList) Set.empty

{- Top-level Statements -}
checkVars :: [Formula] -> Bool
checkVars fs = foldr ((&&) . isVar) True fs
  where
    isVar :: Formula -> Bool
    isVar (Var _ _) = True
    isVar _         = False

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
  parseLisp (List [(Symbol "constraint"), List [(Symbol "forall"), List xs, y]]) = do
    vars <- mapM parseFormula xs
    if not $ checkVars vars then do
      fail $ "invalid variable in constraint forall"
    else do
      formula <- parseFormula y
      return $ HornConstraint vars formula
  -- uninterpreted function
  parseLisp (List [(Symbol "uninterp"), (Symbol n), List ps, rt]) = do
    params <- mapM parseSort ps -- add a dummy flag to help with parsing?
    output <- parseSort rt
    return $ UninterpFunction (T.unpack n) params output
  parseLisp _ = fail "unknown top-level construct"

parseInputExpr :: Lisp -> Parser InputExpr
parseInputExpr = parseLisp

{- Formulas -}
instance FromLisp Formula where
  -- basic literals
  parseLisp (Symbol "False")          = pure $ BoolLit False
  parseLisp (Symbol "True")           = pure $ BoolLit True
  parseLisp (Number n)                = case S.floatingOrInteger n of
    Left  f -> fail $ "non-integral value not supported " ++ (show n)
    Right i -> pure $ IntLit i
  -- variable
  parseLisp (Symbol v)                = pure $ Var AnyS (T.unpack v)
  -- for all
  parseLisp (List [(Symbol "forall"), x, y]) = do
    xExpr <- parseFormula x
    yExpr <- parseFormula y
    return $ All xExpr yExpr
  -- unknown application
  parseLisp (List ((Symbol p):x:xs))
    | T.head p == '$' = do
      args <- mapM parseFormula (x:xs)
      let subs = Map.fromList $ toPairs 0 args
        in do
          return $ Unknown subs $ T.unpack p
      where
        toPairs n (x:xs) = ("a" ++ (show n), x):(toPairs (n + 1) xs)
        toPairs _ [] = []
  -- variable sort assignment
  parseLisp (List [(Symbol v), (Symbol s)])
    | not $ Set.member v reserved = do
      sort <- parseSort (Symbol s)
      return $ Var sort $ T.unpack v
  -- unary operation
  parseLisp (List [(Symbol f), a]) = do
    let maybeOp = Map.lookup f unaryOps
    case maybeOp of
      Nothing -> fail $ "invalid unary op " ++ T.unpack f
      Just op -> do
        expr <- parseFormula a
        return $ Unary op expr
  -- binary operation
  parseLisp (List [(Symbol f), x, y]) = do
    let maybeOp = Map.lookup f binaryOps
    case maybeOp of
      Nothing -> fail $ "invalid binary op " ++ T.unpack f
      Just (op) -> do
        xExpr <- parseFormula x
        yExpr <- parseFormula y
        return $ Binary op xExpr yExpr
  -- logical predicate application
  parseLisp (List ((Symbol p):x:xs)) = do
    args <- mapM parseFormula (x:xs)
    return $ Pred AnyS (T.unpack p) args

  parseLisp f = fail $ "cannot read formula: " ++ show f

parseFormula :: Lisp -> Parser Formula
parseFormula = parseLisp

{- Sorts -}
instance FromLisp Sort where
  parseLisp (Symbol s)
    | Map.member s sorts = do
      let sort = sorts ! s
      return $ sort
    | otherwise          = fail $ "unrecognized sort " ++ T.unpack s

parseSort :: Lisp -> Parser Sort
parseSort = parseLisp

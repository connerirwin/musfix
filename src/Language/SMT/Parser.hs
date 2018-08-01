{-# LANGUAGE OverloadedStrings #-}

module Language.SMT.Parser where

import Language.SMT.Syntax
import Language.Synquid.Logic
import Language.Synquid.Tokens

import Control.Monad

import Data.AttoLisp
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Scientific as S
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import Data.Text (Text)

-- | Unary operators
unaryOps :: Map Text UnOp
unaryOps = Map.fromList [ ("not",     Not)
                        , ("-",       Neg)
                        ]

-- | Binary operators
binaryOps :: Map Text BinOp
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
sorts :: Map Text Sort
sorts = Map.fromList [ ("Int",   IntS),
                       ("Bool", BoolS)
                     ]

reserved :: Set Text
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
  -- | Qualifiers
  parseLisp (List [(Symbol "qualif"), (Symbol n), List xs, y]) = do
      vars <- mapM parseFormula xs
      if not $ checkVars vars then
        fail "invalid expressions in variable list"
      else do
          formula <- parseFormula y
          return $ Qualifier (T.unpack n) vars formula
  -- | Well-formed constraint
  parseLisp (List [(Symbol "wf"), (Symbol n), List xs]) = do
      vars <- mapM parseFormula xs
      if not $ checkVars vars then
        fail "invalid expressions in variable list"
      else
        return $ WFConstraint (T.unpack n) vars
  -- | Horn constraint
  parseLisp (List [(Symbol "constraint"), List [(Symbol "forall"), List xs, y]]) = do
    vars <- mapM parseFormula xs
    if not $ checkVars vars then
      fail "invalid variable in constraint forall"
    else do
        formula <- parseFormula y
        return $ HornConstraint vars formula
  -- | Uninterpreted function declaration
  parseLisp (List [(Symbol "uninterp"), (Symbol n), List ps, rt]) = do
    params <- mapM parseSortM ps
    output <- parseSortM rt
    return $ UninterpFunction (T.unpack n) params output

  parseLisp _ = fail "unknown top-level construct"

parseInputExpr :: Lisp -> Parser InputExpr
parseInputExpr = parseLisp

{- Formulas -}
instance FromLisp Formula where
  -- | Basic literals
  parseLisp (Symbol "False")          = pure $ BoolLit False
  parseLisp (Symbol "True")           = pure $ BoolLit True
  parseLisp (Number n)                = case S.floatingOrInteger n of
    Left  f -> fail $ "non-integral value not supported " ++ (show n)
    Right i -> pure $ IntLit i
  -- | Variable
  parseLisp (Symbol v)                = pure $ Var AnyS (T.unpack v)
  -- | Variable sort assignment
  -- This needs to be incredibly strict, otherwise it overlaps with unary uninterpreted function applications
  parseLisp (List [(Symbol v), s])
    | Set.notMember v reserved      -- ^ Variable name is not a reserved keyword
      && T.head v /= '$'            -- ^ Variable name is not an unknown application
      && isSort s                   -- ^ Sort is valid
      = do
        sort <- parseSortM s
        return $ Var sort $ T.unpack v
  -- | For all
  parseLisp (List [(Symbol "forall"), x, y]) = do
      xExpr <- parseFormula x
      yExpr <- parseFormula y
      return $ All xExpr yExpr
  -- | unary operation
  parseLisp (List [(Symbol f), a])
    | Map.member f unaryOps = do
        let op = unaryOps ! f
        expr <- parseFormula a
        return $ Unary op expr
  -- | binary operation
  parseLisp (List [(Symbol f), x, y])
    | Map.member f binaryOps = do
        let op = binaryOps ! f
        xExpr <- parseFormula x
        yExpr <- parseFormula y
        return $ Binary op xExpr yExpr
  -- | unknown application
  parseLisp (List ((Symbol p):xs))
    | T.head p == '$' = do
        args <- mapM parseFormula xs
        let subs = Map.fromList $ toPairs 0 args
        return $ Unknown subs $ T.unpack p
      where
        toPairs n (x:xs) = ("a" ++ (show n), x):(toPairs (n + 1) xs)
        toPairs _ []     = []
  -- | uninterpreted function application
  parseLisp (List ((Symbol p):xs)) = do
      args <- mapM parseFormula xs
      return $ Pred AnyS (T.unpack p) args

  parseLisp f = fail $ "cannot read formula: " ++ show f

parseFormula :: Lisp -> Parser Formula
parseFormula = parseLisp

{- Sorts -}
instance FromLisp Sort where
  -- | list
  parseLisp (Symbol s) = do
    let maybeSort = parseSort (Symbol s)
    case maybeSort of
      Nothing -> fail $ "unrecognized sort " ++ T.unpack s
      Just sort -> return sort

parseSortM :: Lisp -> Parser Sort
parseSortM = parseLisp

-- TODO cleanup, there is a better way to forward the nothing using monads, but this works for now
parseSort :: Lisp -> Maybe Sort
parseSort (Symbol s)
  | T.head s == '[' && T.last s == ']' = do
      sort <- parseSort subsort -- ^ TODO make sure that this is working properly
      return $ SetS sort
    where
      subsort = Symbol $ (T.drop 1 . T.dropEnd 1) s

parseSort (Symbol s) = Map.lookup s sorts

isSort :: Lisp -> Bool
isSort s = case parseSort s of
    Nothing -> False
    Just _  -> True

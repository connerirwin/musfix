{-# LANGUAGE OverloadedStrings #-}

module Language.SMT.Parser (
  parseInputExpr,
) where

import Language.SMT.Syntax

import Language.Synquid.Logic
import Language.Synquid.Tokens

import Control.Monad

import Data.AttoLisp
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.Scientific as S
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import Data.Text (Text)

import Debug.Trace

{- Utility functions -}
debugOut a = traceShow a a

getInt :: S.Scientific -> Int
getInt n = case S.floatingOrInteger n of
  Left  f -> error $ "non-integral value not supported " ++ (show n)
  Right i -> i


-- | Unary operators
unaryOps :: Map Text UnOp
unaryOps = Map.fromList [ ("not",     Not)
                        , ("-",       Neg)
                        ]

-- | Binary operators
binaryOps :: Map Text BinOp
binaryOps = Map.fromList [ ("*",     Times)
                         , ("+",      Plus)
                         , ("-",     Minus)
                         , ("==",       Eq)
                         , ("=",        Eq)
                         , ("!=",      Neq)
                         , ("<",        Lt)
                         , ("<=",       Le)
                         , (">",        Gt)
                         , (">=",       Ge)
                         , ("&&",      And)
                         , ("and",     And)
                         , ("||",       Or)
                         , ("or",       Or)
                         , ("==>", Implies)
                         , ("=>",  Implies)
                         , ("<==>",    Iff)
                         , ("<=>",     Iff)
                         , ("cup",   Union)
                         , ("union", Union)
                         , ("cap", Intersect)
                         , ("^",   Intersect)
                         , ("intersect", Intersect)
                         , ("diff",     Diff)
                         , ("/",        Diff)
                         , ("in",     Member)
                         , ("member", Member)
                         , ("subset", Subset)
                         ]

-- | Variable sorts
sorts :: Map Text Sort
sorts = Map.fromList [ ("Int",   IntS)
                     , ("Bool", BoolS)
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
  -- | Custom Sorts
  parseLisp (List [(Symbol "declare-sort"), (Symbol n), (Number num)]) =
      return $ SortDecl (T.unpack n) (getInt num)
  -- | Uninterpreted function declaration
  parseLisp (List [(Symbol "declare-fun"), (Symbol n), List ps, rt]) = do
      params <- mapM parseSortM ps
      output <- parseSortM rt
      return $ UninterpFunction (T.unpack n) params output
  -- | Constants
  parseLisp (List [(Symbol "declare-const"), (Symbol n), rt]) = do
      output <- parseSortM rt
      return $ ConstantDecl (T.unpack n) output
  -- | Distinct assertion
  parseLisp (List [(Symbol "assert"), List ((Symbol "distinct"):xs)]) = do
      let names = map sym xs
      return $ DistinctDecl names
    where
      sym :: Lisp -> Id
      sym (Symbol n) = T.unpack n
      sym _          = error "invalid symbol in distinct"
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
  -- | Set
  parseLisp (Symbol "{}") = do
      pure $ SetLit AnyS []
  parseLisp (List ((Symbol "Set"):v:vs))
    | not $ isSort v  = do
      vals <- mapM parseFormula (v:vs)
      pure $ SetLit (VarS "_any") vals
  -- | Map
  parseLisp (List [(Symbol "Map_default"), v]) = do
      defVal  <- parseFormula v
      pure $ MapLit AnyS defVal -- ^ This refers to an unknown key sort
  parseLisp (List [(Symbol "Map_select"), m, k]) = do
      mapExpr <- parseFormula m
      key     <- parseFormula k
      return $ MapSel mapExpr key
  parseLisp (List [(Symbol "Map_store"), m, k, v ]) = do
      mapExpr <- parseFormula m
      key     <- parseFormula k
      val     <- parseFormula v
      return $ MapUpd mapExpr key val
      -- | TODO this is going to be implemented using the z3 map somehow
  -- parseLisp (List [(Symbol "Map_union"), m1, m2]) = do
  --     map1    <- parseFormula m1
  --     map2    <- parseFormula m2
  --     return $ MapUni map1 map2
  -- | Variable
  -- This currently incorrectly matches against declared constants, which are then later resolved
  parseLisp (Symbol v)
    | Set.notMember v reserved
      && ((Char.isLower $ T.head v) || (T.head v == '_'))
      && T.head v /= '$'
      = pure $ Var AnyS (T.unpack v)
  -- | Variable sort assignment
  parseLisp (List [(Symbol v), s])
    | Set.notMember v reserved      -- ^ Variable name is not a reserved keyword
      && ((Char.isLower $ T.head v) || (T.head v == '_'))
      && T.head v /= '$'            -- ^ Variable name is not an unknown application
      && isSort s                   -- ^ Sort is valid
      = do
        sort <- parseSortM s
        return $ Var sort $ T.unpack v
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
  parseLisp (List ((Symbol p):x:xs))
    | T.head p == '$' = do
        args <- mapM parseFormula (x:xs)
        let subs = Map.fromList $ zip distinctFormals args
        return $ Unknown subs $ T.unpack p
      where
        distinctFormals = map (\i -> "a" ++ show i) [0..]
  -- | uninterpreted function application
  parseLisp (List ((Symbol p):x:xs))
    | Set.notMember p reserved
      && (Char.isLower . T.head) p = do
        args <- mapM parseFormula (x:xs)
        return $ Func AnyS (T.unpack p) args
  -- | Constructor
  parseLisp (List ((Symbol p):x:xs))
    | Set.notMember p reserved
      && (Char.isUpper . T.head) p = do
        args <- mapM parseFormula (x:xs)
        return $ Cons AnyS name args
      where
        name = T.unpack p

  parseLisp f = fail $ "cannot read formula: " ++ show f

parseFormula :: Lisp -> Parser Formula
parseFormula = parseLisp

{- Sorts -}
instance FromLisp Sort where
  -- | Sorts can be multiple symbols
  parseLisp s = do
    let maybeSort = parseSort s
    case maybeSort of
      Nothing -> fail $ "unrecognized sort " ++ show s
      Just sort -> return sort

parseSortM :: Lisp -> Parser Sort
parseSortM = parseLisp

parseSort :: Lisp -> Maybe Sort
-- | Set
parseSort (List [(Symbol "Set"), s]) = do
    sort <- parseSort s
    return $ SetS sort
-- | Map
parseSort (List [(Symbol "Map"), k, v]) = do
    keySort <- parseSort k
    valSort <- parseSort v
    return $ MapS keySort valSort
-- | Polymorphic sort
parseSort (Symbol s)
  | T.head s == '@'
    && (T.head $ T.drop 1 s) /= '_'  = pure $ VarS name
    where
      name = T.unpack $ T.tail s
-- | Sort literal
parseSort (Symbol s)
  | Map.member s sorts = Map.lookup s sorts
-- | Constructor
parseSort (List ((Symbol n):fs))
  | (Char.isUpper $ T.head n) || (T.head n == '@') = do
      formals <- mapM parseSort fs
      return $ DataS name formals
    where
      name = if T.head n == '@' then "_any" else T.unpack n
-- | In order to allow a no-arg constructor to not need parenthesis
parseSort (Symbol n)
  | (Char.isUpper $ T.head n) || (T.head n == '@') = do
      return $ DataS name []
    where
      name = if T.head n == '@' then "_any" else T.unpack n
parseSort _ = Nothing

isSort :: Lisp -> Bool
isSort s = case parseSort s of
    Nothing -> False
    Just _  -> True

{-# LANGUAGE OverloadedStrings #-}

module Language.SMT.Parser where
    
import Language.SMT.Logic
import Language.SMT.Tokens

import Control.Monad
import qualified Data.Text as T
import Data.AttoLisp
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Scientific as S

-- | Invert a map (must be 1-to-1)
invertTokenMap :: Ord a => Map a String -> Map T.Text a
invertTokenMap m = Map.fromList m'
    where
        xs = Map.toList m
        m' = map f xs
        f (k, v) = ((T.pack v), k)

{- Operators -}
-- TODO: This does not work, some operators are overloaded!
unaryOps = invertTokenMap unOpTokens
binaryOps = invertTokenMap binOpTokens

{- Sorts -}
sorts :: Map T.Text Sort 
sorts = Map.fromList [ ("int",   IntS),
                       ("bool", BoolS)
                     ]

{- Top-level Statements -}
data InputExpr =
     Qualifier T.Text [Formula] Formula          -- ^ Qualifier with name, variables and equation
   | Constraint Formula                          -- ^ Constraint equation
   | UnknownPredicate T.Text Formula             -- ^ Unknown logical predicate (to find)
   deriving (Show, Eq, Ord)
   
checkVars :: [Formula] -> Bool
checkVars fs = foldr check True fs
    where
        check :: Formula -> Bool -> Bool
        check f r = case f of
            (Var s i) -> r
            _ -> False

instance FromLisp InputExpr where
    parseLisp (List [(Symbol "qualif"), (Symbol n), List xs, y]) = do
            vars <- mapM parseFormula xs
            if not $ checkVars vars then do
                fail $ "invalid expressions in variable list"
            else do
                formula <- parseFormula y
                return $ Qualifier n vars formula
    parseLisp (List [(Symbol "constraint"), y]) = do
            formula <- parseFormula y
            return $ Constraint formula
    parseLisp (List [(Symbol "wf"), (Symbol n), x]) = do
            var <- parseFormula x
            case var of
                (Var s i) -> return $ UnknownPredicate n var
                _ -> fail "predicate parameter is not a variable"
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
            return $ Pred AnyS (T.unpack p) []
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

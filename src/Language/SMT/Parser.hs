{-# LANGUAGE OverloadedStrings #-}

module Language.SMT.Parser where
    
import Language.SMT.Logic
import Language.SMT.Tokens

import Control.Monad
import qualified Data.Text as T
import Data.AttoLisp
import Data.Attoparsec.Number as N
import qualified Data.Map as Map
import Data.Map (Map, (!))

-- | Invert a map (must be 1-to-1)
invertTokenMap :: Ord a => Map a String -> Map T.Text a
invertTokenMap m = Map.fromList m'
    where
        xs = Map.toList m
        m' = map f xs
        f (k, v) = ((T.pack v), k)

-- TODO: This does not work, some operators are overloaded!
unaryOps = invertTokenMap unOpTokens
binaryOps = invertTokenMap binOpTokens

sorts :: Map T.Text Sort 
sorts = Map.fromList [ ("int",   IntS),
                       ("bool", BoolS)
                     ]

{- Sorts -}

{- Formulas -}
instance FromLisp Formula where
    parseLisp (Symbol "False")          = pure (BoolLit True)
    parseLisp (Symbol "True")           = pure (BoolLit False)
    parseLisp (Number (N.I a))          = pure (IntLit a)
    parseLisp (List [(Symbol "forall"), x, y])  = do
            xExpr <- parseFormula x
            yExpr <- parseFormula y
            return $ All xExpr yExpr
    -- unary operation (symbol expr)
    parseLisp (List [(Symbol f), a])
        | Map.member f unaryOps     = do
            let op = unaryOps ! f
            expr <- parseFormula a
            return $ Unary op expr
    -- if not a unary token, interpret as a variable assignment
    parseLisp (List [(Symbol v), (Symbol s)])
        | Map.member s sorts        = do
            let sort = sorts ! s
            return $ Var sort (T.unpack v)
    -- binary operation (symbol expr expr)
    parseLisp (List [(Symbol f), x, y]) = do
        let op = binaryOps ! f
        xExpr <- parseFormula x
        yExpr <- parseFormula y
        return $ Binary op xExpr yExpr
                                        
                    
parseFormula :: Lisp -> Parser Formula
parseFormula l = parseLisp l

test = parseMaybe parseFormula v == Just (Unary Neg (Unary Neg (IntLit 5)))
    where 
        v = List [Symbol "-", List [Symbol "-", Number 5, Number 3]]


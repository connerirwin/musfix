{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString as A
import Language.SMT.Parser
import qualified Data.AttoLisp as L

main :: IO ()
main = do
        print $ f
    where
        l = case A.parseOnly L.lisp "(+ (v1 int) (+ 2 (- 5)))" of
            (Left err) -> error $ "bad input: " ++ err
            (Right lexpr) -> lexpr
        f = case L.parseMaybe parseFormula l of
            (Just formula) -> formula
            Nothing -> error "bad formula"
    
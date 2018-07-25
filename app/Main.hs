{-# LANGUAGE OverloadedStrings #-}

import Language.SMT.Parser
import Language.SMT.Resolver hiding (InputExpr)

import Control.Monad
import Data.ByteString (ByteString)
import System.Environment
import System.Exit
import qualified Data.Attoparsec.ByteString as A
import qualified Data.AttoLisp as L
import qualified Data.ByteString as ByteString

data ProgramOptions = ProgramOptions {
  printResult :: Bool,
  outputFile :: String
} deriving (Show, Eq)

version = putStrLn "musfix 0.1.0.0"
usage   = putStrLn "Usage: musfix [file ...]"

main :: IO ()
main = do
  args <- getArgs
  parseArgs args

parseArgs :: [String] -> IO ()
parseArgs as    = mapM_ parseArg as

parseArg :: String -> IO ()
parseArg "-d"   = debug
parseArg "-h"   = usage     >> exitSuccess
parseArg "-v"   = version   >> exitSuccess
parseArg f      = readConstraints f

readConstraints :: String -> IO ()
readConstraints f = do
    s <- ByteString.readFile f
    let lisp = topSExprs $ s in
      let ins = topInputs lisp in
      putStrLn $ show ins

topSExprs :: ByteString -> [L.Lisp]
topSExprs l = case A.parseOnly (A.many1 L.lisp) l of
    Left err -> error $ "ill formatted lisp: " ++ err
    Right ins -> ins

topInputs :: [L.Lisp] -> [InputExpr]
topInputs ls = map p ls
  where
    p l = case L.parse parseInputExpr l of
        L.Success i -> i
        L.Error r -> error $ "bad input: " ++ r ++ "\n while attempting to parse lisp:" ++ (show l)

-- | A debug printing function designed to be as unobtrusive as possible
debug :: IO ()
debug = putStrLn "debug"

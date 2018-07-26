{-# LANGUAGE OverloadedStrings #-}

import Language.SMT.Syntax
import Language.SMT.Parser
import Language.SMT.Resolver

import Control.Monad
import Data.ByteString (ByteString)
import System.Environment
import System.Exit
import qualified Data.Attoparsec.ByteString as A
import qualified Data.AttoLisp as L
import qualified Data.ByteString as ByteString

data ProgramOptions = ProgramOptions {
  printProgramOutput :: Bool,
  programOutputFile :: String,
  programVerboseLogging :: Bool
} deriving (Show, Eq)

defaultProgramOptions = ProgramOptions {
  printProgramOutput = False,
  programOutputFile = "out.txt",
  programVerboseLogging = False
}

version = putStrLn "musfix 0.1.0.0"
usage   = putStrLn "Usage: musfix [--help] [-p] [--verbose] [--version] [file ...]"
help    = putStrLn "\n\nAvailable options:\n--help\t\tShow this help text\n-p\t\tPrint results to standard out\n--version\tShow current version"
debug   = resolverDebug

main :: IO ()
main = do
  args <- getArgs
  parseArgs defaultProgramOptions args

parseArgs :: ProgramOptions -> [String] -> IO ()
parseArgs o (x:y:xs)
    | x == "-o"   = verboseLog o' m >> parseArgs o' xs
    where
      o' = o { programOutputFile = y }
      m  = "Setting output file to " ++ y
parseArgs o (x:xs)
    | x == "-d"             = debug               >> continue o
    | x == "--help"         = usage >> help       >> exitSuccess
    | x == "-p"             = continue $ o { printProgramOutput = True }
    | x == "--version"      = version             >> exitSuccess
    | x == "--verbose"      = continue $ o { programVerboseLogging = True }
    | otherwise   = readConstraints o x           >> continue o
    where
      continue o' = parseArgs o' xs
parseArgs _ _               = exitSuccess

readConstraints :: ProgramOptions -> String -> IO ()
readConstraints o f = do
    s <- ByteString.readFile f
    let lisp = topSExprs $ s
        ins = resolveSorts $ topInputs lisp
        qmap = generateQualifiers ins
      in do
        putStrLn "\n\nInputs\n--------"
        putStrLn $ show ins
        putStrLn "\n\nQMAP\n--------"
        putStrLn $ show qmap
        
      
verboseLog :: ProgramOptions -> String -> IO ()
verboseLog o s = if programVerboseLogging o then
    putStrLn $ s
  else
    return () 

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

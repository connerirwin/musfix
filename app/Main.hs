{-# LANGUAGE OverloadedStrings #-}

import Language.SMT.Parser
import Language.SMT.Resolver
import Language.SMT.Solve
import Language.SMT.Syntax

import Language.Synquid.Pretty

import Control.Monad

import qualified Data.Attoparsec.ByteString as A
import qualified Data.AttoLisp as L
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)

import System.Environment
import System.Exit

data ProgramOptions = ProgramOptions {
  printProgramOutput :: Bool,
  programOutputFile :: String,
  programVerboseLogging :: Bool,
  programUsesLeastFixpoint :: Bool
} deriving (Show, Eq)

defaultProgramOptions = ProgramOptions {
  printProgramOutput = False,
  programOutputFile = "out.txt",
  programVerboseLogging = False,
  programUsesLeastFixpoint = False
}

version = putStrLn "musfix 0.1.0.0"
usage   = putStrLn "Usage: musfix [-p] [-l] [--verbose] [-h|--help] [--version] [file...]"
help    = putStrLn "\nOptions:\n\
                   \  -p\t\tPrint results to standard out\n\
                   \  -l\t\tUse a least fixpoint solver (default is greatest fixpoint)\n\
                   \  --verbose\tOutput additional logging\n\
                   \  -h, --help\tShow this help text\n\
                   \  --version\tShow current version"
debug   = resolverDebug

main :: IO ()
main = do
  args <- getArgs
  parseArgs defaultProgramOptions args

-- | TODO make argument parsing more robust - add support for arguments in any
-- order, multiple flags with a single dash, clean up the help and usage
-- information
-- Consider using a library? (optparse-applicative)
parseArgs :: ProgramOptions -> [String] -> IO ()
parseArgs o (x:y:xs)
    | x == "-o"   = verboseLog o' m >> parseArgs o' xs
    where
      o' = o { programOutputFile = y }
      m  = "Setting output file to " ++ y
parseArgs o (x:xs)
    | x == "-d"             = debug               >> continue o
    | x == "-p"             = continue $ o { printProgramOutput = True }
    | x == "-l"             = continue $ o { programUsesLeastFixpoint = True }
    | x == "--verbose"      = continue $ o { programVerboseLogging = True }
    | x == "-h"             = usage >> help       >> exitSuccess
    | x == "--help"         = usage >> help       >> exitSuccess
    | x == "--version"      = version             >> exitSuccess
    | otherwise   = readConstraints o x           >> continue o
    where
      continue o' = parseArgs o' xs
parseArgs _ _               = exitSuccess

readConstraints :: ProgramOptions -> String -> IO ()
readConstraints o f = do
    s <- ByteString.readFile f
    let lisp = topSExprs $ s
        ins = prepareInputs $ topInputs lisp
        qmap = generateQualifiers ins
        horns = map formulas $ allHornConstraints ins
      in do
        verboseLog o $ "Preparing to find " ++ (fixPointType o) ++ " fixpoint..."
        verboseLog o "\nInputs\n--------"
        verboseLog o $ "Reading from file " ++ f
        mapM_ ((verboseLog o) . show) ins
        verboseLog o "\nQMAP\n--------"
        verboseLog o $ show qmap
        verboseLog o "\nCandidates\n--------"
        candidates <- findFixPoint (programUsesLeastFixpoint o) horns qmap
        verboseLog o $ show candidates
        verboseLog o "\n\nFinal candidates:"
        mapM_ ((finalOutput o) . show . pretty) candidates

formulas :: InputExpr -> Formula
formulas (HornConstraint vs f) = f
formulas _ = error "non-horn constraint in constraints"

finalOutput :: ProgramOptions -> String -> IO ()
finalOutput o s = putStrLn s

verboseLog :: ProgramOptions -> String -> IO ()
verboseLog o s = if programVerboseLogging o then
    putStrLn $ s
  else
    return ()

fixPointType :: ProgramOptions -> String
fixPointType o = if programUsesLeastFixpoint o then "least" else "greatest"

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

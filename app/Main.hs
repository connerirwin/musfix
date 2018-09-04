{-# LANGUAGE OverloadedStrings #-}

module Main where

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
  printOutput    :: Bool,
  appendOutput   :: Bool,
  outputFile     :: String,
  verboseLogging :: Bool,
  leastFixpoint  :: Bool
} deriving (Show, Eq)

defaultProgramOptions = ProgramOptions {
  printOutput    = True,
  appendOutput   = False,
  outputFile     = "",
  verboseLogging = False,
  leastFixpoint  = False
}

version = putStrLn "musfix 0.1.0.0"
usage   = putStrLn "Usage: musfix [-o FILE] [-s|--silent] [-l] [--verbose] [-h|--help] [--version] [file...]"
help    = putStrLn "\nOptions:\n\
                   \  -o\t\tPrint results to the specified file\n\
                   \  -s, --silent\t\tRun the program without printing results to standard out\n\
                   \  -l\t\tUse a least fixpoint solver (default is greatest fixpoint)\n\
                   \  --verbose\tOutput additional logging\n\
                   \  -h, --help\tShow this help text\n\
                   \  --version\tShow current version"

main :: IO ()
main = do
  args <- getArgs
  parseArgs defaultProgramOptions args

-- TODO add ProgramOptions as State

-- | TODO make argument parsing more robust - add support for arguments in any
-- order, multiple flags with a single dash, clean up the help and usage
-- information
-- Consider using a library? (optparse-applicative), this library is now required already by dependencies
parseArgs :: ProgramOptions -> [String] -> IO ()
parseArgs o (x:y:xs)
    | x == "-o"   = do
      let o' = o { outputFile = y }
      verboseLog o' $ "Setting output file to " ++ y
      parseArgs o' xs
parseArgs o (x:xs)
    | x `elem` ["-s", "--silent"] = continue $ o { printOutput    = False }
    | x == "-l"                   = continue $ o { leastFixpoint  = True }
    | x == "--verbose"            = continue $ o { verboseLogging = True }
    | x `elem` ["-h", "--help"]   = usage >> help       >> exitSuccess
    | x == "--version"            = version             >> exitSuccess
    | otherwise                   = readConstraints o x >> continue o
    where
      continue o' = parseArgs o' xs
parseArgs _ _               = exitSuccess

readConstraints :: ProgramOptions -> String -> IO ()
readConstraints o f = do
    s <- ByteString.readFile f
    let lisp = topSExprs s
    let ins = prepareInputs $ topInputs lisp
    let qmap = generateQualifiers ins
    let horns = map formulas $ allHornConstraints ins
    prepOutput o
    verboseLog o $ "Preparing to find " ++ (fixPointType o) ++ " fixpoint..."
    verboseLog o $"\nInputs\n--------"
    verboseLog o $ "Reading from file " ++ f
    mapM_ ((verboseLog o) . show) ins
    verboseLog o $ "\nQMAP\n--------"
    verboseLog o $ show qmap
    verboseLog o $ "\nCandidates\n--------"
    candidates <- findFixPoint (leastFixpoint o) horns qmap
    verboseLog o $ show candidates
    verboseLog o $ "\n\nFinal candidates: "
    mapM_ ((normalLog o) . show . pretty) candidates
    normalLog o $ "\n"

prepOutput :: ProgramOptions -> IO ()
<<<<<<< HEAD
prepOutput o = unless (length (outputFile o) == 0 || appendOutput o) $ writeFile (outputFile o) "" -- ^ Wipe the outputFile
=======
prepOutput o = unless (length (outputFile o) < 0 || appendOutput o) $ writeFile (outputFile o) "" -- ^ Wipe the outputFile
>>>>>>> 2c0069ac7712a39427c090a93604304f733f41ed

verboseLog :: ProgramOptions -> String -> IO ()
verboseLog o s = when (verboseLogging o) $ normalLog o s

normalLog :: ProgramOptions -> String -> IO ()
normalLog o s = do
  when (printOutput o) $ putStrLn s
  when (length (outputFile o) > 0) $ appendFile (outputFile o) $ "\n" ++ s

formulas :: InputExpr -> Formula
formulas (HornConstraint vs f) = f
formulas _ = error "non-horn constraint in constraints"

fixPointType :: ProgramOptions -> String
fixPointType o = if leastFixpoint o then "least" else "greatest"

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

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Language.SMT.Parser
import Language.SMT.Resolver
import Language.SMT.Solve
import Language.SMT.Syntax

import Language.Synquid.Pretty hiding ((<>))

import Control.Monad

import qualified Data.Attoparsec.ByteString as A
import qualified Data.AttoLisp as L
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))
import Data.Version (showVersion)

import Development.GitRev (gitHash)

import Options.Applicative

import Paths_musfix (version)

import System.Environment
import System.Exit

data ProgramOptions = ProgramOptions {
  inputFiles     :: [String],
  outputFile     :: String,
  appendOutput   :: Bool,
  suppressOutput :: Bool,
  verboseLogging :: Bool,
  leastFixpoint  :: Bool
} deriving (Show, Eq)

cmdParser :: Parser ProgramOptions
cmdParser = ProgramOptions
  <$> some (argument str (metavar "INPUT FILE"))
  <*> strOption (long "output" <> short 'o' <> metavar "FILE" <> value "" <> help "Prints results to the specified file")
  <*> switch (long "append" <> short 'a' <> help "Append file output")
  <*> switch (long "silent" <> short 's' <> help "Supresses command-line output")
  <*> switch (long "verbose" <> help "Output additional logging")
  <*> switch (long "least-fixpoint" <> short 'l' <> help "Use a least fixpoint solver (default is greatest fixpoint)")

versionOption :: Parser (a -> a)
versionOption = infoOption (concat ["musfix", showVersion version, " git commit ", $(gitHash)])
                           (long "version" <> help "Show current version")

opts :: ParserInfo ProgramOptions
opts = info (helper <*> versionOption <*> cmdParser)
            (fullDesc <> progDesc "\nRun a fixpoint solver on INPUT FILE" <> header "Musfix")

main :: IO ()
main = do
  options <- execParser opts
  mapM_ (readConstraints options) $ inputFiles options

-- TODO add ProgramOptions as State
readConstraints :: ProgramOptions -> FilePath -> IO ()
readConstraints o f = do
    s <- ByteString.readFile f
    let lisp = topSExprs s
    let ins = prepareInputs $ topInputs lisp
    let qmap = generateQualifiers ins
    let horns = map formulas $ allHornConstraints ins
    let cs = map constant $ allConstants ins
    let ds = map distinct $ allDistincts ins
    prepOutput o
    verboseLog o $ "Preparing to find " ++ (fixPointType o) ++ " fixpoint..."
    verboseLog o $"\nInputs\n--------"
    verboseLog o $ "Reading from file " ++ f
    mapM_ ((verboseLog o) . show) ins
    verboseLog o $ "\nQMAP\n--------"
    verboseLog o $ show qmap
    verboseLog o $ "\nCandidates\n--------"
    let params = SolverInputs {
        useLeastFixpoint = leastFixpoint o,
        constraints = horns,
        qualifierMap = qmap,
        inConsts = cs,
        inDistinctConsts = ds
      }
    candidates <- findFixPoint params
    verboseLog o $ show candidates
    verboseLog o $ "\n\nFinal candidates: "
    mapM_ ((normalLog o) . show . pretty) candidates
    normalLog o $ "\n"

prepOutput :: ProgramOptions -> IO ()
prepOutput o = unless (length (outputFile o) == 0 || appendOutput o) $ writeFile (outputFile o) "" -- ^ Wipe the outputFile

verboseLog :: ProgramOptions -> String -> IO ()
verboseLog o s = when (verboseLogging o) $ normalLog o s

normalLog :: ProgramOptions -> String -> IO ()
normalLog o s = do
  unless (suppressOutput o) $ putStrLn s
  when (length (outputFile o) > 0) $ appendFile (outputFile o) $ "\n" ++ s

formulas :: InputExpr -> Formula
formulas (HornConstraint vs f) = f
formulas _ = error "non-horn constraint in constraints"

constant :: InputExpr -> (Id, Sort)
constant (ConstantDecl n s) = (n, s)
constant _ = error "non-constant expression in constants"

distinct :: InputExpr -> [Id]
distinct (DistinctDecl ns) = ns
distinct _ = error "non-distinct expression in distincts"

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

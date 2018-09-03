module Test where

import Main hiding (main)

import System.FilePath

import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = do
  putStrLn "why?"
  defaultMain =<< goldenTests

-- crash, cut, elim, minimize, neg, pos, proof, sample, todo]

goldenTests :: IO TestTree
goldenTests = do
  let progOpts = ProgramOptions {
    printOutput    = True,
    outputFile     = "tmp_output",
    verboseLogging = True,
    leastFixpoint  = False
  }
  smt2Files <- findByExtension [".smt2"] "test/."
  putStrLn "<3"
  mapM_ putStrLn smt2Files
  return $ testGroup "Smt2 golden tests" [
    goldenVsFile (takeBaseName smt2File) (replaceExtension smt2File ".golden")
      "tmp_output" $ readConstraints progOpts smt2File
      | smt2File <- smt2Files
    ]

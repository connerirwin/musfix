module Harness where

import Main hiding (main)

import System.Directory
import System.FilePath

import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = defaultMain =<< goldenTests

-- crash, cut, elim, minimize, neg, pos, proof, sample, todo]
-- TODO instead of writing to file, create internal string
goldenTests :: IO TestTree
goldenTests = do
    smt2Files <- findByExtension [".smt2"] "test/."
    return $ testGroup "Smt2 golden tests" [
      goldenVsFile (takeBaseName smt2File) (replaceExtension smt2File ".golden")
        tmpOut $ action smt2File tmpOut
        | smt2File <- smt2Files,
        let tmpOut = replaceExtension smt2File ".tmp"
      ]
  where
    action :: FilePath -> FilePath -> IO ()
    action inFile outFile = do
      readConstraints (ProgramOptions False False outFile True False) inFile
      -- removeFile outFile

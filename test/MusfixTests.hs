module Test.Harness where

import Main hiding (main)

import qualified Data.ByteString.Lazy as LBS

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
      goldenVsString (takeBaseName smt2File) (replaceExtension smt2File ".golden")
        $ action smt2File
        | smt2File <- smt2Files
      ]
  where
    action :: FilePath -> IO LBS.ByteString
    action inFile = do
      readConstraints (ProgramOptions False False "test_output.tmp" True False) inFile
      o <- LBS.readFile "test_output.tmp"
      removeFile "test_output.tmp"
      return o

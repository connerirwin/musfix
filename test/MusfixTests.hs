module Test.Harness where

import Main hiding (main)

import qualified Data.ByteString.Lazy as LBS

import System.Directory
import System.FilePath

import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = defaultMain =<< goldenTests

testDirs = ["crash", "cut", "elim", "minimize", "neg", "pos", "proof", "sample", "todo"]

goldenTests :: IO TestTree
goldenTests = do
  testGroups <- mapM mkTestGroup testDirs
  return $ testGroup "Smt2 Golden Tests" testGroups

mkTestGroup :: FilePath -> IO TestTree
mkTestGroup dir = do
  smt2Files <- findByExtension [".smt2"] $ "test/" ++ dir
  return $ testGroup dir [mkTest smt2File | smt2File <- smt2Files]

mkTest :: FilePath -> TestTree
mkTest f = goldenVsString (takeBaseName f) (replaceExtension f ".golden") $ action f

action :: FilePath -> IO LBS.ByteString
action inFile = do
  readConstraints (ProgramOptions False False "test_output.tmp" True False) inFile
  o <- LBS.readFile "test_output.tmp"
  removeFile "test_output.tmp"
  return o

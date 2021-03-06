-- | This performs golden tests on all files in a subfolder of the test
-- directory that end in .msmt. Since it uses the Tasty harness, additionally
-- testing strategies can be added fairly easily. For more details, see
-- http://hackage.haskell.org/package/tasty

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
  return $ testGroup "Golden Tests" testGroups

mkTestGroup :: FilePath -> IO TestTree
mkTestGroup dir = do
  testFiles <- findByExtension [".msmt"] $ "test/" ++ dir
  return $ testGroup dir [mkTest testFile | testFile <- testFiles]

mkTest :: FilePath -> TestTree
mkTest f = goldenVsString (takeBaseName f) (replaceExtension f ".golden") $ action f

action :: FilePath -> IO LBS.ByteString
action inFile = do
  let opts = ProgramOptions {
    inputFiles = [inFile],
    outputFile = "test_output.tmp",
    appendOutput = False,
    suppressOutput = True,
    verboseLogging = True,
    leastFixpoint = False
  }
  readConstraints opts inFile
  o <- LBS.readFile "test_output.tmp"
  removeFile "test_output.tmp"
  return o

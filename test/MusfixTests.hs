module Test where

import Test.Tasty
import Test.Tasty.Golden

import Main hiding (main)

main :: IO ()
main = defaultMain tests

-- TODO add golden tests
tests:: TestTree
tests = testGroup "Tests" []
-- crash, cut, elim, minimize, neg, pos, proof, sample, todo]

-- crash = testGroup "Crash" [
--     testCase "Testing EqConstr0" $ assertEqual "Should output something" (return ()) (readConstraints defaultProgramOptions "test/crash/EqConstr0.smt2_")
--   ]

-- cut = testCase "Cut" $ assertEqual "" "asd" ""
-- elim = testCase "Elim" $ assertEqual "" "" ""
-- minimize = testCase "Minimize" $ assertEqual "" "" ""
-- neg = testCase "Neg" $ assertEqual "" "" ""
-- pos = testCase "Pos" $ assertEqual "" "" ""
-- proof = testCase "Proof" $ assertEqual "" "" ""
-- sample = testCase "Sample" $ assertEqual "" "" ""
-- todo = testCase "Todo" $ assertEqual "" "" ""

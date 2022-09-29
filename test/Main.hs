module Main (
  main
) where

import Test.HUnit
import System.Exit

basicTest :: Test
basicTest = TestCase $ assertEqual "Basic Test" (1 + 2) 3

main :: IO ()
main = do
  let tests = TestList [basicTest]

  result <- runTestTT tests
  if errors result + failures result == 0
  then exitSuccess
  else exitWith (ExitFailure 1)

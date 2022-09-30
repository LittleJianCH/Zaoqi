module Main (
  main
) where

import Test.HUnit
import System.Exit

import CoreTest ( coreTest )

basicTest :: Test
basicTest = TestCase $ assertEqual "Basic Test" (1 + 2) 3

main :: IO ()
main = do
  let tests = TestList [basicTest, coreTest]

  result <- runTestTT tests
  if errors result + failures result == 0
  then exitSuccess
  else exitWith (ExitFailure 1)

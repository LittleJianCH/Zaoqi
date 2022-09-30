module CoreTest (
  coreTest
) where

import Value
import Goal
import Substitution
import qualified Stream as S

import Test.HUnit
import qualified Data.Map as Map

bindTest :: Test
bindTest = TestCase $
  assertEqual "Bind Test" 
              (S.streamToList $ run (equal (Var "x") (Atom "a")) empty)
              [Map.fromList [("x", Atom "a")]]

coreTest :: Test
coreTest = TestList [bindTest]

module CoreTest (
  coreTest
) where

import Value
import Goal
import Substitution
import qualified Stream as S

import Test.HUnit
import qualified Data.Map as Map

bindTest1 :: Test
bindTest1 = TestCase $
  assertEqual "Bind Test 1" 
              (S.streamToList $ run (equal (Var "x") (Atom "a")) empty)
              [Map.fromList [("x", Atom "a")]]

bindTest2 :: Test
bindTest2 = TestCase $
  assertEqual "Bind Test 2"
              (S.streamToList $ run (equal (List [Var "x",  Var "y", Var "z"])
                                           (List [Atom "a", Atom "b", Atom "c"]))
                                    empty)
              [Map.fromList [("x", Atom "a"), ("y", Atom "b"), ("z", Atom "c")]]

failTest1 :: Test
failTest1 = TestCase $
  assertEqual "Fail Test 1" 
              (S.streamToList $ run (equal (Atom "a") (Atom "b")) empty)
              []

failTest2 :: Test
failTest2 = TestCase $
  assertEqual "Fail Test 2" 
              (S.streamToList . (`run` empty) $ 
                foldl1 conj [
                  equal (Var "x") (Atom "a")
                , equal (Var "y") (Atom "b")
                , equal (Var "x") (Var "y")])
              []

coreTest :: Test
coreTest = TestList [bindTest1, bindTest2, failTest1, failTest2]

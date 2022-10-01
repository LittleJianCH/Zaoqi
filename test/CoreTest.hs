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
    (S.streamToList $ run 
      (equal (List [Var "x",  Var "y", Var "z"])
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
  assertEqual "Fail Test 1" 
    (S.streamToList $ 
      run (equal (List [Var "a", Var "a"])
                 (List [Atom "x", Atom "y"])) empty)
    []

conjTest1 :: Test
conjTest1 = TestCase $
  assertEqual "Conj Test 1"
    (S.streamToList $ 
      run (conj (equal (Var "x") (Atom "a"))
                (equal (Var "y") (Atom "b")))
      empty)
    [Map.fromList [("x", Atom "a"), ("y", Atom "b")]]

conjTest2 :: Test
conjTest2 = TestCase $
  assertEqual "Conj Test 2" 
    (S.streamToList . (`run` empty) $ 
      foldl1 conj [
        equal (Var "x") (Atom "a")
      , equal (Var "y") (Atom "b")
      , equal (Var "x") (Var "y")])
    []

disjTest1 :: Test 
disjTest1 = TestCase $
  assertEqual "Disj Test 1"
    (S.streamToList $ 
      run (disj (equal (Var "x") (Atom "a"))
                (equal (Var "x") (Atom "b")))
          empty)
    [Map.fromList [("x", Atom "a")], Map.fromList [("x", Atom "b")]]

coreTest :: Test
coreTest = TestList [
  bindTest1, bindTest2, 
  failTest1, failTest2,
  conjTest1, conjTest2,
  disjTest1 ]

module Goal (
  Goal,
  run,
  equal,
  conj,
  disj,
  succeed,
  Goal.fail,
  always,
  never,
) where

import Value
import Substitution
import Stream

import Data.Maybe ( maybeToList )

type Goal = Substitution -> Stream Substitution

run :: Goal -> Substitution -> Stream Substitution
run = ($)

equal :: Value -> Value -> Goal
equal x y = maybeToStream . unify x y

succeed :: Goal
succeed = return

fail :: Goal
fail = const Nil

always :: Goal
always = disj succeed always

never :: Goal
never = never

conj :: Goal -> Goal -> Goal
conj g1 g2 sub = g1 sub >>= g2

disj :: Goal -> Goal -> Goal
disj g1 g2 sub = append (g1 sub) (Delay (g2 sub))

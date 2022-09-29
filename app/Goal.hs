module Goal (
  Goal,
  run,
  equal,
  conj,
  disj,
) where

import Value
import Substitution

import Data.Maybe ( maybeToList )

type Goal = Substitution -> [Substitution]

run :: Goal -> Substitution -> [Substitution]
run = ($)

equal :: Value -> Value -> Goal
equal x y = maybeToList . unify x y

succeed :: Goal
succeed = return

fail :: Goal
fail = const []

conj :: Goal -> Goal -> Goal
conj g1 g2 = concatMap g2 . g1

mixList :: [a] -> [a] -> [a]
mixList [] ys = ys
mixList xs [] = xs
mixList (x:xs) ys = x : mixList ys xs

disj :: Goal -> Goal -> Goal
disj g1 g2 sub = mixList (g1 sub) (g2 sub)
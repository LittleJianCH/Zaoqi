module Goal (
  Goal,
  run,
  equal,
  conj,
  disj,
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
  where maybeToStream :: Maybe a -> Stream a
        maybeToStream Nothing = Nil
        maybeToStream (Just x) = Cons x Nil

succeed :: Goal
succeed = return

fail :: Goal
fail = const Nil

conj :: Goal -> Goal -> Goal
conj g1 g2 sub = g1 sub >>= g2

disj :: Goal -> Goal -> Goal
disj g1 g2 sub = append (g1 sub) (g2 sub)
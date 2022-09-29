module Goal (

) where

import Value
import Substitution

import Data.Maybe

type Goal = Substitution -> [Substitution]

run :: Goal -> Substitution -> [Substitution]
run = ($)

equal :: Value -> Value -> Goal
equal x y sub = maybeToList $ unify x y sub
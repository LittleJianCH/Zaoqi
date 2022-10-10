module Substitution (
  Substitution,
  empty,
  walk,
  occurs,
  unify,
) where

import Value ( Value(..), usedName, valueId, valueConst )
import Utility ( (?), (|>) )
import Stream

import qualified Data.Map as Map
import Control.Monad ( foldM )

type Substitution = Map.Map String Value

empty :: Substitution
empty = Map.empty

freshen :: String -> [String] -> String
freshen str used =
  head [s | s <- iterate (++ "'") str, s `notElem` used]

walk :: Value -> Substitution -> Value
walk (Var x) sub = case Map.lookup x sub of
  Just v -> walk v sub
  Nothing -> Var x
walk val _ = val

occurs :: String -> Value -> Substitution -> Bool
occurs x y sub = case walk y sub of
  y'@(Var _) -> Var x == y'
  (List ys) -> any (\y -> occurs x y sub) ys
  (Tie name body) -> x /= name && occurs x body sub
  (App rator rand) -> occurs x rator sub || occurs x rand sub
  _ -> False

extend :: String -> Value -> Substitution -> [Substitution]
extend s v sub = occurs s v sub ? [] $ [Map.insert s v sub]

unify :: Value -> Value -> Substitution -> [Substitution]
unify x y sub =
  let x' = walk x sub
      y' = walk y sub
  in x' == y' ? [sub] $
    let used = usedName x' ++ usedName y' ++
               Map.keys sub ++ concatMap usedName (Map.elems sub)
    in case (x', y') of
      (Var xn, _) -> extend xn y' sub
      (_, Var yn) -> extend yn x' sub
      (List xs, List ys) ->
        length xs /= length ys ? [] $
          zip xs ys |> foldM (flip $ uncurry unify) sub
      (Tie xn xe, Tie yn ye) ->
        sub |> unify (Var xn) (Var yn) >>= unify xe ye
      (Tie _ _, _) -> unify x' (Tie "y" (App y' (Var "y"))) sub
      (_, Tie _ _) -> unify y' x' sub
      (App xrator xrand, App yrator yrand) ->
        sub |> unify xrator yrator >>= unify xrand yrand
      (App _ _, _) ->
        unify x' (App valueId y') sub ++
        unify x' (App (valueConst y') (Var $ freshen "t" used)) sub
      (_, App _ _) -> unify y' x' sub
      _ -> []

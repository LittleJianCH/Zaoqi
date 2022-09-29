module Substitution (
  Substitution,
  empty,
  walk,
  occurs,
  unify,
) where

import Value ( Value(List, Var) )
import Utility ( (?), (|>) )

import qualified Data.Map as Map
import Control.Monad (foldM)

type Substitution = Map.Map String Value

empty :: Substitution
empty = Map.empty

walk :: Value -> Substitution -> Value
walk (Var x) sub = case Map.lookup x sub of
  Just v -> walk v sub
  Nothing -> Var x
walk val _ = val

occurs :: String -> Value -> Substitution -> Bool
occurs x y sub = case walk y sub of
  y'@(Var _) -> Var x == y'
  (List ys) -> any (\y -> occurs x y sub) ys
  _ -> False 

extend :: String -> Value -> Substitution -> Maybe Substitution
extend s v sub = occurs s v sub ? Nothing $ Just (Map.insert s v sub)

unify :: Value -> Value -> Substitution -> Maybe Substitution
unify x y sub = 
  let x' = walk x sub
      y' = walk y sub
  in x' == y' ? Just sub $
     case (x', y') of
       (Var xn, _) -> extend xn y' sub
       (_, Var yn) -> extend yn x' sub
       (List xs, List ys) -> zip xs ys |> foldM (flip $ uncurry unify) sub 
       _ -> Nothing

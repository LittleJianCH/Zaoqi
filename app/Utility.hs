module Utility (
  (?),
  (|>),
) where

infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) bool a b = if bool then a else b

infixr 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

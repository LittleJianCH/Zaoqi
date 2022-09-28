module Value (
  Value(..),
) where

data Value = Var String
           | Atom String
           | List [Value]
  deriving (Eq, Show)

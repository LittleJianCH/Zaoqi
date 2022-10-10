module Value (
  Value(..),
) where

data Value = Var !String
           | Atom !String
           | List ![Value]
           | Tie !String !Value
           | App !Value !Value
  deriving (Eq, Show)

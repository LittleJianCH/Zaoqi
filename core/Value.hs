module Value (
  Value(..),
  usedName,
  valueId,
  valueConst,
) where

import Data.List

data Value = Var !String
           | Atom !String
           | List ![Value]
           | Tie !String !Value
           | App !Value !Value
  deriving (Eq, Show)

unique :: Ord a => [a] -> [a]
unique = head . group . sort

usedName :: Value -> [String]
usedName (Var name) = [name]
usedName (List list) = unique $ concatMap usedName list
usedName (Tie name body) = delete name $ usedName body
usedName (App rator rand) = unique $ usedName rator ++ usedName rand
usedName _ = []

valueId :: Value
valueId = Tie "x" (Var "x")

valueConst :: Value -> Value
valueConst a = Tie "x" (Var "a")

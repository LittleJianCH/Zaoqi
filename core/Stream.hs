{-# LANGUAGE DeriveFunctor, InstanceSigs #-}

module Stream (
  Stream(..),
  Stream.take,
  maybeToStream,
  streamToList,
  append,
) where

data Stream a = Nil | Cons !a !(Stream a) | Delay (Stream a)
  deriving Functor

take :: Int -> Stream a -> Stream a
take 0 _ = Nil
take n Nil = Nil
take n (Cons a s) = Cons a (Stream.take (n-1) s)
take n (Delay s) = Stream.take n s

append :: Stream a -> Stream a -> Stream a
append Nil t = t
append (Cons a s) t = Cons a (append s t)
append (Delay s) t = Delay (append t s)

maybeToStream :: Maybe a -> Stream a
maybeToStream Nothing = Nil
maybeToStream (Just x) = Cons x Nil

listToStream :: [a] -> Stream a
listToStream = foldr Cons Nil

streamToList :: Stream a -> [a]
streamToList Nil = []
streamToList (Cons a s) = a : streamToList s
streamToList (Delay s) = streamToList s

instance Show a => Show (Stream a) where
  show :: Show a => Stream a -> String
  show s = show $ streamToList s

instance Applicative Stream where
  pure :: a -> Stream a
  pure a = Cons a Nil

  (<*>) :: Stream (a -> b) -> Stream a -> Stream b
  Nil <*> _ = Nil
  (Cons f fs) <*> as = (f <$> as) `append` (fs <*> as)
  (Delay fs) <*> as = Delay (fs <*> as)

instance Monad Stream where
  (>>=) :: Stream a -> (a -> Stream b) -> Stream b
  Nil >>= f = Nil
  (Cons a as) >>= f = f a `append` (as >>= f)
  (Delay as) >>= f = Delay (as >>= f)

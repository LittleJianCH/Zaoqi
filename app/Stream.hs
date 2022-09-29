{-# LANGUAGE DeriveFunctor, InstanceSigs #-}

module Stream (
  Stream(..),
  Stream.take,
  takeAll,
  append,
) where

data Stream a = Nil | Cons !a !(Stream a) | Delay (Stream a)
  deriving Functor

take :: Int -> Stream a -> [a]
take 0 _ = []
take n Nil = []
take n (Cons a s) = a : Stream.take (n-1) s
take n (Delay s) = Stream.take n s

takeAll :: Stream a -> [a]
takeAll Nil = []
takeAll (Cons a s) = a : takeAll s
takeAll (Delay s) = takeAll s

append :: Stream a -> Stream a -> Stream a
append Nil t = t
append (Cons a s) t = Cons a (append s t)
append (Delay s) t = Delay (append t s)

instance Show a => Show (Stream a) where
  show :: Show a => Stream a -> String
  show s = show $ takeAll s

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

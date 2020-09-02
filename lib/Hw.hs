module Hw (Idt(..)) where

data Idt a = I a
  deriving (Show, Eq, Ord)

instance Functor Idt where
  fmap f (I a) = I $ f a

instance Applicative Idt where
  pure = I
  (I a) <*> (I b) = I $ a b

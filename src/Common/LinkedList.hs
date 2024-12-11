module Common.LinkedList where

import Prelude hiding (length)

data LinkedList a = Link
    {   value :: a
    ,   next :: LinkedList a
    } | ListEnd

cons :: LinkedList a -> LinkedList a -> LinkedList a
cons ListEnd m = m
cons (Link x n) m = Link x (cons n m)

instance Functor LinkedList where
    fmap :: (a -> b) -> LinkedList a -> LinkedList b
    fmap f (Link a n) = Link (f a) (f <$> n)
    fmap _ ListEnd = ListEnd

instance Applicative LinkedList where
  pure :: a -> LinkedList a
  pure x = Link x ListEnd
  (<*>) :: LinkedList (a -> b) -> LinkedList a -> LinkedList b
  (<*>) (Link f fn) (Link a an) = Link (f a) (fn <*> an)
  (<*>) _ _ = ListEnd

instance Monad LinkedList where
  (>>=) :: LinkedList a -> (a -> LinkedList b) -> LinkedList b
  (>>=) (Link a na) f = case f a of 
    ListEnd -> ListEnd
    (Link b _) -> Link b (na >>= f)
  (>>=) ListEnd _ = ListEnd

instance Semigroup (LinkedList a) where
  (<>) :: LinkedList a -> LinkedList a -> LinkedList a
  (<>) = cons

instance Monoid (LinkedList a) where
  mempty :: LinkedList a
  mempty = ListEnd


fromList :: [a] -> LinkedList a
fromList = foldr Link ListEnd

instance Foldable LinkedList where
  foldr :: (a -> b -> b) -> b -> LinkedList a -> b
  foldr f acc (Link a n) = f a (foldr f acc n)
  foldr _ acc ListEnd = acc

length :: LinkedList a -> Int
length ListEnd = 0
length (Link _ n) = 1 + length n
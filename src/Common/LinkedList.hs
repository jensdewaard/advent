module Common.LinkedList where

import Prelude hiding (length)
import Common.Prelude (HasLength (..), MonadOrd ((>==)))

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

instance HasLength (LinkedList a) where
  len :: LinkedList a -> Int
  len ListEnd = 0
  len (Link _ n) = 1 + len n

applyF :: Ord a => (a -> LinkedList a) -> LinkedList a -> LinkedList a
applyF _ ListEnd = ListEnd
applyF f (Link a n) = f a <> applyF f n

instance Monad LinkedList where
  (>>=) :: LinkedList a -> (a -> LinkedList b) -> LinkedList b
  (>>=) (Link a na) f = case f a of 
    ListEnd -> ListEnd
    (Link b _) -> Link b (na >>= f)
  (>>=) ListEnd _ = ListEnd

instance MonadOrd LinkedList where
  (>==) = (>>=)
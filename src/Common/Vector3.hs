module Common.Vector3 (Vector3, vector3, x, y, z) where

data Vector3 a = Vector3 
    {   x :: a
    ,   y :: a
    ,   z :: a
    } deriving (Eq, Show)

vector3 :: a -> a -> a -> Vector3 a
vector3 = Vector3

instance Num a => Semigroup (Vector3 a) where
  (<>) :: Vector3 a -> Vector3 a -> Vector3 a
  (<>) v w = Vector3 (x v + x w) (y v + y w) (z v + z w)

instance Num a => Monoid (Vector3 a) where
  mempty :: Vector3 a
  mempty = Vector3 0 0 0

instance Functor Vector3 where
  fmap :: (a -> b) -> Vector3 a -> Vector3 b
  fmap f v = vector3 (f $ x v) (f $ y v) (f $ z v)

instance Applicative Vector3 where
  pure a = vector3 a a a
  (<*>) :: Vector3 (a -> b) -> Vector3 a -> Vector3 b
  (<*>) (Vector3 fa fb fc) (Vector3 a b c) = Vector3 (fa a) (fb b) (fc c)


instance Num a => Num (Vector3 a) where
  (+) :: Vector3 a -> Vector3 a -> Vector3 a
  (+) = plus
  (*) :: Vector3 a -> Vector3 a -> Vector3 a
  (*) = undefined
  abs :: Vector3 a -> Vector3 a
  abs v = Vector3 (abs $ x v) (abs $ y v) (abs $ z v)
  signum :: Vector3 a -> Vector3 a
  signum v = Vector3 (signum $ x v) (signum $ y v) (signum $ z v)
  fromInteger :: Integer -> Vector3 a
  fromInteger a = Vector3 (fromInteger a) 0 0
  negate :: Vector3 a -> Vector3 a
  negate v = Vector3 (negate $ x v) (negate $ y v) (negate $ z v)
    

plus :: Num a => Vector3 a -> Vector3 a -> Vector3 a
plus (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)
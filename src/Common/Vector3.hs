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
  pure :: a -> Vector3 a
  pure a = vector3 a a a
  (<*>) :: Vector3 (a -> b) -> Vector3 a -> Vector3 b
  (<*>) (Vector3 fa fb fc) (Vector3 a b c) = Vector3 (fa a) (fb b) (fc c)

instance Foldable Vector3 where
  foldMap :: Monoid m => (a -> m) -> Vector3 a -> m
  foldMap f (Vector3 a b c) = f a <> f b <> f c

instance Traversable Vector3 where
  traverse :: Applicative f => (a -> f b) -> Vector3 a -> f (Vector3 b)
  traverse f (Vector3 a b c) = Vector3 <$> f a <*> f b <*> f c

instance Num a => Num (Vector3 a) where
  (+) :: Vector3 a -> Vector3 a -> Vector3 a
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) :: Vector3 a -> Vector3 a -> Vector3 a
  (*) = liftA2 (*)
  abs :: Vector3 a -> Vector3 a
  abs v = abs <$> v
  signum :: Vector3 a -> Vector3 a
  signum v = signum <$> v
  fromInteger :: Integer -> Vector3 a
  fromInteger a = pure $ fromInteger a
  negate :: Vector3 a -> Vector3 a
  negate v = negate <$> v
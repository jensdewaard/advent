module FloydWarshall (Weight (Weight, Inf), floydwarshall) where

import Data.List (transpose)

data Weight = Weight Int | Inf deriving (Eq, Show)

instance Num Weight where
    (+) = add
    (*) = mult
    (-) = sub
    abs = absW
    signum = sign
    fromInteger = \x -> Weight (fromInteger x)

instance Ord Weight where compare = compareWeights

add :: Weight -> Weight -> Weight
add (Weight x) (Weight y) = Weight (x + y)
add _ _ = Inf

compareWeights :: Weight -> Weight -> Ordering
compareWeights Inf Inf = EQ
compareWeights _ Inf = LT
compareWeights Inf _ = GT
compareWeights (Weight x) (Weight y) = compare x y

sub :: Weight -> Weight -> Weight
sub (Weight x) (Weight y) = Weight (x - y)
sub _ _ = Inf

mult :: Weight -> Weight -> Weight
mult (Weight x) (Weight y) = Weight (x * y)
mult _ _ = Inf

absW :: Weight -> Weight
absW (Weight x) = Weight (Prelude.abs x)
absW Inf = Inf

sign :: Weight -> Weight
sign (Weight x) = Weight (signum x)
sign Inf = Inf


floydwarshall :: [[Weight]] -> [[Weight]]
floydwarshall m = snd (iterate step (0, m) !! length m)

step :: (Int, [[Weight]]) -> (Int, [[Weight]])
step (k, m) = (k + 1, zipWith (stepRow ktojs) istok m)
  where
    ktojs = m !! k  -- current k to each j
    istok = transpose m !! k  -- each i to current k

stepRow :: [Weight] -> Weight -> [Weight] -> [Weight]
stepRow ktojs itok itojs = zipWith stepOne itojs ktojs
  where
    stepOne itoj ktoj = itoj `min` (itok + ktoj)

module Common.Math (crt, mmi, josephus, biggestPot, biggestPowerOf) where

import Data.Maybe (fromJust)
import Data.List (find)

crt :: [(Int, Int)] -> Int
crt cs = let
    nums = map snd cs
    rems = map fst cs
    prod = product nums
    pps = map (\ni -> prod `div` ni) nums
    invs = zipWith mmi pps nums
    in sum (zipWith3 (\ ri ppi invi -> ri * ppi * invi) rems pps invs) `mod` prod

-- The modular multiplicative inverse
mmi :: Int -> Int -> Int
mmi a m = x where
    x = fromJust $ find (\k -> a*k `mod` m == 1) [1..m]

-- | The Josephus number of the input.
josephus :: Int -> Int
josephus n = let
    l = n - biggestPot n
    in 2 * l + 1

-- | The biggest power of two that is less than or equal to the input
biggestPot :: Int -> Int
biggestPot = biggestPowerOf 2

-- | biggestPowerOf y x.
--   The biggest power of y that is less than or equal to x.
biggestPowerOf :: Int -> Int -> Int
biggestPowerOf y x = bigg' 1 where
        bigg' c = if y * c > x then c else bigg' (y*c)

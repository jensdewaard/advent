module Common.Math (crt, mmi) where

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
module Common.Char (shift) where

import Data.Char (ord, chr, isUpper)

-- | Shift a character a number of places forward in the alphabet. A becomes B, B becomes C, Z becomes A, etc.
shift :: Int -> Char -> Char
shift n c = let 
    oc = ord c
    n' = n `mod` 26
    oc' = oc + n'
    u = if isUpper c then 90 else 122
    l = if isUpper c then 64 else 96
    in chr $ (oc' `mod` u) + (oc' `div` u) * l

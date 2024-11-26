{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Common.List (
  chunksOf,
  count,
  deleteAll,
  endsWith,
  findCycle,
  longest,
  moveElem,
  occur,
  prepend,
  replace,
  rotate,
  rotateR,
  splitOn,
  sumWith,
  swapElems,
  takeUntil,
  uninterleave,
  ) where

import Common.Prelude
import Data.List (delete, isPrefixOf)
import Data.Bifunctor (first, second)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n rs = take n rs : chunksOf n (drop n rs)

-- | Take elements from a list until the prefix list satisfies a predicate
--   or the source list is empty.
takeUntil :: Predicate [a] -> [a] -> [a]
takeUntil _ [] = []
takeUntil p as = go p [] as where
  go :: ([a] -> Bool) -> [a] -> [a] -> [a]
  go _ prefix [] = prefix
  go prd prefix (x:xs) = if prd prefix
    then prefix
    else go prd (prefix ++ [x]) xs

endsWith :: Eq a => a -> [a] -> Bool
endsWith a as = last as == a

sumWith :: Num b => (a -> b) -> [a] -> b
sumWith f =  sum . map f

-- | Count the number of elements in a list that satisfy a predicate.
count :: Predicate a -> [a] -> Int
count predicate = length . filter predicate

-- | The number of occurences of an element in a list.
occur :: Eq a => [a] -> [(a, Int)]
occur [] = []
occur (a:as) = let n = 1 + length (filter (==a) as) in
  (a, n) : occur (filter (a /=) as)

-- | Return the longest list. Returns the first if there are multiple lists of the same length.
longest :: [[a]] -> [a]
longest [] = error "longest on empty list"
longest ls = let l = maximum $ map length ls in head $ filter (\l' -> length l' == l) ls

prepend :: ([a],[b]) -> ([a],[b]) -> ([a],[b])
prepend (a,b) (as,bs) = (a ++ as, b ++ bs)

-- | Finds a cycle on a list. 
findCycle :: Eq a => [a] -> ([a],[a])
findCycle xxs = fCycle xxs xxs
  where fCycle (x:xs) (_:y:ys)
         | x == y              = fStart xxs xs
         | otherwise           = fCycle xs ys
        fCycle _      _        = (xxs,[]) -- not cyclic
        fStart (x:xs) (y:ys)
          | x == y              = ([], x:fLength x xs)
          | otherwise           = let (as,bs) = fStart xs ys in (x:as,bs)
        fStart _ _              = error "fStart called on empty lists"
        fLength x (y:ys)
         | x == y              = []
         | otherwise           = y:fLength x ys
        fLength _ _            = error "fLength called on empty list"

-- | Rotate a list n places to the left, e.g. 
--   rotate 2 ['#','#','#','@'] becomes 
--   ['#','@','#','#'].
rotate :: Int -> [a] -> [a]
rotate n xs = bs ++ as where (as, bs) = splitAt (n `mod` length xs) xs

-- | Rotate a list n places to the right, e.g. 
--   rotateR 2 ['#','@','#','#'] becomes 
--   ['#','#','#','@'].
rotateR :: Int -> [a] -> [a]
rotateR n xs = rotate (length xs - n) xs

deleteAll :: Eq a => [a] -> [a] -> [a]
deleteAll ds ls = foldl (flip delete) ls ds

uninterleave :: [a] -> ([a],[a])
uninterleave [] = ([],[])
uninterleave [a] = ([a],[])
uninterleave (a:as) = first (a :) as' where
  as' = uninterleave' as
  uninterleave' :: [a] -> ([a],[a])
  uninterleave' [] = ([],[])
  uninterleave' [b] = ([],[b])
  uninterleave' (b:bs) = second (b :) bs' where
    bs' = uninterleave bs

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn split list = case indexOf split list of
  Nothing -> [list]
  Just i  -> take i list : splitOn split (drop (i + length split) list)

-- | Returns the first index of the needle if contained in the haystack
--   and Nothing otherwise.
--   e.g. indexOf "foo" "hafoobar" = 2.
indexOf :: Eq a => [a] -> [a] -> Maybe Int
indexOf = indexOf' 0 where
    indexOf' :: Eq a => Int -> [a] -> [a] -> Maybe Int
    indexOf' _ _ [] = Nothing
    indexOf' index needle haystack = if needle `isPrefixOf` haystack
      then Just index
      else indexOf' (index+1) needle (tail haystack)

swapElems :: Int -> Int -> [a] -> [a]
swapElems i j xs
  | i > length xs = error "index i out of bounds"
  | j > length xs = error "index j out of bounds"
  | i == j = xs
  | i > j  = swapElems j i xs
swapElems i j xs = let
              elemI = xs !! i
              elemJ = xs !! j
              initial = take i xs
              middle = take (j - i - 1) $ drop (i+1) xs
              final = drop (j+1) xs
                   in initial ++ [elemJ] ++ middle ++ [elemI] ++ final

moveElem :: Int -> Int -> [a] -> [a]
moveElem i j s
  | i == j = s
moveElem x y s = let
        initial = take x s
        elemX = s !! x
        final = drop (x+1) s
        s' = initial ++ final
        initial' = take y s'
        final' = drop y s'
                     in initial' ++ [elemX] ++ final'

-- | Replaces the element at the given index of a list.
replace :: Int -> a -> [a] -> [a]
replace _ _ [] = error "replace: index out of bounds"
replace n r (a:as)
  | n < 0      = replace (negate n) a (reverse as)
  | n == 0     = r:as
  | otherwise  = a : replace (pred n) r as

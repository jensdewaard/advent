module Common.List (count, chunksOf, endsWith, longest, prepend, findCycle, takeUntil, occur, sumWith, rotate, rotateR, deleteAll) where

import Common.Prelude
import Data.List (delete)

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
count pred = length . filter pred

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
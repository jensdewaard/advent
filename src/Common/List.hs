{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Common.List
  ( chunksOf,
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
    aggregateOn,
    firstWhere,
    firstIndex,
    monotone,
    differences,
    subsets,
    none,
    sorted,
    sublist,
  )
where

import Common.Prelude
import Data.Bifunctor (first, second)
import Data.List (delete, isPrefixOf)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n rs = take n rs : chunksOf n (drop n rs)

-- | Take elements from a list until the prefix list satisfies a predicate
--   or the source list is empty.
takeUntil :: Predicate [a] -> [a] -> [a]
takeUntil _ [] = []
takeUntil p as = go p [] as
  where
    go :: ([a] -> Bool) -> [a] -> [a] -> [a]
    go _ prefix [] = prefix
    go prd prefix (x : xs) =
      if prd prefix
        then prefix
        else go prd (prefix ++ [x]) xs

endsWith :: (Eq a) => a -> [a] -> Bool
endsWith a as = last as == a

sumWith :: (Num b) => (a -> b) -> [a] -> b
sumWith f = sum . map f

-- | Count the number of elements in a list that satisfy a predicate.
count :: (Foldable f) => Predicate a -> f a -> Int
count predicate = foldr (\e n -> if predicate e then 1 + n else n) 0

-- | The number of occurences of an element in a list.
occur :: (Eq a) => [a] -> [(a, Int)]
occur [] = []
occur (a : as) =
  let n = 1 + length (filter (== a) as)
   in (a, n) : occur (filter (a /=) as)

-- | Return the longest list. Returns the first if there are multiple lists of the same length.
longest :: [[a]] -> [a]
longest [] = error "longest on empty list"
longest ls = let l = maximum $ map length ls in head $ filter (\l' -> length l' == l) ls

firstWhere :: (a -> Bool) -> [a] -> Maybe a
firstWhere _ [] = Nothing
firstWhere predicate (a : as) = if predicate a then Just a else firstWhere predicate as

firstIndex :: (a -> Bool) -> [a] -> Maybe Int
firstIndex _ [] = Nothing
firstIndex predicate as = go 0 (length as - 1)
  where
    go ptr1 ptr2
      | ptr2 < ptr1 = error "bounds have crossed"
      | ptr1 == ptr2 = if predicate (as !! ptr1) then Just ptr1 else Nothing
      | otherwise =
          if predicate (as !! halfway)
            then go ptr1 nextPoint
            else go nextPoint ptr2
      where
        halfway = ptr1 + ((ptr2 - ptr1) `div` 2)
        nextPoint
          | halfway == ptr1 = ptr1 + 1
          | halfway == ptr2 = ptr2 - 1
          | otherwise = halfway

prepend :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
prepend (a, b) (as, bs) = (a ++ as, b ++ bs)

-- | Finds a cycle on a list.
findCycle :: (Eq a) => [a] -> ([a], [a])
findCycle xxs = fCycle xxs xxs
  where
    fCycle (x : xs) (_ : y : ys)
      | x == y = fStart xxs xs
      | otherwise = fCycle xs ys
    fCycle _ _ = (xxs, []) -- not cyclic
    fStart (x : xs) (y : ys)
      | x == y = ([], x : fLength x xs)
      | otherwise = let (as, bs) = fStart xs ys in (x : as, bs)
    fStart _ _ = error "fStart called on empty lists"
    fLength x (y : ys)
      | x == y = []
      | otherwise = y : fLength x ys
    fLength _ _ = error "fLength called on empty list"

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

deleteAll :: (Eq a) => [a] -> [a] -> [a]
deleteAll ds ls = foldl (flip delete) ls ds

uninterleave :: [a] -> ([a], [a])
uninterleave [] = ([], [])
uninterleave [a] = ([a], [])
uninterleave (a : as) = first (a :) as'
  where
    as' = uninterleave' as
    uninterleave' :: [a] -> ([a], [a])
    uninterleave' [] = ([], [])
    uninterleave' [b] = ([], [b])
    uninterleave' (b : bs) = second (b :) bs'
      where
        bs' = uninterleave bs

splitOn :: (Eq a) => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn split list = case indexOf split list of
  Nothing -> [list]
  Just i -> take i list : splitOn split (drop (i + length split) list)

-- | Returns the first index of the needle if contained in the haystack
--   and Nothing otherwise.
--   e.g. indexOf "foo" "hafoobar" = 2.
indexOf :: (Eq a) => [a] -> [a] -> Maybe Int
indexOf = indexOf' 0
  where
    indexOf' :: (Eq a) => Int -> [a] -> [a] -> Maybe Int
    indexOf' _ _ [] = Nothing
    indexOf' index needle haystack =
      if needle `isPrefixOf` haystack
        then Just index
        else indexOf' (index + 1) needle (tail haystack)

swapElems :: Int -> Int -> [a] -> [a]
swapElems _ _ [] = []
swapElems i j xs
  | i == j = xs
  | i > j = swapElems j i xs
swapElems i j xs =
  let elemI = xs !! (i `mod` length xs)
      elemJ = xs !! (j `mod` length xs)
      initial = take i xs
      middle = take (j - i - 1) $ drop (i + 1) xs
      final = drop (j + 1) xs
   in initial ++ [elemJ] ++ middle ++ [elemI] ++ final

moveElem :: Int -> Int -> [a] -> [a]
moveElem i j s
  | i == j = s
moveElem x y s =
  let initial = take x s
      elemX = s !! x
      final = drop (x + 1) s
      s' = initial ++ final
      initial' = take y s'
      final' = drop y s'
   in initial' ++ [elemX] ++ final'

-- | Replaces the element at the given index of a list.
replace :: Int -> a -> [a] -> [a]
replace _ _ [] = error "replace: index out of bounds"
replace n r (a : as)
  | n < 0 = replace (negate n) a (reverse as)
  | n == 0 = r : as
  | otherwise = a : replace (pred n) r as

-- | Aggregates a list. If two successive elements are equal by the repr function, they are combined via the combine function.
aggregateOn :: (Eq b) => (a -> b) -> (a -> a -> a) -> [a] -> [a]
aggregateOn _ _ [] = []
aggregateOn _ _ [a] = [a]
aggregateOn repr combine (a : b : cs) =
  if repr a == repr b
    then aggregateOn repr combine (combine a b : cs)
    else a : aggregateOn repr combine (b : cs)

differences :: (Num a) => [a] -> [a]
differences ns = zipWith (-) (drop 1 ns) ns

monotone :: (Num a, Ord a) => [a] -> Bool
monotone ns = allDecreasing ns || allIncreasing ns

allDecreasing :: (Num a, Ord a) => [a] -> Bool
allDecreasing ns =
  let ds = zipWith (-) ns (drop 1 ns)
   in all (< 0) ds

allIncreasing :: (Num a, Ord a) => [a] -> Bool
allIncreasing ns =
  let ds = zipWith (-) ns (drop 1 ns)
   in all (> 0) ds

subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x : xs) = subsets xs ++ map (x :) (subsets xs)

none :: (a -> Bool) -> [a] -> Bool
none predicate = not . any predicate

-- | sorted is true if every element in the list is less than or equal to every successive element
sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (a : as) = all (a <=) as && sorted as

{- | Take a sublist of a list. If the second index is larger than the upper bound of the 
original list, it will as much elements as possible.
 -}
sublist :: Int -- ^ The index of the first desired element.
  -> Int -- ^ The index of the last desired element.
  -> [a] -- ^ The list to take a sublist from.
  -> [a]
sublist x y = take (y - x) . drop x

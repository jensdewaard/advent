module Common.List (chunksOf, endsWith, longest, prepend, findCycle) where
    
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n rs = take n rs : chunksOf n (drop n rs)

endsWith :: Eq a => a -> [a] -> Bool
endsWith a as = last as == a

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
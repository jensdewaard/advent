module Challenges.Y2016.Day13 (solutionA, solutionB) where

import Common.Coord (Coord, cardinal)
import Common.Parsing (int)
import Common.Prelude
import Common.Search (bfs, simple)
import Data.List (nub)

solutionA :: String -> String
solutionA = solve int (\fave -> simple (next (isOpen fave)) (1, 1) (== (31, 39)))

next :: (Int -> Int -> Bool) -> Coord -> [Coord]
next predicate = filter (uncurry predicate) . filter positive . cardinal

positive :: Coord -> Bool
positive (x, y) = x >= 0 && y >= 0

-- 269 is too high?!?
solutionB :: String -> String
solutionB = solve int (\fave -> length $ nub $ map fst $ bfs (next' (isOpen fave)) [((1, 1), 50)])

next' :: (Int -> Int -> Bool) -> (Coord, Int) -> [(Coord, Int)]
next' _ (c, 0) = [(c, 0)]
next' predicate (c, fuel) = map (,fuel - 1) $ next predicate c

formula :: Int -> Int -> Int
formula x y = x * x + 3 * x + 2 * x * y + y + y * y

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n
  | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
  | otherwise = toBin (n `div` 2) ++ [0]

isOpen :: Int -> Int -> Int -> Bool
isOpen favorite x y = even (length (filter (== 1) $ toBin (favorite + formula x y)))

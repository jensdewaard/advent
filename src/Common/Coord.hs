module Common.Coord (Coord, Dir(..), move, moveN, dist, above, below, belowN, left, right, rightN, direction, showMap, showMapWith, cardinal, reverse ) where

import Data.Bifunctor (second)
import Data.List (sort)
import Prelude hiding (reverse)

type Coord = (Int, Int)

data Dir = U | D| L| R deriving (Show, Eq, Ord)

-- | Move into a direction d from the given coordinate.
move :: Dir -> Coord -> Coord
move U c = above c
move D c = below c
move L c = left c
move R c = right c

moveN :: Int -> Dir -> Coord -> Coord
moveN n U c = aboveN n c
moveN n L c = leftN n c
moveN n R c = rightN n c
moveN n D c = belowN n c

dist :: Coord -> Coord -> Int
dist (px, py) (qx,qy) = abs (px - qx) + abs (py - qy)

above :: Coord -> Coord
above (x,y) = (x,y-1)

aboveN :: Int -> Coord -> Coord
aboveN n (x,y) = (x,y-n)

below :: Coord -> Coord
below = belowN 1

belowN :: Int -> Coord -> Coord
belowN n (x,y) = (x, y+n)

left :: Coord -> Coord
left (x,y) = (x-1, y)

leftN :: Int -> Coord -> Coord
leftN n (x,y) = (x-n,y)

right :: Coord -> Coord
right = rightN 1

rightN :: Int -> Coord -> Coord
rightN n (x,y) = (x+n, y)

direction :: Coord -> Coord -> Dir
direction x y
    | y == above x = U
    | y == below x = D
    | y == right x = R
    | y == left x = L
    | otherwise = error "coords not adjacent"

cardinal :: Coord -> [Coord]
cardinal c = [above c, right c, below c, left c]

diag :: Coord -> [Coord]
diag c = [above $ left c, above $ right c, below $ left c, below $ right c]

showMapWith :: (a -> Char) -> [(Coord,a)] -> String
showMapWith f m = showMap $ map (second f) m

showMap :: [(Coord, Char)] -> String
showMap m = unlines $ map (showLine ms) [1..maxY] where 
    ms = sort m
    maxY = maximum $ map (snd .fst) ms

showLine :: [(Coord, Char)] -> Int -> String
showLine m y = let xs = filter (\c -> snd (fst c) == y) m in map snd xs

reverse :: Dir -> Dir
reverse U = D
reverse L = R
reverse R = L
reverse D = U
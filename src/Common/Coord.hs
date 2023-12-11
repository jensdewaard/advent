module Common.Coord (Coord, Dir(..), move, dist, above, below, belowN, left, right, rightN, direction, showMap, showMapWith) where

import Data.Bifunctor (second)
import Data.List (sort)

type Coord = (Int, Int)

data Dir = U | D| L| R deriving (Show, Eq)

move :: Dir -> Coord -> Coord
move U c = above c
move D c = below c
move L c = left c
move R c = right c

dist :: Coord -> Coord -> Int
dist (px, py) (qx,qy) = abs (px - qx) + abs (py - qy)

above :: Coord -> Coord
above (x,y) = (x,y-1)

below :: Coord -> Coord
below = belowN 1

belowN :: Int -> Coord -> Coord
belowN n (x,y) = (x, y+n)

left :: Coord -> Coord
left (x,y) = (x-1, y)

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
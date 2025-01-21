{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Common.Coord (Coord, origin, Dir (..), move, moveN, dist, above, below, belowN, left, right, turnLeft, turnRight, rightN, direction, showMap, showMapWith, cardinal, reverse, area, diag) where

import Common.List (chunksOf)
import Data.Bifunctor (second)
import Data.List (maximumBy, sort, tails)
import Data.Ord (comparing)
import Prelude hiding (reverse)

type Coord = (Int, Int)

instance Num Coord where
  (+) :: Coord -> Coord -> Coord
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  (*) :: Coord -> Coord -> Coord
  (*) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
  abs :: Coord -> Coord
  abs (x, y) = (abs x, abs y)
  signum :: Coord -> Coord
  signum (x, y) = (signum x, signum y)
  fromInteger :: Integer -> Coord
  fromInteger n = (fromInteger n, fromInteger n)
  negate :: Coord -> Coord
  negate (x, y) = (negate x, negate y)

data Dir = U | D | L | R deriving (Show, Eq, Ord)

origin :: Coord
origin = (0, 0)

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

-- | Computes the Manhattan distance between two points.
dist :: Coord -> Coord -> Int
dist (px, py) (qx, qy) = abs (px - qx) + abs (py - qy)

above :: Coord -> Coord
above (x, y) = (x, y - 1)

aboveN :: Int -> Coord -> Coord
aboveN n (x, y) = (x, y - n)

below :: Coord -> Coord
below = belowN 1

belowN :: Int -> Coord -> Coord
belowN n (x, y) = (x, y + n)

left :: Coord -> Coord
left (x, y) = (x - 1, y)

leftN :: Int -> Coord -> Coord
leftN n (x, y) = (x - n, y)

right :: Coord -> Coord
right = rightN 1

rightN :: Int -> Coord -> Coord
rightN n (x, y) = (x + n, y)

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

showMapWith :: (a -> Char) -> [(Coord, a)] -> String
showMapWith f m = showMap $ map (second f) m

showMap :: [(Coord, Char)] -> String
showMap m = "\n" <> unlines (chunksOf width cs)
  where
    cs = map snd m
    width = fst $ fst $ maximumBy (comparing fst) m

reverse :: Dir -> Dir
reverse U = D
reverse L = R
reverse R = L
reverse D = U

turnLeft :: Dir -> Dir
turnLeft U = L
turnLeft L = D
turnLeft D = R
turnLeft R = U

turnRight :: Dir -> Dir
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

area :: (Integral a) => [(a, a)] -> a
area xs = sum [x1 * y2 - x2 * y1 | (y1, x1) : (y2, x2) : _ <- tails xs] `quot` 2

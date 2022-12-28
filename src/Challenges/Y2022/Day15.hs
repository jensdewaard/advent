#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle pkgs.heap ])"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Challenges.Y2022.Day15 where

import Data.Maybe
import Data.Set as S (Set, fromList, empty, union, filter, difference, size) 
import Data.Bifunctor (bimap)
import Shared
import Text.ParserCombinators.Parsec
import qualified IntervalSet as IS

main :: IO ()
main = do
    m <- readFile "inputs/15.txt"
    let Right c = parseInput (init m)
    --print $ solveA c
    print $ IS.add (IS.fromPair (1,5)) [IS.fromPair (6,9)]
    print $ IS.add (IS.fromPair (1,5)) [IS.fromPair (4,9)]
    print $ IS.add (IS.fromPair (4,5)) [IS.fromPair (1,3),IS.fromPair (6,9)]
    print $ solveB c

solveA :: [(Sensor, Beacon)] -> Int
solveA ps = S.size $ difference os bs where
    bs = fromList $ map snd ps 
    os = foldl union empty (map (fromList . uncurry (noneAtForY 2_000_000)) ps)

solveB :: [(Sensor, Beacon)] -> [IS.IntervalSet]
solveB ps =  foldl (zipWith IS.merge) [repeat IS.Empty] $ map setsForPair ps
    where s = fst . head $ ps
          b = snd . head $ ps
          ps = [((8,7), (2,10))]

-- data
type Sensor = Coord
type Beacon = Coord


{- Coords where a beacon is not, given a Y, a sensor and its closest beacon -}
noneAtForY :: Int -> Coord -> Coord -> [Coord]
noneAtForY y s@(sx, sy) b = let dist = distanceC s b
    in
    [(x, y) | x <- [sx - dist..sx+ dist], distanceC s (x,y) <= dist ]

setsForPair :: (Sensor, Beacon) -> [IS.IntervalSet]
setsForPair (s,b) = map (\y -> noneAtForY' y s b) [0..max_y]
    where max_y = 20

noneAtForY' :: Int -> Sensor -> Beacon -> IS.IntervalSet
noneAtForY' y s@(sx, sy) b = subs [0..max_x] ps where
    max_x = 20
    ps = [(x,y) | x <- [sx - dist..sx + dist], distanceC s (x,y) <= dist]
    dist = distanceC s b
    lbx = fst (last ps) + 1
    ubx = fst (head ps) - 1
    subs xs [] = [IS.fromPair (0, max_x)]
    subs xs ys
        | ubx < 0 && lbx > max_x = []
        | ubx < 0 = [IS.fromPair (lbx, max_x)]
        | lbx > max_x = [IS.fromPair (0, ubx)]
        | otherwise = IS.add (IS.fromPair (fst (last ys) + 1 ,max_x)) [IS.fromPair (0, fst (head ys)- 1)] 

noneAt :: Coord -> Coord -> Set Coord
noneAt s@(sx, sy) b = let dist = distanceC s b
    in
    fromList [(x, y) | x <- [sx - dist..sx+ dist], y <- [sy - dist..sy + dist], distanceC s (x,y) <= dist ]

tf :: Coord -> Int
tf (x, y) = x * 4_000_000 + y

searchSpace :: Set Coord
searchSpace = fromList [(x, y) | x <-  [0..4_000_000], y <- [0..4_000_000]]


-- parsing
parseInput :: String -> Either ParseError [(Sensor, Coord)]
parseInput = parse file "could not parse file" where
    file = line `sepBy` newline
    line = do
        string "Sensor at x="
        sx <- number
        string ", y="
        sy <- number
        string ": closest beacon is at x="
        bx <- number
        string ", y="
        by <- number
        return ((sx, sy), (bx, by))

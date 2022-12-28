#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

module Challenges.Y2022.Day08 where

import qualified Data.Text as T
import Data.Char
import Data.List
import Data.Tuple

main :: IO ()
main = do
    contents <- readFile "inputs/8.txt"
    let ls = map T.unpack $ T.splitOn (T.pack "\n") (T.pack contents)
    let m = readMap ls
    print $ length $ allVisible m
    let mx = length $ head m
    let my = length m - 1 -- not the empty line
    let cs = [(x, y) | x <- [0..length (head m) - 1], y <- [0..my -1]]
    let ss = map (\c -> (c, scenicScore m c)) cs
    print $ maxScore ((0,0), 0) ss

maxScore :: (Coord, Int) -> [(Coord, Int)] -> (Coord, Int)
maxScore p [] = p
maxScore (c, s) ((c', s'):cs) = maxScore m cs 
    where m = if s' > s then (c', s') else (c, s)

type TreeMap = [[Int]]
type Coord = (Int, Int)
data ViewDir = ViewTop | ViewLeft | ViewRight | ViewBottom

readLine :: String -> [Int]
readLine = map digitToInt

readMap :: [String] -> TreeMap
readMap = map readLine

allVisible :: TreeMap -> [Coord]
allVisible tm = nub $ visible ViewLeft tm 
    ++ visible ViewRight tm 
    ++ visible ViewTop tm 
    ++ visible ViewBottom tm

visible :: ViewDir -> TreeMap -> [Coord]
visible ViewLeft tm = concat $ zipWith (visibleAtY id) [1..length tm] tm
visible ViewRight tm = concat $ zipWith (visibleAtY reverse) [1..length tm] tm
visible ViewTop tm = map swap $ visible ViewLeft . transpose $ tm
visible ViewBottom tm = map swap $ visible ViewRight . transpose $ tm

visibleAtY :: ([Int] -> [Int]) -> Int -> [Int] -> [Coord]
visibleAtY f y l = visible' (f [1..length l]) (y-1) (-1) (f l)

visible' :: [Int] -> Int -> Int -> [Int] -> [Coord]
visible' _ _ _ [] = []
visible' (x : xs) y m (t : ts) = if t > m 
   then (x - 1, y) : visible' xs y t ts
   else visible' xs y m ts

scenicScore :: TreeMap -> Coord -> Int
scenicScore m c = visibleFrom ViewLeft m c 
    * visibleFrom ViewRight m c
    * visibleFrom ViewTop m c
    * visibleFrom ViewBottom m c

visibleFrom :: ViewDir -> TreeMap -> Coord -> Int
visibleFrom ViewRight m c = takeUntil h ts
    where
        ts = drop (fst c + 1) $ m !! snd c
        h = m !! snd c !! fst c
visibleFrom ViewLeft m c = takeUntil h ts
    where
        ts = drop (length l - fst c) l
        l = reverse (m !! snd c)
        h = m !! snd c !! fst c
visibleFrom ViewTop m c = visibleFrom ViewLeft (transpose m) (swap c) 
visibleFrom ViewBottom m c = visibleFrom ViewRight (transpose m) (swap c) 

takeUntil :: Int -> [Int] -> Int
takeUntil _ [] = 0
takeUntil h (t : ts) = if t >= h then 1 else 1 + takeUntil h ts

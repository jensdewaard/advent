#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

module Challenges.Y2022.Day09 where

import qualified Data.Text as T
import Data.List

main :: IO ()
main = do
    contents <- readFile "inputs/9.txt"
    let ms = concatMap readMove $ filter (/= "") $ map T.unpack (T.splitOn (T.pack "\n") (T.pack contents))
    let rope = ((0,0), [(0,0)])
    let tenKnotRope = ((0,0), replicate 9 (0,0))
    let ps = scanl doMove rope ms
    let ps' = scanl doMove tenKnotRope ms
    print $ length $ nub $ map (last. snd) ps
    print $ length $ nub $ map (last . snd) ps'


data Dir = Up | L | R | Down deriving (Show)
type Move = (Dir, Int)
type Rope = ((Int, Int), [(Int, Int)])

type Head = (Int, Int)
type Tail = [(Int,Int)]

readMove :: String -> [Move]
readMove ('U' : s) = replicate (read s) (Up, 1)
readMove ('L' : s) = replicate (read s) (L, 1)
readMove ('R' : s) = replicate (read s) (R, 1)
readMove ('D' : s) = replicate (read s) (Down, 1)

moveHead :: (Int, Int) -> Move -> (Int, Int)
moveHead (x, y) (Up, n) = (x, y - n)
moveHead (x, y) (Down, n) = (x, y + n)
moveHead (x, y) (L, n) = (x - n, y)
moveHead (x, y) (R, n) = (x + n, y)

doMove :: Rope -> Move -> Rope
doMove (h , t) m = (h', drop 1 $ moveRope (h': t))
    where h' = moveHead h m

moveRope :: [(Int, Int)] -> [(Int, Int)]
moveRope [] = []
moveRope [t] = [t]
moveRope (t: t' : ts) = if distance t t' > 1
    then t : t'' : drop 1 (moveRope (t'' : ts))
    else t : t' : ts
    where t'' = moveToward t t'

moveToward :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveToward p t = (fst t + signum (fst p - fst t), snd t + signum (snd p - snd t))

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x, y) (x', y') = max (abs (x' - x)) (abs (y' - y))

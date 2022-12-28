#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

module Challenges.Y2022.Day04 where

import qualified Data.Text as T
import Data.List

main :: IO ()
main = do
    contents <- readFile "inputs/4.txt"
    let ls = map T.unpack (T.splitOn (T.pack "\n") (T.pack contents))
    let as = map readPair ls
    let part1 = map hasOverlap as
    let part2 = map hasOverlapAtAll as
    print ("Overlapping pairs: " ++ show (countTrue part1))
    print ("Pairs overlapping at all: " ++ show (countTrue part2))

type Assignment = [Int]

readAssignment :: String -> Assignment
readAssignment s = [a..b] where
    ss = T.splitOn (T.pack "-") (T.pack s)
    a = readInt (T.unpack $ head ss)
    b = readInt (T.unpack $ last ss)

readPair :: String -> (Assignment, Assignment)
readPair "" = ([], [])
readPair s = (a, b) where
    ss = T.splitOn (T.pack ",") (T.pack s)
    a = readAssignment (T.unpack $ head ss)
    b = readAssignment (T.unpack $ last ss)

readInt :: String -> Int
readInt = read

hasOverlap :: (Assignment, Assignment) -> Bool
hasOverlap ([], []) = False
hasOverlap (a, b) = isSubsequenceOf a b || isSubsequenceOf b a

hasOverlapAtAll :: (Assignment, Assignment) -> Bool
hasOverlapAtAll (a, b) = f a b where
    f a b = not $ null (a `intersect` b)

countTrue :: [Bool] -> Int
countTrue [] = 0
countTrue bs = foldr countTrue' 0 bs

countTrue' :: Bool -> Int -> Int
countTrue' v a = if v then a + 1 else a

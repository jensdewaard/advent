#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

module Challenges.Y2022.Day06 where

import qualified Data.Text as T
import Data.List

main :: IO ()
main = do
    contents <- readFile "inputs/6.txt"
    --let ls = T.splitOn (T.pack "\n") (T.pack contents)
    print $ findStartPacket contents
    print $ findStartMessage contents

type Signal = [Char]

findStartPacket :: Signal -> Int
findStartPacket = findStart' 4 0

findStartMessage :: Signal -> Int
findStartMessage = findStart' 14 0

findStart' :: Int -> Int -> Signal -> Int
findStart' _ _ [] = error "no sequence of 4 unique characters"
findStart' l n cs = if unique $ take l cs then n + l else findStart' l (n+1) (drop 1 cs)

unique :: [Char] -> Bool
unique cs = length (nub cs) == length cs

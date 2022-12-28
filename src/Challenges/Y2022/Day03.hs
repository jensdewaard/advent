#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

module Challenges.Y2022.Day03 where

import qualified Data.Text as T
import Data.List
import Data.Char

main :: IO ()
main = do
    contents <- readFile "inputs/3.txt"
    let all = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPRQSTUVWXZY"
    let rs = map (readRucksack . T.unpack) (T.splitOn (T.pack "\n") (T.pack contents))
    let gs = chunks 3 (map allItems rs)
    let is = map inBoth rs
    let ps = map (map priority . nub) is
    let ps' = map sum ps
    let p = sum ps'
    let bs = map (nub . foldr intersect all) gs
    let pbs = concatMap (map priority) bs
    print ("Sum of priority: " ++ show p)
    print ("Priority of badges " ++ show (sum pbs))

type Item = Char
type Compartment = [Item]
type Rucksack = (Compartment, Compartment)

allItems :: Rucksack -> [Item]
allItems = uncurry (++)

readRucksack :: String -> Rucksack
readRucksack s = let l = length s in
    splitAt (l `div` 2) s

inBoth :: Rucksack -> [Item]
inBoth (l, r) = l `intersect` r



chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n rs = take n rs : chunks n (drop n rs)


priority :: Item -> Int
priority c = let v = ord c in
    if v <= 90 then v - 38 else v - 96

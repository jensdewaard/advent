#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle pkgs.heap ])"

module Challenges.Y2022.Day12 where

import Data.List
import Data.Char
import Data.Graph
import Data.Maybe
import DijkstraSimple as D

main :: IO ()
main = do
    m <- readFile "inputs/12.txt"
    let w = length . head $ lines m
    let m' = filter (/= '\n') m
    let coords = indexToCoordW w 
    let label = labelForW w
    let s = fromJust $ elemIndex 'S' m'
    let e = fromJust $ elemIndex 'E' m'
    --let s = label (0,0)
    --let e = label (1,2)
    let (g, nodeFromVertex, vertexFromKey) = 
            graphFromEdges [(n,n, a) | n <- [0..length m' - 1], let a = map label $ adjacentNodes m' w (coords n)]
    let coordsL = map coords [0..length m' - 1]
    --print g

    --print $ D.findShortestDistance g s e
    let as = s : [a | a <- [0..length m' - 1], m' !! a == 'a' ]
    let ds = map (\s -> D.findShortestDistance g s e) as
    --print ds
    print $ minimum ds

adjacentNodes :: String -> Int -> (Int, Int) -> [(Int, Int)]
adjacentNodes m w n = [p | p <- map (indexToCoordW w) [0..length m - 1], adjacent m w n p]

adjacent :: String -> Int -> (Int, Int) -> (Int, Int) -> Bool
adjacent s w p@(x,y) q@(x', y')
  | p == q = False
  | abs (x - x') > 1 = False
  | abs (y - y') > 1 = False
  | abs (x - x') == 1 && abs (y - y') == 1 = False
  | otherwise = hdiff (s !! labelForW w p) (s !! labelForW w q) <= 1

labelForW :: Int -> (Int, Int) -> Int
labelForW w (x,y) = y*w + x

indexToCoordW :: Int -> Int -> (Int, Int)
indexToCoordW w i = (mod i w, div i w)

hdiff :: Char -> Char -> Int
hdiff 'S' b = hdiff 'a' b
hdiff 'E' b = hdiff 'z' b
hdiff a 'S' = hdiff a 'a'
hdiff a 'E' = hdiff a 'z'
hdiff a b = ord b - ord a

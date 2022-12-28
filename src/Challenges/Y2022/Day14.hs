#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle pkgs.heap ])"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Challenges.Y2022.Day14 where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Shared
import Text.ParserCombinators.Parsec
import Numeric

main :: IO ()
main = do
    m <- readFile "inputs/14.txt"
    let Right c = parseInput (init m)
    let sandedCave = dropSand (lowestY c) c (500,0)
    --print $ solveA c
    print $ solveB c

solveA :: Cave -> Int
solveA c = countSand $ dropSand (lowestY c) c (500,0)

solveB :: Cave -> Int
solveB c = countSand $ dropSand' (lowestY c) c (500,0)

-- data
type Filling = (Coord, Char)
type Cave = [Filling]

dropSand :: Int -> Cave -> Coord -> Cave
dropSand y c p = let p' = nextPosition c p in
    if p == p' then dropSand y ((p, 'o') : c) (500,0) -- rests elsewhere
    else if snd p' > y then c
    else dropSand y c p'

dropSand' :: Int -> Cave -> Coord -> Cave
dropSand' y c p = let p' = nextPosition c p in
    if p' == (500,0) then (p', 'o') : c  -- came to rest at source
    else if p == p' then dropSand' y ((p, 'o') : c) (500,0) -- rests elsewhere
    else if snd p' > y then dropSand' y ((p', 'o') : c) (500,0)
    else dropSand' y c p'

nextPosition :: Cave -> Coord -> Coord
nextPosition c p@(px, py) = let
    p1 = (px, py+1)
    p2 = (px-1, py+1)
    p3 = (px+1, py+1) in
    if isNothing $ getF c p1 then p1 else
    if isNothing $ getF c p2 then p2 else
    if isNothing $ getF c p3 then p3 else p

countSand :: Cave -> Int
countSand = length . filter (=='o') . map snd

getF :: Cave -> Coord -> Maybe Filling
getF [] _ = Nothing
getF (f:fs) p = if fst f == p then Just f else getF fs p

-- A Y value at which, if the sand falls below this, it will
-- fall forever. (in part one)
lowestY :: Cave -> Int
lowestY = maximum . map (snd . fst)

-- parsing
parseInput :: String -> Either ParseError Cave
parseInput = parse file "could not parse file"

file :: GenParser Char st Cave
file = concat <$> path `sepBy` newline

path :: GenParser Char st [Filling]
path = map (, '#') . genPath <$> coords `sepBy` rightArrow

rightArrow :: GenParser Char st String
rightArrow = string " -> "

genPath :: [Coord] -> [Coord]
genPath [] = []
genPath [p] = [p]
genPath (s: e: ps) = if s == e then genPath (e:ps)
    else s : genPath (oneCloser s e : e : ps)

oneCloser :: Coord -> Coord -> Coord
oneCloser p@(px, py) (qx, qy)
    | px > qx = (px - 1, py)
    | px < qx = (px + 1, py)
    | py > qy = (px, py - 1)
    | py < qy = (px, py + 1)
    | otherwise = p

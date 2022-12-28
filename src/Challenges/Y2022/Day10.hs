#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

module Challenges.Y2022.Day10 where

import qualified Data.Text as T
import Control.Monad.State
import Data.List
import Shared

main :: IO ()
main = do
    contents <- readFile "inputs/10.txt"
    let cs = concatMap readInstruction $ toLines contents
    let ss = scanl runInstruction 1 cs
    print $ sum $ map (strengthAtCycle ss) [20, 60, 100, 140, 180, 220]
    putStr $ unlines $ map (zipWith draw [0..]) (chunks 40 ss)


data Instruction = Noop | Addx Int deriving Show

readInstruction :: String -> [Instruction]
readInstruction s
    | s == "noop" = [Noop]
    | "addx" `isPrefixOf` s = [Noop, Addx (read $ drop 4 s)]

runInstruction :: Int -> Instruction -> Int
runInstruction c Noop = c
runInstruction c (Addx n) = c + n

strengthAtCycle :: [Int] -> Int -> Int
strengthAtCycle ss c = (ss !! (c - 1)) * c

draw :: Int -> Int -> Char
draw i n = if abs (n - i) <= 1 then '#' else ' '

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n rs = take n rs : chunks n (drop n rs)

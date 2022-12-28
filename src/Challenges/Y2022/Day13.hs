#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle pkgs.heap ])"
{-# LANGUAGE OverloadedStrings #-}

module Challenges.Y2022.Day13 where

import Data.Char
import Data.List
import Data.Maybe
import Shared
import Text.ParserCombinators.Parsec
import qualified Data.Text as T
import Numeric

main :: IO ()
main = do
    m <- readFile "inputs/13.txt"
    let i = parseInput (init m)
    let p1 = List [List [Val 2]]
    let p2 = List [List [Val 6]]
    let Right ps = parseInput (init m)
    print $ solveA ps
    print $ solveB ps

solveA :: [(Packet, Packet)] -> Int
solveA = sum . map fst . filter snd . indexedList . map (uncurry (<=))

solveB :: [(Packet, Packet)] -> Int
solveB ps = succ iDiv * succ iDiv' where
    Just iDiv = elemIndex p1 aps
    Just iDiv' = elemIndex p2 aps
    p1 = List [List [Val 2]]
    p2 = List [List [Val 6]]
    aps = sort (p1 : p2 : concatMap (\p -> [fst p, snd p]) ps)

-- parsing
parseInput :: String -> Either ParseError [(Packet, Packet)]
parseInput = parse file "could not parse file"

file :: GenParser Char st [(Packet, Packet)]
file = sepBy pair (count 2 newline)

pair :: GenParser Char st (Packet, Packet)
pair = do
    l <- packet
    string "\n"
    r <- packet
    return (l, r)

packet :: GenParser Char st Packet
packet = (do 
        char '['
        p <- sepBy packet (char ',')
        char ']'
        return (List p)) <|> literal

literal :: GenParser Char st Packet
literal = Val . read <$> many1 digit

-- data
data Packet = Val Int | List [Packet] deriving (Eq, Read, Show)

instance Ord Packet where
    compare (Val x) (Val y) = compare x y
    compare (List []) (List []) = EQ
    compare (List []) _ = LT
    compare (List (x : xs)) (List []) = GT
    compare (Val x) (List ys) = compare (List [Val x]) (List ys)
    compare (List xs) (Val y) = compare (List xs) (List [Val y])
    compare (List (x:xs)) (List (y:ys)) = let c = compare x y in
        if c == EQ then compare xs ys else c

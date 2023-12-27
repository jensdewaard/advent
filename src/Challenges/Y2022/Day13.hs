{-# LANGUAGE OverloadedStrings #-}

module Challenges.Y2022.Day13 (solutionA, solutionB) where

import qualified Data.List as L
import Text.ParserCombinators.Parsec
import Data.Maybe (fromJust)
import Data.Either (fromRight)

solutionA :: String -> String
solutionA = show . solveA . fromRight [] . parseInput

solutionB :: String -> String
solutionB = show . solveB . fromRight [] . parseInput

solveA :: [(Packet, Packet)] -> Int
solveA = sum . map fst . filter snd . zip [1..] . map (uncurry (<=))

solveB :: [(Packet, Packet)] -> Int
solveB ps = succ iDiv * succ iDiv' where
    iDiv = fromJust $ L.elemIndex p1 aps
    iDiv' = fromJust $ L.elemIndex p2 aps
    p1 = List [List [Val 2]]
    p2 = List [List [Val 6]]
    aps = L.sort (p1 : p2 : concatMap (\p -> [fst p, snd p]) ps)

-- parsing
parseInput :: String -> Either ParseError [(Packet, Packet)]
parseInput = parse file "could not parse file"

file :: GenParser Char st [(Packet, Packet)]
file = sepBy pair (count 2 newline)

pair :: GenParser Char st (Packet, Packet)
pair = do
    l <- packet
    _ <- string "\n"
    r <- packet
    return (l, r)

packet :: GenParser Char st Packet
packet = (do
        _ <- char '['
        p <- sepBy packet (char ',')
        _ <- char ']'
        return (List p)) <|> literal

literal :: GenParser Char st Packet
literal = Val . read <$> many1 digit

-- data
data Packet = Val Int | List [Packet] deriving (Eq, Read, Show)

instance Ord Packet where
    compare (Val x) (Val y) = compare x y
    compare (List []) (List []) = EQ
    compare (List []) _ = LT
    compare (List (_ : _)) (List []) = GT
    compare (Val x) (List ys) = compare (List [Val x]) (List ys)
    compare (List xs) (Val y) = compare (List xs) (List [Val y])
    compare (List (x:xs)) (List (y:ys)) = let c = compare x y in
        if c == EQ then compare xs ys else c

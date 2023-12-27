module Challenges.Y2022.Day10 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec

import Data.List (isPrefixOf)
import Common.List (chunksOf)
import Common.Prelude

solutionA :: String -> String
solutionA = solve parseInput (\cs -> let ss = scanl runInstruction 1 cs in sum $ map (strengthAtCycle ss) [20, 60, 100, 140, 180, 220])
solutionB :: String -> String
solutionB = solve parseInput (\cs -> let ss = scanl runInstruction 1 cs in unlines $ map (zipWith draw [0..]) (chunksOf 40 ss))

parseInput :: Parser [Instruction]
parseInput = concat <$> (do
    l <- manyTill anyChar (lookAhead newline)
    return $ readInstruction l
    ) `sepEndBy` newline

data Instruction = Noop | Addx Int deriving Show

readInstruction :: String -> [Instruction]
readInstruction s
    | s == "noop" = [Noop]
    | "addx" `isPrefixOf` s = [Noop, Addx (read $ drop 4 s)]
    | otherwise = error ("cannot read instruction " ++ s)

runInstruction :: Int -> Instruction -> Int
runInstruction c Noop = c
runInstruction c (Addx n) = c + n

strengthAtCycle :: [Int] -> Int -> Int
strengthAtCycle ss c = (ss !! (c - 1)) * c

draw :: Int -> Int -> Char
draw i n = if abs (n - i) <= 1 then '#' else ' '

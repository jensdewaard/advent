module Challenges.Y2023.Day06 where
import Text.ParserCombinators.Parsec
import Shared (mult, solve)

solutionA :: String -> String
solutionA = solve parseInput (mult . map solveRace)
solutionB :: String -> String
solutionB = solve parseRace solveRace

solveRace :: Race -> Int
solveRace (t, d) = length [x | x <- [0..t], (x * (t - x) - d) > 0]

dist :: Int -> Int -> Int
dist total held = held * (total - held)

parseRace :: Parser Race
parseRace = do
    _ <- string "Time:"
    skipMany space
    ts <- sepEndBy1 (many1 digit) spaces
    _ <- string "Distance:"
    skipMany space
    ds <- sepBy1 (many1 digit) spaces
    return (read $ concat ts, read $ concat ds)

type Race = (Int, Int)
parseInput :: Parser [Race]
parseInput = do
    _ <- string "Time:"
    skipMany space
    ts <- sepEndBy1 (many1 digit) spaces
    _ <- string "Distance:"
    skipMany space
    ds <- sepBy1 (many1 digit) spaces
    return $ zip (map read ts) (map read ds)
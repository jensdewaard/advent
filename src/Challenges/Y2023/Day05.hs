module Challenges.Y2023.Day05 where
import Text.ParserCombinators.Parsec

import Shared (solve)

solutionA :: String -> String
solutionA = solve parseInput (minimum . runPuzzle)

solutionB :: String -> String
solutionB = undefined

data Puzzle = Puzzle {
    seeds :: [Int],
    seedSoil :: Map,
    soilFertilizer :: Map,
    fertilizerWater :: Map,
    waterLight :: Map,
    lightTemp :: Map,
    tempHumidity :: Map,
    humiditiyLoc :: Map
} deriving (Show)

type Map = [Rule]
type Rule = (Int, Int, Int)

runPuzzle :: Puzzle -> [Int]
runPuzzle p = map (applyM $ humiditiyLoc p)
    $ map (applyM $ tempHumidity p)
    $ map (applyM $ lightTemp p)
    $ map (applyM $ waterLight p)
    $ map (applyM $ fertilizerWater p)
    $ map (applyM $ soilFertilizer p)
    $ map (applyM $ seedSoil p)
    $ seeds p

applyM :: Map -> Int -> Int
applyM m i = case foldl (foldRule i) Nothing m of
    Nothing -> i
    Just x -> x

foldRule :: Int -> Maybe Int -> (Int, Int, Int) -> Maybe Int
foldRule i Nothing (d, s, r) = if i >= s && i < (s + r) then Just $ d + (i - s) else Nothing
foldRule i (Just x) r = Just x

parseMap :: Parser Map
parseMap = do
    _ <- manyTill anyToken newline -- <text>:
    rules <- sepEndBy (do
        source <- many1 digit
        skipMany1 space
        dest <- many1 digit
        skipMany1 space
        size <- many1 digit
        return (read source, read dest, read size)
        ) newline
    _ <- optional newline
    return rules

parseInput :: Parser Puzzle
parseInput = do
    _ <- string "seeds: "
    seeds <- sepBy1 (many1 digit) (char ' ')
    newline >> newline
    ss <- parseMap
    sf <- parseMap
    fw <- parseMap
    wl <- parseMap
    lt <- parseMap
    th <- parseMap
    hl <- parseMap
    return $ Puzzle (map read seeds) ss sf fw wl lt th hl
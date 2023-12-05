module Challenges.Y2023.Day05 where
import Text.ParserCombinators.Parsec

import Shared (solve)
import qualified IntervalSet as Interval
import IntervalSet (Interval (Interval))

solutionA :: String -> String
solutionA = solve parseInput (minimum . map Interval.lb . runPuzzle)

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
type Rule = (Interval, Interval)

mkIntervals :: [Int] -> [Interval]
mkIntervals [] = []
mkIntervals (i:is) = Interval.fromPair (i,i) : mkIntervals is

runPuzzle :: Puzzle -> [Interval]
-- runPuzzle p = map (applyM $ seedSoil p)
--     $ mkIntervals $ seeds p
runPuzzle p = map (applyM $ humiditiyLoc p)
    $ map (applyM $ tempHumidity p)
    $ map (applyM $ lightTemp p)
    $ map (applyM $ waterLight p)
    $ map (applyM $ fertilizerWater p)
    $ map (applyM $ soilFertilizer p)
    $ map (applyM $ seedSoil p)
    $ mkIntervals $ seeds p

applyM :: Map -> Interval -> Interval
applyM m i = case foldl (foldRule i) Nothing m of
    Nothing -> i
    Just x -> x

foldRule :: Interval -> Maybe Interval -> (Interval, Interval) -> Maybe Interval
foldRule i Nothing (d, s) = if s `Interval.includes` i
    then Just $ Interval.fromPair ((Interval.lb d) + ((Interval.lb i) - (Interval.lb s)), (Interval.lb d) + ((Interval.lb i) - (Interval.lb s)) + Interval.length i)
    else Nothing
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
        return (Interval.fromPair (read source, read source + read size), Interval.fromPair(read dest, read dest + read size))
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
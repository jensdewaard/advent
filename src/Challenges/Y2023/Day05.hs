module Challenges.Y2023.Day05 where
import Text.ParserCombinators.Parsec

import Shared (solve, chunksOf)
import qualified IntervalSet as Interval
import IntervalSet (Interval (Interval))
import qualified Data.List as List
import Data.Maybe (fromJust)

solutionA :: String -> String
solutionA = solve parseInput (minimum . map Interval.lb . runPuzzle mkIntervals)

solutionB :: String -> String
solutionB = solve parseInput (minimum . map Interval.lb . runPuzzle mkIntervals')
--solutionB = solve parseInput (runPuzzle mkIntervals')

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
mkIntervals = map (\i -> Interval.fromPair (i,i))

mkIntervals' :: [Int] -> [Interval]
mkIntervals' = map (\[a,b] -> Interval.fromPair (a,a+b-1)) . chunksOf 2

runPuzzle :: ([Int] -> [Interval]) -> Puzzle -> [Interval]
-- runPuzzle p = map (applyM $ seedSoil p)
--     $ mkIntervals $ seeds p
runPuzzle f p = process p $ f $ seeds p 

process :: Puzzle -> [Interval] -> [Interval]
process p = concatMap (applyM $ humiditiyLoc p)
    . concatMap (applyM $ tempHumidity p)
    . concatMap (applyM $ lightTemp p)
    . concatMap (applyM $ waterLight p)
    . concatMap (applyM $ fertilizerWater p)
    . concatMap (applyM $ soilFertilizer p)
    . concatMap (applyM $ seedSoil p)

applyM :: Map -> Interval -> [Interval]
applyM map seeds = case foldl (foldRule map seeds) Nothing map of
    Nothing -> [seeds]
    Just x -> x

foldRule :: Map -> Interval -> Maybe [Interval] -> (Interval, Interval) -> Maybe [Interval]
foldRule m i Nothing (d, s) 
    -- "interval i is a subset of interval s"
    | s `Interval.includes` i = 
        Just $ List.singleton $ Interval.fromPair ((Interval.lb d) + ((Interval.lb i) - (Interval.lb s)), (Interval.lb d) + ((Interval.lb i) - (Interval.lb s)) + Interval.length i)
    -- "interval s overlaps partly with the beginning of interval i"
    | s `Interval.overlaps` i = Just $ --error ("s: " ++ show s ++ "  i: " ++ show i)
        (fromJust $ foldRule m (Interval.fromPair (Interval.lb i, Interval.ub s)) Nothing (d, s)) ++
        applyM m (Interval.fromPair (Interval.ub s + 1, Interval.ub i))
    -- "interval i overlaps partly with the beginning of interval s"
    | i `Interval.overlaps` s = Just $ -- error ("(i: " ++ show i ++ "  s:  " ++ show s)
        applyM m (Interval.fromPair (Interval.lb i, Interval.lb s - 1)) ++
        (fromJust $ foldRule m (Interval.fromPair (Interval.lb s, Interval.ub i)) Nothing (d,s))
    -- "interval s is a subset of interval i"
    | i `Interval.includes` s = Just $
        applyM m (Interval.fromPair (Interval.lb i, Interval.lb s - 1)) ++
        [d] ++
        applyM m (Interval.fromPair (Interval.ub s + 1, Interval.ub i))
    | Interval.distinct i s = Nothing
    | otherwise = error ("unexpected interval relation " ++ show i ++ show s)
foldRule m i (Just x) r = Just x

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
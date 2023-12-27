module Challenges.Y2023.Day05 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec

import Common.Interval (Interval (..), Comparison (..), compare)
import Data.Maybe (fromJust, fromMaybe)
import Prelude hiding (compare,LT,GT)
import Common.Prelude
import Common.List (chunksOf)
import qualified Common.Interval as Interval
import qualified Data.List as List

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
-- process p = concatMap (applyM $ seedSoil p)
process p = concatMap (applyM $ humiditiyLoc p)
    . concatMap (applyM $ tempHumidity p)
    . concatMap (applyM $ lightTemp p)
    . concatMap (applyM $ waterLight p)
    . concatMap (applyM $ fertilizerWater p)
    . concatMap (applyM $ soilFertilizer p)
    . concatMap (applyM $ seedSoil p)

applyM :: Map -> Interval -> [Interval]
applyM m ss = fromMaybe [ss] (foldl (foldRule m ss) Nothing m)

foldRule :: Map -> Interval -> Maybe [Interval] -> (Interval, Interval) -> Maybe [Interval]
foldRule m i Nothing (d, s) = case s `compare` i of
    OUT ->
        Just $ List.singleton $ Interval.fromPair (Interval.lb d + (Interval.lb i - Interval.lb s), Interval.lb d + (Interval.lb i - Interval.lb s) + Interval.length i)
    LTE -> Just $ 
        fromJust (foldRule m (Interval.fromPair (Interval.lb i, Interval.ub s)) Nothing (d, s)) ++
        applyM m (Interval.fromPair (Interval.ub s + 1, Interval.ub i))
    GTE -> Just $ 
        applyM m (Interval.fromPair (Interval.lb i, Interval.lb s - 1)) ++
        fromJust (foldRule m (Interval.fromPair (Interval.lb s, Interval.ub i)) Nothing (d,s))
    IN -> Just $
        applyM m (Interval.fromPair (Interval.lb i, Interval.lb s - 1)) ++
        [d] ++
        applyM m (Interval.fromPair (Interval.ub s + 1, Interval.ub i))
    LT -> Nothing
    GT -> Nothing
foldRule _ _ (Just x) _ = Just x

parseMap :: Parser Map
parseMap = do
    _ <- manyTill anyToken newline
    rules <- (do
        dest <- many1 digit
        skipMany1 space
        source <- many1 digit
        skipMany1 space
        size <- many1 digit
        return (Interval.fromPair (read dest, read dest + read size - 1), Interval.fromPair (read source, read source + read size - 1))
        ) `sepEndBy` newline
    _ <- optional newline
    return rules

parseInput :: Parser Puzzle
parseInput = do
    _ <- string "seeds: "
    sds <- sepBy1 (many1 digit) (char ' ')
    _ <- newline >> newline
    ss <- parseMap
    sf <- parseMap
    fw <- parseMap
    wl <- parseMap
    lt <- parseMap
    th <- parseMap
    Puzzle (map read sds) ss sf fw wl lt th <$> parseMap
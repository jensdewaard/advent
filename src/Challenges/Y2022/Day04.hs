module Challenges.Y2022.Day04 (solutionA, solutionB) where

import qualified Data.Text as T
import Data.List ( isSubsequenceOf, intersect )
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, newline, sepEndBy1, char)
import Common.Interval (Interval)
import qualified Common.Interval as Interval
import Common.Parsing (int)
import Control.Arrow ((>>>), Arrow ((&&&), (***)), first, second)
import Data.Tuple (swap)

solutionA :: String -> String
solutionA = solve parser (map (arr Interval.includes) >>> filter id >>> length)

solutionB :: String -> String
-- solutionB = solve parser (map (arr Interval.distinct) >>> filter not >>> length)
solutionB = solve parser length

parser :: Parser [(Interval Int, Interval Int)]
parser = pairInterval `sepEndBy1` newline where
    pairInterval :: Parser (Interval Int, Interval Int)
    pairInterval = do
        x1 <- int
        _ <- char '-'
        y1 <- int
        _ <- char ','
        x2 <- int
        _ <- char '-'
        y2 <- int
        return (Interval.fromPair (x1,y1), Interval.fromPair (x2,y2))

arr :: (Interval a -> Interval a -> Bool) -> (Interval a, Interval a) -> Bool
arr f = (id &&& swap) >>> uncurry f *** uncurry f >>> uncurry (||)

hasOverlap :: Interval Int -> Interval Int -> Bool
hasOverlap a b = Interval.includes a b || Interval.includes b a
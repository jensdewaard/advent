module Challenges.Y2015.Day09 (solutionA, solutionB) where

import Data.Graph.Inductive.Basic (undir)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (nub)
import Text.ParserCombinators.Parsec
import Shared (solve, mkLGraph, tsp, tspWith)

type City = String

cities :: [(City, City, Int)] -> [City]
cities cs = nub $ cities' cs where
    cities' ((x, y, _):cs') = x:y:cities' cs'
    cities' [] = []

cityGraph :: [(City, City, Int)] -> Gr City Int
cityGraph es = undir $ mkLGraph (cities es) es

solutionA :: String -> String
solutionA = solve parser (show . tsp . cityGraph)
solutionB :: String -> String
solutionB = solve parser (show . tspWith maximum . cityGraph)


parser :: Parser [(City, City, Int)]
parser = sepBy1 parseLine newline where
    parseLine = do
        c1 <- many1 letter
        _ <- string " to "
        c2 <- many1 letter
        _ <- string " = "
        d <- many1 digit
        return (c1, c2, read d)

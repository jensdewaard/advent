module Challenges.Y2015.Day09 (solutionA, solutionB) where

import Data.Graph.Inductive.Basic (undir)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (permutations, nub)
import Text.ParserCombinators.Parsec
import Shared (solve, mkLGraph, tsp, tspWith)

type City = String

cities :: [(City, City, Int)] -> [City]
cities cs = nub $ cities' cs where 
    cities' ((x, y, _):cs') = x:y:(cities' cs')
    cities' [] = []

getDist :: [(City, City, Int)] -> City -> City -> Int
getDist [] _ _ = error "empty edge list"
getDist ((c1, c2, d):es) c1' c2' = 
    if (c1 == c1' && c2 == c2') || (c1 == c2' && c2 == c1') 
    then d else getDist es c1' c2'

cityGraph :: [(City, City, Int)] -> Gr City Int
cityGraph es = undir $ mkLGraph (cities es) es

solutionA :: String -> String
solutionA = solve parser (show . tsp . cityGraph)
solutionB = solve parser (show . (tspWith maximum) . cityGraph)

totalDist :: [(City, City, Int)] -> [City] -> Int
totalDist [] _ = error "empty edge list"
totalDist _ [] = 0
totalDist _ [_] = 0
totalDist es (a:b:cs) = getDist es a b + totalDist es (b:cs)

parser :: Parser [(City, City, Int)]
parser = sepBy1 parseLine newline where
    parseLine = do
        c1 <- many1 letter
        _ <- string " to "
        c2 <- many1 letter
        _ <- string " = "
        d <- many1 digit
        return (c1, c2, read d)
        
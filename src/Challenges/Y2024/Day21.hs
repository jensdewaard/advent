module Challenges.Y2024.Day21 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, newline, sepEndBy, many1, alphaNum)
import Data.Map (Map)
import qualified Data.Map as Map
import Common.Coord (Coord)
import Common.List (sumWith, zipWithNext)
import Data.Foldable (minimumBy)
import Data.Function (on)

solutionA :: String -> String
solutionA = solve parser (solution 2)
solutionB :: String -> String
solutionB = solve parser (const "")

parser :: Parser [String]
parser = many1 alphaNum `sepEndBy` newline

solution :: Int -> [String] -> Int
solution depth = sumWith f 
  where 
    f c = findCost c depth numpaths * x
    x = undefined

numpad :: Map Coord Char
numpad = Map.fromList [
    ((0,0),'7'), ((1,0), '8'), ((2,0), '9'),
    ((0,1),'4'), ((1,1), '5'), ((2,1), '6'),
    ((0,2),'1'), ((1,2), '2'), ((2,2), '3'),
                 ((1,3), '0'), ((2,3), 'A')
  ]

numpaths :: Map (Char, Char) [String]
numpaths = allPaths numpad

dirPad :: Map Coord Char
dirPad = Map.fromList [
                  ((1,0), '^'), ((2,0), 'A'),
    ((0,1), '<'), ((1,1), 'v'), ((2,1), '>')
  ]

dirpaths :: Map (Char, Char) [String]
dirpaths = allPaths dirPad

allPaths :: Map Coord Char -> Map (Char, Char) [String]
allPaths m = let
  keys = Map.keys m
  paths = [ ((m Map.! x, m Map.! y),findShortestPath m x y) | x <- keys, y <- keys ] 
  in Map.fromList paths

findShortestPath :: Map Coord Char -> Coord -> Coord -> [String]
findShortestPath m start end = undefined

diff :: Coord -> Coord -> Char
diff p q = case p - q of
  (0,1) -> '^'
  (0,-1) -> 'v'
  (-1,0) -> '<'
  (1,0) -> '>'
  _ -> error "invalid direction"


-- TODO memoize this function
findCost :: String -> Int -> Map (Char, Char) [String] -> Int
findCost code depth transitions = sumWith f $ zipWithNext ('A' : code)
  where 
    f :: (Char, Char) -> Int
    f x = let ps = transitions Map.! x 
     in case depth of
        0 -> length $ minimumBy (compare `on` length) ps
        _ -> length $ minimumBy (compare `on` \p -> findCost p (depth - 1) dirpaths) ps
  

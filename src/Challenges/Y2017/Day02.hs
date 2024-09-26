module Challenges.Y2017.Day02 (solutionA, solutionB) where

import Common.Coord
import Common.Parsing (grid, int)
import Common.Prelude
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = solve parser (sum . map diff . rows)

solutionB :: String -> String
solutionB = solve parser (sum . concatMap divisors . rows)

parser :: Parser (Map Coord Int)
parser =
  grid
    ( do
        i <- int
        _ <- optional spaces
        return i
    )

rows :: Map Coord Int -> [[Int]]
rows m =
  let maxY = maximum $ map (\((_, y), _) -> y) ls
      ls = Map.assocs m
      f r ((_, y), _) = y == r
   in map (\r -> map snd $ filter (f r) ls) [1 .. maxY]

diff :: [Int] -> Int
diff ns = maximum ns - minimum ns

divisors :: [Int] -> [Int]
divisors ns = [c | a <- ns, b <- ns, let c = a `div` b, a `mod` b == 0, a /= b]

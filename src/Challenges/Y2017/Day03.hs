module Challenges.Y2017.Day03 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Common.Parsing (int)
import Control.Arrow ((>>>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Common.Coord

solutionA :: String -> String
solutionA = solve parser ((\ n -> day3Gen !! pred n) >>> dist origin)
solutionB :: String -> String
solutionB = solve parser (const "")

level :: Int -> Int
level x  = last [ n | n <- [1,3..x], odd n, (n ^ 2) <= x ]

parser :: Parser Int
parser = int

directions = [ move R, move U, move L, move D ]

day3Gen = scanl (\c f -> f c) origin $ concat $ zipWith replicate steps (cycle directions)
    where
        steps = concat $ zipWith (\a b -> [a,b]) [1..] [1..]

-- getValue :: Num a => Coord -> Map Coord a -> a
-- getValue position table = sum $ mapMaybe (\f -> M.lookup (move f position) table) dir
--     where 
--         dir = directions ++ [\(a,b) -> (a + 1, b + 1), \(a,b) -> (a - 1, b + 1), \(a,b) -> (a + 1, b - 1), \(a,b) -> (a - 1, b - 1)]

-- setValue table coord = 
--     let x = getValue coord table
--     in (Map.insert coord x table, x)

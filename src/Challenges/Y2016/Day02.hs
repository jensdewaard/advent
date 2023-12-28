module Challenges.Y2016.Day02 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Data.Map (Map)
import qualified Data.Map as Map
import Common.Coord hiding (reverse)
import Common.Parsing (dir)

solutionA :: String -> String
solutionA = solve parser (map (worldA Map.!) . tail . scanl (foldl (move' worldA)) (1,1))
solutionB :: String -> String
solutionB = solve parser (map (worldB Map.!) . tail . scanl (foldl (move' worldB)) (0,2))

move' :: Map Coord Char -> Coord -> Dir -> Coord
move' world c d = let c' = move d c in
    if Map.member c' world then c' else c

worldA :: Map Coord Char
worldA = Map.fromList [ ((0,0), '1'), ((1,0), '2'), ((2,0), '3'),
                       ((0,1), '4'), ((1,1), '5'), ((2,1), '6'),
                       ((0,2), '7'), ((1,2), '8'), ((2,2), '9')]

worldB :: Map Coord Char
worldB = Map.fromList         [ ((2,0), '1'),
                  ((1,1), '2'), ((2,1), '3'), ((3,1), '4'),
    ((0,2), '5'), ((1,2), '6'), ((2,2), '7'), ((3,2), '8'), ((4,2), '9'),
                  ((1,3), 'A'), ((2,3), 'B'), ((3,3), 'C'),
                                ((2,4), 'D')
    ]

parser :: Parser [[Dir]]
parser = many1 dir `sepEndBy` newline
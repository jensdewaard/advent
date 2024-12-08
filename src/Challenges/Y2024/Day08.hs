module Challenges.Y2024.Day08 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, alphaNum, char)
import Control.Arrow ((>>>))
import Data.Map (Map)
import qualified Data.Map as M
import Common.Coord (Coord)
import Common.Parsing (grid)
import Data.List (nub)
import Control.Monad (join)
import Control.Applicative ((<|>))

solutionA :: String -> String
solutionA = solve parser (antinodes [1,-2] >>> length)
solutionB :: String -> String
solutionB = solve parser (antinodes [-100..100] >>> length)

antinodes :: [Integer] -> Map Coord Char -> [Coord]
antinodes is as = nub $ join [ms |
    (c1, a1) <- M.toList as,
    a1 /= '.',
    (c2, a2) <- M.toList as,
    a1 == a2,
    c1 /= c2,
    let ns = map (\k -> fromInteger k*(c1 - c2) + c1) is,
    let ms = filter (`M.member` as) ns
    ]

parser :: Parser (Map Coord Char)
parser = grid (alphaNum <|> char '.' <|> char '#')
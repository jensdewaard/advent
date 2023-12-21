module Challenges.Y2023.Day15 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve)
import Data.Char (ord)
import Parsing (symbol)

solutionA :: String -> String
solutionA = solve parser (sum . map (hash 0))
solutionB :: String -> String
solutionB = solve parser (const "")

hash :: Int -> String -> Int
hash current "" = current
hash current (c:cs) = let c' = (current + ord c) * 17 `mod` 256
    in hash c' cs

parser :: Parser [String]
parser = word `sepBy1` char ','
    where word = many1 (alphaNum <|> symbol)
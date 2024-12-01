module Challenges.Y2024.Day01 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Common.Parsing (int)
import Data.Bifunctor (bimap)
import Data.List (sort)
import Control.Arrow ((>>>))

solutionA :: String -> String
solutionA = solve parser (bimap sort sort >>> uncurry zip >>> map (uncurry diff) >>> sum)
solutionB :: String -> String
solutionB = solve parser ((\(ls, rs) -> map (\n -> (n,countIn rs n)) ls)
    >>> map (uncurry (*))
    >>> sum)


diff :: Int -> Int -> Int
diff l r = abs $ l - r

countIn :: [Int] -> Int -> Int
countIn ns x = length $ filter (==x) ns

parser :: Parser ([Int], [Int])
parser = unzip <$> row `sepEndBy1` newline where
    row :: Parser (Int, Int)
    row = do
        l <- int
        _ <- spaces
        r <- int
        return (l, r)

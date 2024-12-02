module Challenges.Y2022.Day03 (solutionA, solutionB) where

import Data.List ( intersect, nub )
import qualified Data.Map as M
import Common.Prelude (solve)
import Common.List (chunksOf)
import Text.ParserCombinators.Parsec (Parser, newline, sepEndBy1, many1, letter)
import Control.Arrow ( (>>>) )
import Data.Maybe (fromJust)

solutionA :: String -> String
solutionA = solve parser (map (inBoth >>> (nub >>> map priority >>> sum)) >>> sum)
solutionB :: String -> String
solutionB = solve parser (sum . concatMap (map priority . nub . foldr intersect (['a'..'z'] ++ ['A'..'Z'])) . chunksOf 3 . map allItems)

parser :: Parser [Rucksack]
parser = rucksack `sepEndBy1` newline where
    rucksack :: Parser Rucksack
    rucksack = do
        contents <- many1 letter
        let totalItems = length contents
        return (splitAt (totalItems `div` 2) contents)

type Item = Char
type Compartment = [Item]
type Rucksack = (Compartment, Compartment)

allItems :: Rucksack -> [Item]
allItems = uncurry (++)

inBoth :: Rucksack -> [Item]
inBoth (l, r) = l `intersect` r

priority :: Item -> Int
priority c = fromJust $ M.lookup c m where
    m = M.fromList (zip ['a'..'z'] [1..26] ++ zip ['A'..'Z'] [27..52])

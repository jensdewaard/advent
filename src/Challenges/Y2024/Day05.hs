{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE GADTs #-}
module Challenges.Y2024.Day05 (solutionA, solutionB, PageOrdering(..), UpdateItem(..)) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, char, sepEndBy1, newline, sepBy1)
import Control.Arrow ((>>>))
import Common.Parsing (int)
import Data.List (sort)
import Common.List (sorted)

solutionA :: String -> String
solutionA = solve parser (filter isSorted >>> map (value . middle) >>> sum)

isSorted :: Update -> Bool
isSorted (Update is) = sorted is

middle :: Update -> UpdateItem
middle (Update is) = let n = length is in (is !! max 0 (n `div` 2))

solutionB :: String -> String
solutionB = solve parser (filter (not . isSorted) >>> map (value . middle . sortU) >>> sum)

newtype PageOrdering where
  PageOrdering :: (Int, Int) -> PageOrdering
  deriving (Eq, Show)

newtype Update where
  Update :: {items :: [UpdateItem]} -> Update
  deriving (Show)

data UpdateItem = UpdateItem
    { value :: Int
    , orderings :: [PageOrdering]
    , originalPos :: Int
    }

instance Eq Update where
  (==) u1 u2 = items u1 == items u2

instance Eq UpdateItem where
    (==) x y = value x == value y

instance Ord UpdateItem where
  compare (UpdateItem v1 pos o1) (UpdateItem v2 _ o2) 
    | v1 == v2 = EQ
    | PageOrdering (v1,v2) `elem` pos = LT
    | PageOrdering (v2,v1) `elem` pos = GT
    | otherwise = compare o1 o2

instance Show UpdateItem where
    show :: UpdateItem -> String
    show = show . value

sortU :: Update -> Update
sortU (Update is) = Update $ sort is

pageorders :: Parser [PageOrdering]
pageorders = (do
    x <- int
    _ <- char '|'
    y <- int
    return (PageOrdering (x, y))) `sepEndBy1` newline

updates :: [PageOrdering] -> Parser [Update]
updates pos = (do
    is <- int `sepBy1` char ','
    let uis = map (`UpdateItem` pos) is
    return $ Update (zipWith (\f n -> f n) uis [1..]) )
    `sepEndBy1` newline

parser :: Parser [Update]
parser = do
    po <- pageorders
    _ <- newline
    updates po
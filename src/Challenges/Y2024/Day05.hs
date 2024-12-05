{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Challenges.Y2024.Day05 (solutionA, solutionB, compareUI, uiLE, PageOrdering(..), UpdateItem(..)) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, char, sepEndBy1, newline, sepBy1)
import Control.Arrow ((>>>), (&&&), first, second)
import Common.Parsing (int)
import Data.List (sortBy)

solutionA :: String -> String
solutionA = solve parser (first isSorted >>> uncurry filter >>> map (ui2Int . middle) >>> sum)

isSorted :: [PageOrdering] -> Update -> Bool
isSorted pos u = sortU pos u == u

middle :: Update -> UpdateItem
middle u = let n = length u in head $ drop (n `div` 2) u

ui2Int :: UpdateItem -> Int
ui2Int (UpdateItem n) = n

solutionB :: String -> String
solutionB = solve parser (first length >>> second (map length) >>> second (any even))

data PageOrdering = PageOrdering (UpdateItem,UpdateItem) deriving (Eq, Show)

data Update = Update
    { items :: [Int]
    , orderings :: [PageOrdering]
    } deriving (Show)

data UpdateItem = UpdateItem Int deriving (Eq, Show)

instance Eq Update where
  (==) u1 u2 = items u1 == items u2

instance Ord Update where
  compare u1 u2 = compareUI (orderings u1) (items u1) (items u2)

left :: PageOrdering -> UpdateItem
left (PageOrdering (u,_)) = u

sortU :: [PageOrdering] -> Update -> Update
sortU pos u = let u' = sortBy (compareUI pos) u in if u == u' then u' else sortU pos u'

right :: PageOrdering -> UpdateItem
right (PageOrdering (_,u)) = u

uiLE :: [PageOrdering] -> UpdateItem -> UpdateItem -> Bool
uiLE pos u1 u2 = PageOrdering (u1,u2) `elem` pos || any (uiLE pos u1 . left) (filter ((==u2) . right) pos)

compareUI :: [PageOrdering] -> UpdateItem -> UpdateItem -> Ordering
compareUI pos u1 u2
  | uiLE pos u1 u2 = LT
  | uiLE pos u2 u1 = GT
  | otherwise = EQ
--   | otherwise = error "incomparable"

pageorders :: Parser [PageOrdering]
pageorders = (do
    x <- int
    _ <- char '|'
    y <- int
    return (PageOrdering (UpdateItem x, UpdateItem y))) `sepEndBy1` newline

updates :: [PageOrdering] -> Parser [Update]
updates pos = (do
    is <- int `sepBy1` char ','
    let uis = is
    return $ Update uis pos )
    `sepEndBy1` newline

parser :: Parser [Update]
parser = do
    po <- pageorders
    _ <- newline
    updates po
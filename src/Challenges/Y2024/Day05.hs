{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Challenges.Y2024.Day05 (solutionA, solutionB, PageOrdering(..), UpdateItem(..)) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, char, sepEndBy1, newline, sepBy1)
import Control.Arrow ((>>>), (&&&), first, second)
import Common.Parsing (int)
import Data.List (sortBy, sort)

solutionA :: String -> String
solutionA = solve parser (filter isSorted >>> map (value . middle) >>> sum)

isSorted :: Update -> Bool
isSorted (Update is ) = sort is == is

middle :: Update -> UpdateItem
middle (Update is) = let n = length is in (is !! max 0 (n `div` 2))

solutionB :: String -> String
solutionB = solve parser (const "")

data PageOrdering = PageOrdering (Int,Int) deriving (Eq, Show)

data Update = Update
    { items :: [UpdateItem]
    } deriving (Show)

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
  compare = uiLE

uiLE :: UpdateItem -> UpdateItem -> Ordering
uiLE u1@(UpdateItem v1 pos o1) u2@(UpdateItem v2 _ o2) 
    | v1 == v2 = EQ
    | PageOrdering (v1,v2) `elem` pos = LT
    | PageOrdering (v2,v1) `elem` pos = GT
    | otherwise = compare o1 o2

instance Show UpdateItem where
    show :: UpdateItem -> String
    show = show . value

left :: PageOrdering -> Int
left (PageOrdering (u,_)) = u

-- sortU :: [PageOrdering] -> Update -> Update
-- sortU pos u = let u' = sortBy (compareUI pos) u in if u == u' then u' else sortU pos u'

right :: PageOrdering -> Int
right (PageOrdering (_,u)) = u

-- compareUI :: [PageOrdering] -> UpdateItem -> UpdateItem -> Ordering
-- compareUI pos u1 u2
--   | uiLE pos u1 u2 = LT
--   | uiLE pos u2 u1 = GT
--   | otherwise = EQ
--   | otherwise = error "incomparable"

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
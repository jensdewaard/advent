{-# LANGUAGE FlexibleInstances #-}

module Shared where

import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import qualified Parsing as P

solve :: String -> Parser a -> (a -> String) -> String
solve input parser f = f $ parse' parser input 

parse' :: Parser a -> String -> a
parse' p i = case parse p "advent" i of
                  Left err -> error ("could not run parser " ++ show err)
                  Right val -> val

toLines :: String -> [String]
toLines s = filter (/= "") $ map T.unpack $ T.splitOn (T.pack "\n") (T.pack s)

splitOn :: String -> String -> [String]
splitOn d s = map T.unpack $ T.splitOn (T.pack d) (T.pack s)

indexedList :: [a] -> [(Int, a)]
indexedList = indexedList' 1 where
    indexedList' n (a:as) = (n, a) : indexedList' (succ n) as
    indexedList' _ [] = []

-- Coords
class Coordinate a where
    predX :: a -> a
    succX :: a -> a
    predY :: a -> a
    succY :: a -> a
    xCoord :: a -> Int
    yCoord :: a -> Int

type Coord = (Int, Int)

instance Coordinate Coord where
    predX (x, y) = (pred x, y)
    succX (x, y) = (succ x, y)
    predY (x, y) = (x, pred y)
    succY (x, y) = (x, succ y)
    xCoord = fst
    yCoord = snd

data Dir = U | D| L| R deriving (Show, Eq)

move :: Coordinate a => Dir -> a -> a
move U c = succY c
move D c = predY c
move L c = predX c
move R c = succX c

coords :: GenParser Char st Coord
coords = do
    l <- number
    _ <- string ","
    r <- number
    return (l, r)

rectFromTo :: Coord -> Coord -> [Coord]
rectFromTo (x1,y1) (x2, y2) = concatMap (\x -> map (\y -> (x, y)) (enumFromTo y1 y2)) (enumFromTo x1 x2)

distanceC :: Coordinate a => a -> a -> Int
distanceC p q = abs (xCoord p - xCoord q) + abs (yCoord p - yCoord q)

number :: GenParser Char st Int
number = P.int

fromRight :: Either a b -> b
fromRight (Left _) = error "fromRight from Left"
fromRight (Right r) = r


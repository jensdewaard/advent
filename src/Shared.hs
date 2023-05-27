module Shared where

import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import qualified Parsing as P

toLines :: String -> [String]
toLines s = filter (/= "") $ map T.unpack $ T.splitOn (T.pack "\n") (T.pack s)

splitOn :: String -> String -> [String]
splitOn d s = map T.unpack $ T.splitOn (T.pack d) (T.pack s)

indexedList :: [a] -> [(Int, a)]
indexedList = indexedList' 1 where
    indexedList' n (a:as) = (n, a) : indexedList' (succ n) as
    indexedList' _ [] = []

-- Coords
type Coord = (Int, Int)

data Dir = U | D| L| R deriving (Show, Eq)

move :: Dir -> Coord -> Coord
move U (x, y) = (x, y + 1)
move D (x, y) = (x, y - 1)
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)

coords :: GenParser Char st Coord
coords = do
    l <- number
    _ <- string ","
    r <- number
    return (l, r)

distanceC :: Coord -> Coord -> Int
distanceC (px, py) (qx, qy) = abs (px - qx) + abs (py - qy)

number :: GenParser Char st Int
number = P.int

fromRight :: Either a b -> b
fromRight (Left _) = error "fromRight from Left"
fromRight (Right r) = r
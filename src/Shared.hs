module Shared where

import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import Data.Bifunctor
import Control.Monad (ap)
import qualified Parsing as P

toLines :: String -> [String]
toLines s = filter (/= "") $ map T.unpack $ T.splitOn (T.pack "\n") (T.pack s)

indexedList :: [a] -> [(Int, a)]
indexedList = indexedList' 1 where
    indexedList' n (a:as) = (n, a) : indexedList' (succ n) as
    indexedList' _ [] = []

-- Coords
type Coord = (Int, Int)

coords :: GenParser Char st Coord
coords = do
    l <- number
    string ","
    r <- number
    return (l, r)

distanceC :: Coord -> Coord -> Int
distanceC (px, py) (qx, qy) = abs (px - qx) + abs (py - qy)

number :: GenParser Char st Int
number = P.int

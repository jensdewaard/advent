{-# LANGUAGE FlexibleInstances #-}

module Shared where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (find, notElem)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Text.ParserCombinators.Parsec
import qualified Data.Text as T
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

mkLGraph :: (Eq a) => [a] -> [(a,a,b)] -> Gr a b
mkLGraph ns es = let
    nodes = map swap (indexNodes ns)
    mkEdge :: Eq a => [a] -> (a,a,b) -> (LEdge b)
    mkEdge ns (n1, n2, e) = (simpl $ find (fstEq n1) (indexNodes ns), simpl $ find (fstEq n2) (indexNodes ns), e)
    edges = map (mkEdge ns) es
    in mkGraph nodes edges

indexNodes :: [a] -> [(a,Int)]
indexNodes ns = zip ns [1..]

fstEq :: Eq a => a -> (a,b) -> Bool
fstEq y x = (fst x) == y

simpl :: Maybe (a, Int) -> Int
simpl = snd . fromJust

until1 :: (a -> Bool) -> (a -> a) -> a -> a
until1 p f x = until p f (f x)

hasPair :: Eq a => [a] -> Bool
hasPair [] = False
hasPair [a] = False
hasPair (a:a':as) = a == a' || hasPair (a':as)

hasTwoPair :: Eq a => [a] -> Bool
hasTwoPair [] = False
hasTwoPair [a] = False
hasTwoPair (a:a':as) = (a == a' && hasPair as) || hasTwoPair (a':as)
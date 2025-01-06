{-# LANGUAGE TypeFamilies #-}

module Challenges.Y2024.Day20 (solutionA, solutionB) where

import Common.Coord (Coord, cardinal, dist)
import Common.Parsing (grid)
import Common.Prelude (solve)
import Common.Search (Dijkstra (..), liftCost, searchFor)
import Control.Arrow ((&&&), (>>>))
import Data.Map (Map)
import Data.List (singleton)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec (Parser, alphaNum, char, (<|>))
import Data.Maybe (fromJust)
import Common.List (occur, sumWith)

solutionA :: String -> String
solutionA = solve parser (singleton >>> searchFor (\s -> position s == destination s)
    >>> fromJust
    >>> fst 
    >>> pathTaken
    >>> shortcuts
    >>> occur
    >>> filter ((>=100) . fst)
    >>> sumWith snd
    )

solutionB :: String -> String
solutionB = solve parser (const "")

data State = State
  { position :: Coord,
    pathTaken :: [Coord],
    destination :: Coord,
    walls :: Set Coord
  }
  deriving (Show)

instance Eq State where
  (==) s t = position s == position t

instance Ord State where
  compare = comparing position

instance Dijkstra State where
  type DijkstraRepr State = Coord
  type DijkstraCost State = Int
  represent = position
  adjacency = liftCost findPath
  estimate = const 1

findPath :: State -> [State]
findPath s =
  [ s {position = p, pathTaken = p : pathTaken s}
    | p <- cardinal (position s),
      p `notElem` walls s
  ]

shortcuts :: [Coord] -> [Int]
shortcuts cs = [ timeSaved
    | i1 <- [0..length cs - 1]
    , i2 <- [i1 .. length cs - 1]
    , i1 /= i2
    , dist (cs !! i1) (cs !! i2) == 2
    , let timeSaved = i2 - (i1 + 2)
    , timeSaved /= 2
    ]

parser :: Parser State
parser = do 
    g <- grid (alphaNum <|> char '#' <|> char '.')
    let ws = S.fromList $ map fst $ M.toList $ M.filter (=='#') g
    let s = fst $ head $ M.toList $ M.filter (=='S') g
    let e = fst $ head $ M.toList $ M.filter (=='E') g
    return $ State s [] e ws

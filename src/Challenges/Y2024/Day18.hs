{-# LANGUAGE TypeFamilies #-}

module Challenges.Y2024.Day18 (solutionA, solutionB) where

import Common.Coord (Coord, cardinal)
import Common.Parsing (int)
import Common.Prelude (solve)
import Common.Search (Dijkstra (..), searchFor)
import Control.Arrow ((&&&), (>>>))
import Data.List (singleton, inits)
import Text.ParserCombinators.Parsec (Parser, char, newline, sepEndBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust, isNothing)
import Common.List (firstIndex)

solutionA :: String -> String
solutionA = solve parser (take 1024 >>> initialState (70, 70) >>> singleton 
    >>> searchFor (\s -> pos s == destination s) 
    >>> fromJust 
    >>> snd)

solutionB :: String -> String
solutionB = solve parser (inits >>> map (initialState (70,70)) >>> firstIndex noPathExist)


noPathExist :: State -> Bool
noPathExist = isNothing . searchFor (\s -> pos s == destination s) . singleton

initialState :: Coord -> [Coord] -> State
initialState = State (0, 0) 0

data State = State
  { pos :: Coord,
    time :: Int,
    destination :: Coord,
    fallingBytes :: [Coord]
  }
  deriving (Show)

instance Eq State where
    (==) s t = pos s == pos t

instance Ord State where
  compare = comparing pos

instance Dijkstra State where
  type DijkstraRepr State = Coord
  type DijkstraCost State = Int
  represent = pos
  adjacency = move
  estimate = const 1

move :: State -> [(State, Int)]
move s =
  [ (State p' (time s + 1) (destination s) (fallingBytes s), 1)
    | p' <- cardinal (pos s),
      fst p' >= 0,
      snd p' >= 0,
      fst p' <= fst (destination s),
      snd p' <= snd (destination s),
      p' `notElem` fallingBytes s
  ]

-- hasFallenYet :: Int -> [(Int, Coord)] -> Bool

parser :: Parser [Coord]
parser =
  ( do
      x <- int
      _ <- char ','
      y <- int
      return (x, y)
  )
    `sepEndBy` newline

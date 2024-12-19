{-# LANGUAGE TypeFamilies #-}
module Challenges.Y2024.Day19 (solutionA, solutionB) where

import Common.Prelude (solve)
import Control.Arrow ((&&&), (>>>))
import Data.List (isPrefixOf, singleton)
import Text.ParserCombinators.Parsec (Parser, many, newline, sepBy, string, char, (<|>), many1, sepEndBy)
import Common.Search (Dijkstra (..), searchFor, searchFor')
import Data.Maybe (isJust)
import Common.List (count)

solutionA :: String -> String
solutionA = solve parser ((\(ts, ds) -> map (State ts []) ds) >>> map (searchFor (null . required) . singleton) >>> count isJust)

solutionB :: String -> String
solutionB = solve parser ((\(ts, ds) -> map (State ts []) ds) >>> map (searchFor' (null . required) . singleton) >>> map length)

data Color = W | U | B | R | G deriving (Eq, Ord, Show)

type Design = [Color]

type Towel = [Color]

data State = State
  { towels :: [Towel]
  , towelsUsed :: [Towel]
  , required :: Design
  } deriving (Eq, Ord, Show)

instance Dijkstra State where
    type DijkstraRepr State = ([Color], [Towel])
    type DijkstraCost State = Int
    represent s = (required s, towelsUsed s)
    adjacency = findNext
    estimate = const 1

findNext :: State -> [(State, Int)]
findNext s =
  [ (State (towels s) (t : towelsUsed s) r', negate (length t))
    | t <- towels s,
      t `isPrefixOf` required s,
      let r' = drop (length t) (required s)
  ]

parser :: Parser ([Towel], [Design])
parser = do
  ts <- many1 color `sepEndBy` string ", "
  _ <- newline >> newline
  ds <- many1 color `sepEndBy` newline
  return (ts, ds)
    where
        color = (char 'r' >> return R)
            <|> (char 'w' >> return W)
            <|> (char 'u' >> return U)
            <|> (char 'b' >> return B)
            <|> (char 'g' >> return G)


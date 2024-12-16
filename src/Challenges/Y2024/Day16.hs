{-# LANGUAGE TypeFamilies #-}

module Challenges.Y2024.Day16 (solutionA, solutionB) where

import Common.Coord (Coord, Dir (..), move, turnLeft, turnRight, dist)
import Common.Parsing (grid)
import Common.Prelude (solve)
import Common.Search (Dijkstra (..), searchFor, searchFor')
import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.List (singleton, nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import Text.ParserCombinators.Parsec (Parser, char)
import Data.Maybe (fromJust)
import Common.List (sumWith)

solutionA :: String -> String
solutionA = solve parser (reindeerStart >>> searchFor reindeerAtFinish)

solutionB :: String -> String
solutionB =
  solve parser (\m -> 
    let 
      start = reindeerStart m 
      shortestPathLength = length $ pathTaken $ fst $ fromJust $ searchFor reindeerAtFinish start
      allPaths = searchFor' reindeerAtFinish start
      shortestPaths = takeWhile ((==shortestPathLength) . length . pathTaken . fst) allPaths
    in length $ nub $ concatMap (pathTaken . fst) shortestPaths
    -- in shortestPaths
    )
-- solutionB = const ""

reindeerAtFinish :: Reindeer -> Bool
reindeerAtFinish r = M.lookup (position r) (maze r) == Just 'E'

finish :: Map Coord Char -> Coord
finish m = fst $ head $ M.toList $ M.filter (=='E') m

data Reindeer = Reindeer
  { position :: Coord,
    facing :: Dir,
    score :: Int,
    pathTaken :: [Coord],
    maze :: Map Coord Char
  }

instance Show Reindeer where
  show r = "(" <> show (position r) <> "," <> show (facing r) <> ")"

instance Eq Reindeer where
  (==) r s = position r == position s && facing r == facing s && score r == score s

instance Ord Reindeer where
  compare = comparing score <> comparing position <> comparing facing

reindeerStart :: Map Coord Char -> [Reindeer]
reindeerStart m = case M.toList $ M.filter (== 'S') m of
  [(c, 'S')] -> singleton $ Reindeer c R 0 [c] m
  _ -> error "could not find start reindeer"

instance Dijkstra Reindeer where
  type DijkstraRepr Reindeer = (Coord, Dir)
  type DijkstraCost Reindeer = Int
  represent :: Reindeer -> DijkstraRepr Reindeer
  represent r = (position r, facing r)
  adjacency :: Reindeer -> [(Reindeer, DijkstraCost Reindeer)]
  adjacency = possibleMoves
  estimate :: Reindeer -> DijkstraCost Reindeer
  estimate r = let w = maze r in dist (finish w) (position r)

possibleMoves :: Reindeer -> [(Reindeer, Int)]
possibleMoves r =
  [ (Reindeer p' (facing r) (score r + 1) (p' : pathTaken r) (maze r), 1)
    | let p' = move (facing r) (position r),
      M.lookup p' (maze r) /= Just '#' -- destination is not a wall
  ]
    ++ [ (Reindeer (position r) f' (score r + 1000) (pathTaken r) (maze r), 1000)
         | f' <- [turnLeft (facing r), turnRight (facing r)]
       ]

parser :: Parser (Map Coord Char)
parser = grid (char 'E' <|> char 'S' <|> char '#' <|> char '.')
{-# LANGUAGE TypeFamilies #-}

module Challenges.Y2024.Day16 (solutionA, solutionB) where

import Common.Coord (Coord, Dir (..), cardinal, move, turnLeft, turnRight)
import Common.Parsing (grid)
import Common.Prelude (solve)
import Common.Search (Dijkstra (..), bfs, dfs, searchFor)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&), (>>>))
import Data.List (nub, singleton)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Text.ParserCombinators.Parsec (Parser, char)

solutionA :: String -> String
solutionA = solve parser (reindeerStart >>> searchFor reindeerAtFinish)

solutionB :: String -> String
solutionB =
  solve
    parser
    ( \m ->
        let shortestPath = pathTaken $ fst $ fromJust $ searchFor reindeerAtFinish (reindeerStart m)
         in bfs couldReachEnd (map (fuelUp (length shortestPath)) $ reindeerStart m)
        -- in length shortestPath
    )

couldReachEnd :: FueledReindeer -> [FueledReindeer]
couldReachEnd r = [r'
    | p' <- cardinal (fuelPosition r)
    , M.lookup p' (fuelMaze r) /= Just '#'
    , let r' = FueledReindeer p' (fuel r - 1) (p' : fueledPath r) (fuelMaze r)
    , endReachable (fuel r - 1) r'
    ]

endReachable :: Int -> FueledReindeer -> Bool
endReachable 0 r = fueledReindeerAtFinish r
endReachable n r =
  any
    (endReachable (n - 1))
    [ FueledReindeer p' (fuel r - 1) [] (fuelMaze r)
      | p' <- cardinal (fuelPosition r),
        M.lookup p' (fuelMaze r) /= Just '#'
    ]

fuelUp :: Int -> Reindeer -> FueledReindeer
fuelUp f r = FueledReindeer (position r) f (pathTaken r) (maze r)

reindeerAtFinish :: Reindeer -> Bool
reindeerAtFinish r = M.lookup (position r) (maze r) == Just 'E'
fueledReindeerAtFinish :: FueledReindeer -> Bool
fueledReindeerAtFinish r = M.lookup (fuelPosition r) (fuelMaze r) == Just 'E'

data FueledReindeer = FueledReindeer
    { fuelPosition :: Coord
    , fuel :: Int
    , fueledPath :: [Coord]
    , fuelMaze :: Map Coord Char
    }

instance Eq FueledReindeer where
    (==) f g = fuelPosition f == fuelPosition g

instance Ord FueledReindeer where
    compare = comparing fuelPosition

instance Show FueledReindeer where
    show r = show (fuelPosition r) ++ "(" ++ show (fuel r) ++ ")"

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
  compare = comparing position <> comparing facing

reindeerStart :: Map Coord Char -> [Reindeer]
reindeerStart m = case M.toList $ M.filter (== 'S') m of
  [(c, 'S')] -> singleton $ Reindeer c R 0 [] m
  _ -> error "could not find start reindeer"

instance Dijkstra Reindeer where
  type DijkstraRepr Reindeer = (Coord, Dir)
  type DijkstraCost Reindeer = Int
  represent :: Reindeer -> DijkstraRepr Reindeer
  represent r = (position r, facing r)
  adjacency :: Reindeer -> [(Reindeer, DijkstraCost Reindeer)]
  adjacency = possibleMoves
  estimate :: Reindeer -> DijkstraCost Reindeer
  estimate = const 1

possibleMoves :: Reindeer -> [(Reindeer, Int)]
possibleMoves r =
  [ (Reindeer p' (facing r) (score r + 1) (p' : pathTaken r) (maze r), 1)
    | let p' = move (facing r) (position r),
      M.lookup p' (maze r) /= Just '#' -- destination is not a wall
  ]
    ++ [ (Reindeer (position r) f' (score r + 1000) (pathTaken r) (maze r), 1000)
         | f' <- [turnLeft (facing r), turnRight (facing r)]
       ]

possibleMovesNS :: Reindeer -> [Reindeer]
possibleMovesNS r =
  [ Reindeer p' (facing r) (score r + 1) (p' : pathTaken r) (maze r)
    | let p' = move (facing r) (position r),
      M.lookup p' (maze r) /= Just '#' -- destination is not a wall
  ]
    ++ [ Reindeer (position r) f' (score r + 1000) (pathTaken r) (maze r)
         | f' <- [turnLeft (facing r), turnRight (facing r)]
       ]

parser :: Parser (Map Coord Char)
parser = grid (char 'E' <|> char 'S' <|> char '#' <|> char '.')
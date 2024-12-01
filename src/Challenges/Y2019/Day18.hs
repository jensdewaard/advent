{-# LANGUAGE TypeFamilies #-}
module Challenges.Y2019.Day18 (solutionA, solutionB) where
import Common.Prelude ( solve )
import Text.ParserCombinators.Parsec
import Common.Coord (Coord, cardinal)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Char (isUpper, toLower, isLower)
import Control.Arrow ((>>>))
import Common.Search (searchFor, Dijkstra (..))
import Data.List (singleton)
import Data.Ord (comparing)
import Common.Parsing (symbol, grid)

solutionA :: String -> String
solutionA = solve parser (MazeState (41,41) S.empty >>> singleton >>> searchFor allKeysCollected)
solutionB :: String -> String
solutionB = solve parser (MazeStateB (40,40) (40,42) (42,40) (42,42) S.empty
    >>> show
    )

parser :: Parser (Map Common.Coord.Coord Char)
parser = grid (letter <|> symbol <|> char '@')

allKeysCollected :: MazeState -> Bool
allKeysCollected (MazeState _ ks _) =
    S.isSubsetOf (S.fromList ['a'..'z']) ks

nextMoves :: MazeState -> [(MazeState, Int)]
nextMoves (MazeState p ks w) = let
    cns = [ (c,n) |
        c <- Common.Coord.cardinal p,
        let n = M.findWithDefault '#' c w, 
        n /= '#',
        not (isUpper n) || (toLower n `elem` ks)
        ]
    move c '.' = (MazeState c ks w, 1)
    move c '@' = (MazeState c ks w, 1)
    move c k
        | isLower k = (MazeState c (S.insert k ks) w, 1)
        | isUpper k = (MazeState c ks w, 1)
    move c f = error ("non exhaustive " ++ show c ++ show f ++ " keys : " ++ show ks)
    in map (uncurry move) cns

data MazeStateB = MazeStateB
    { robotA :: Common.Coord.Coord
    , robotB :: Common.Coord.Coord
    , robotC :: Common.Coord.Coord
    , robotD :: Common.Coord.Coord
    , keysB  :: Set Char
    , worldB :: Map Common.Coord.Coord Char
    }

data MazeState = MazeState
    { position :: Common.Coord.Coord
    , keys :: Set Char
    , world :: Map Common.Coord.Coord Char
    }
instance Eq MazeState where (==) a b = position a == position b && keys a == keys b
instance Ord MazeState where compare :: MazeState -> MazeState -> Ordering
                             compare = comparing keys <> comparing position
instance Show MazeState where show a = show (position a) <> "," <> show (keys  a)
instance Eq MazeStateB where (==) a b = robotA a == robotA b && 
                                robotB a == robotB b &&
                                robotC a == robotC b &&
                                robotD a == robotD b &&
                                keysB a == keysB b
instance Ord MazeStateB where compare = comparing keysB <> comparing robotA <> comparing robotB <> comparing robotC <> comparing robotD
instance Show MazeStateB where show _ = "mazestate"
instance Dijkstra MazeStateB where


instance Dijkstra MazeState where
  type DijkstraCost MazeState = Int
  type DijkstraRepr MazeState = MazeState
  represent = id
  adjacency = nextMoves
  estimate = const 1


{-# LANGUAGE TypeFamilies #-}
module Challenges.Y2019.Day18 (solutionA, solutionB) where
import Common.Prelude ( solve )
import Text.ParserCombinators.Parsec
import Common.Coord (Coord, cardinal, showMap)
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
solutionB = solve parser (placeWall (41,40) >>> placeWall (41,41) >>> placeWall (41,42)
    >>> placeWall (40,41) >>> placeWall (42,41) >>>
    MazeStateB (40,40) (40,42) (42,40) (42,42) S.empty
    -- >>> singleton
    -- >>> searchFor allKeysCollectedB
    >>> nextMovesB
    >>> concatMap (nextMovesB . fst)
    )

placeWall :: Coord -> Map Coord Char -> Map Coord Char
placeWall = M.update (const $ Just '#')

parser :: Parser (Map Common.Coord.Coord Char)
parser = grid (letter <|> symbol <|> char '@')

allKeysCollected :: MazeState -> Bool
allKeysCollected (MazeState _ ks _) =
    S.isSubsetOf (S.fromList ['a'..'z']) ks

allKeysCollectedB :: MazeStateB -> Bool
allKeysCollectedB (MazeStateB _ _ _ _ ks _) =
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

nextMovesB :: MazeStateB -> [(MazeStateB, Int)]
nextMovesB (MazeStateB p1 p2 p3 p4 ks w) = let
    cns1 = [ (c,n) |
        c <- Common.Coord.cardinal p1,
        let n = M.findWithDefault '#' c w,
        n /= '#',
        not (isUpper n) || (toLower n `elem` ks)
        ]
    move1 c k
        | isLower k = (MazeStateB c p2 p3 p4 (S.insert k ks) w, 1)
    move1 c _ = (MazeStateB c p2 p3 p4 ks w, 1)
    cns2 = [ (c,n) |
        c <- Common.Coord.cardinal p2,
        let n = M.findWithDefault '#' c w,
        n /= '#',
        not (isUpper n) || (toLower n `elem` ks)
        ]
    move2 c k
        | isLower k = (MazeStateB p1 c p3 p4 (S.insert k ks) w, 1)
    move2 c _ = (MazeStateB p1 c p3 p4 ks w, 1)
    cns3 = [ (c,n) |
        c <- Common.Coord.cardinal p3,
        let n = M.findWithDefault '#' c w,
        n /= '#',
        not (isUpper n) || (toLower n `elem` ks)
        ]
    move3 c k
        | isLower k = (MazeStateB p1 p2 c p4 (S.insert k ks) w, 1)
    move3 c _ = (MazeStateB p1 p2 c p4 ks w, 1)
    cns4 = [ (c,n) |
        c <- Common.Coord.cardinal p4,
        let n = M.findWithDefault '#' c w,
        n /= '#',
        not (isUpper n) || (toLower n `elem` ks)
        ]
    move4 c k
        | isLower k = (MazeStateB p1 p2 p3 c (S.insert k ks) w, 1)
    move4 c _ = (MazeStateB p1 p2 p3 c ks w, 1)
    in map (uncurry move1) cns1 ++ map (uncurry move2) cns2 ++ map (uncurry move3) cns3 ++ map (uncurry move4) cns4

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
                             compare = comparing position <> comparing keys
instance Show MazeState where show a = show (position a) <> "," <> show (keys  a)
instance Eq MazeStateB where (==) a b = 
                                robotA a == robotA b &&
                                robotB a == robotB b &&
                                robotC a == robotC b &&
                                robotD a == robotD b &&
                                keysB a == keysB b
instance Ord MazeStateB where compare = comparing robotA <> comparing robotB <> comparing robotC <> comparing robotD <> comparing keysB
instance Show MazeStateB where show ms = show $ represent ms
instance Dijkstra MazeStateB where
    type DijkstraCost MazeStateB = Int
    type DijkstraRepr MazeStateB = (Coord, Coord, Coord, Coord, Set Char)
    represent ms = (robotA ms, robotB ms, robotC ms, robotD ms, keysB ms)
    adjacency = nextMovesB
    estimate = const 1


instance Dijkstra MazeState where
  type DijkstraCost MazeState = Int
  type DijkstraRepr MazeState = (Coord, Set Char)
  represent ms = (position ms, keys ms)
  adjacency :: MazeState -> [(MazeState, DijkstraCost MazeState)]
  adjacency = nextMoves
  estimate = const 1


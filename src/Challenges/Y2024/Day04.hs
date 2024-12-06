{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Challenges.Y2024.Day04 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, letter)
import Control.Arrow ((>>>), (&&&))
import Common.Parsing (grid)
import Common.Coord
    ( Coord, cardinal, diag, above, right, below, left )
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Common.Search (bfs)
import Data.Ord (comparing)

solutionA :: String -> String
solutionA = solve parser (allStarts >>> bfs nextLetter >>> filter ((==4) . length . poss)  >>> length)
solutionB :: String -> String
solutionB = solve parser (isXMAS &&& allA >>> uncurry filter >>> length)

allStarts :: Map Coord Char -> [LetterSearch]
allStarts m = let l = M.toList m in
    map (\(coord, _) -> LetterSearch [coord] ['X'] m) $ filter (\(_,a) -> a == 'X') l

allA :: Map Coord Char -> [Coord]
allA m = let l = M.toList m in
    map fst $ filter (\(_,a) -> a == 'A') l

isXMAS :: Map Coord Char -> Coord -> Bool
isXMAS m p = let
    lu = above $ left p
    ru = above $ right p
    lb = below $ left p
    rb = below $ right p
    in (M.lookup lu m == Just 'M' && M.lookup lb m == Just 'M' && M.lookup ru m == Just 'S' && M.lookup rb m == Just 'S')
     || (M.lookup lu m == Just 'S' && M.lookup lb m == Just 'S' && M.lookup ru m == Just 'M' && M.lookup rb m == Just 'M')
     || (M.lookup lu m == Just 'S' && M.lookup lb m == Just 'M' && M.lookup ru m == Just 'S' && M.lookup rb m == Just 'M')
     || (M.lookup lu m == Just 'M' && M.lookup lb m == Just 'S' && M.lookup ru m == Just 'M' && M.lookup rb m == Just 'S')

instance Eq LetterSearch where (==) a b = poss a == poss b && found a == found b
instance Ord LetterSearch where compare = comparing found <> comparing poss

data LetterSearch = LetterSearch
    { poss :: [Coord]
    , found :: [Char]
    , world :: Map Coord Char
    } deriving Show

instance Num Coord where
  (+) (x,y) (x',y')= (x+x', y + y')
  (-) (x,y) (x',y') = (x - x', y - y')
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined

nextLetter :: LetterSearch -> [LetterSearch]
nextLetter (LetterSearch ps f m) = [
        LetterSearch (p' : ps) f' m |
        let p = head ps,
        p' <- cardinal p ++ diag p,
        length ps == 1 || (p - ps !! 1) == (p' - p),  -- keep going in same direction
        let fp = M.lookup p' m,
        isJust fp,              -- position is in world
        fits f (fromJust fp),   -- found the right letter
        let f' = f ++ [fromJust fp]
    ]

fits :: [Char] -> Char -> Bool
fits [] 'X' = True
fits ['X' ] 'M' = True
fits ['X', 'M'] 'A' = True
fits ['X', 'M', 'A' ] 'S' = True
fits _ _ = False

parser :: Parser (Map Coord Char)
parser = grid letter
{-# LANGUAGE TypeFamilies #-}

module Challenges.Y2024.Day20 (solutionA, solutionB) where

import Common.Coord (Coord, cardinal, dist)
import Common.List (count, sumWith)
import Common.Parsing (grid)
import Common.Prelude (solve)
import Data.Array (Array, array, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec (Parser, alphaNum, char, (<|>))

solutionA :: String -> String
solutionA = solve parser (cheats goal 2) where goal = 100

solutionB :: String -> String
solutionB = solve parser (cheats goal 20) where goal = 100

type Track = Array Int Coord

data Maze = Maze Coord Coord (Set Coord) deriving (Show)

findTrack :: Maze -> [Coord]
findTrack = go []
  where
    go visited (Maze start end walls)
      | start == end = [end]
      | otherwise =
          let c = head $ filter (\x -> x `notElem` walls && x `notElem` visited) $ cardinal start
           in start : go (start : visited) (Maze c end walls)

validCheat :: Int -> Int -> (Int, Coord) -> (Int, Coord) -> Bool
validCheat targetedSaving maxCheatDistance start end =
  let d = dist (snd start) (snd end)
   in d <= maxCheatDistance && targetedSaving <= (fst end - fst start - d)

cheats :: Int -> Int -> Track -> Int
cheats goal cheatSize track = sumWith f [1..length track]
  where
    f start =
      count
        (\end -> validCheat goal cheatSize (start, track ! start) (end, track ! end))
        [start..length track]

parser :: Parser Track
parser = do
  g <- grid (alphaNum <|> char '#' <|> char '.')
  let ws = S.fromList $ map fst $ M.toList $ M.filter (== '#') g
  let s = fst $ head $ M.toList $ M.filter (== 'S') g
  let e = fst $ head $ M.toList $ M.filter (== 'E') g
  let t = findTrack $ Maze s e ws
  return $ array (1, length t) $ zip [1 ..] t

module Challenges.Y2024.Day06 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, char, (<|>))
import Control.Arrow ((>>>))
import Data.Map (Map)
import qualified Data.Map as M
import Common.Coord
import Common.Parsing (grid)
import Data.List (nub)
import Common.List (firstWhere)
import Data.Maybe (isNothing, fromJust)
import qualified Data.Set as S
import Data.Ord (comparing)

solutionA :: String -> String
solutionA = solve parser (findGuard >>> iterate moveGuardAlways >>> firstWhere isGone >>> path . fromJust >>> nub >>> length)
solutionB :: String -> String
solutionB = solve parser (possibleLoops >>> moveAll >>> map moveGuardAlways >>> filter (not . isGone) >>> length)

currentPath :: Map Coord Char -> [Coord]
currentPath = findGuard >>> iterate moveGuardAlways >>> firstWhere isGone >>> path . fromJust

possibleLoops :: Map Coord Char -> [GuardState]
possibleLoops m = let
        ps = currentPath m
        startPos = head ps
        pathWithoutStart = filter (/= startPos) ps
        m' = map (`placeObstacle` m) (nub pathWithoutStart)
    in map findGuard m'

moveAll :: [GuardState] -> [GuardState]
moveAll = map (iterate moveGuard >>> firstRepeating )

parser :: Parser (Map Coord Char)
parser = grid (char '.' <|> char '#' <|> char '^')

moveGuard :: GuardState -> GuardState
moveGuard g@(GuardState p f n w) = if isBlocked g
    then GuardState p (turnRight f) n w
    else let p' = move f p
    in if isGone (GuardState p' f n w)
        then g
        else GuardState p' f (n <> [p']) w

moveGuardAlways :: GuardState -> GuardState
moveGuardAlways g@(GuardState p f n w) = if isBlocked g
    then GuardState p (turnRight f) n w
    else let p' = move f p
    in GuardState p' f (n <> [p']) w

isGone :: GuardState -> Bool
isGone (GuardState p _ _ w) = isNothing (M.lookup p w)

isBlocked :: GuardState -> Bool
isBlocked (GuardState p f _ w) = let
    p' = move f p
    in M.lookup p' w == Just '#'

data GuardState = GuardState
    {   pos :: Coord
    ,   facing :: Dir
    ,   path :: [Coord]
    ,  world :: Map Coord Char
    }

instance Eq GuardState where
  (==) :: GuardState -> GuardState -> Bool
  (==) g h = pos g == pos h && facing g == facing h

instance Ord GuardState where
  compare = comparing pos <> comparing facing

instance Show GuardState where
    show g = show (pos g) <> "," <> show (facing g) <> "," <> show (path g)

placeObstacle :: Coord -> Map Coord Char -> Map Coord Char
placeObstacle = M.update (\_ -> Just '#')


findGuard :: Map Coord Char -> GuardState
findGuard w = let
    cs = M.toList w
    (p,_) = head $ filter (\(_,a) -> a == '^') cs
    in GuardState p U [p] w

firstRepeating :: Ord a => [a] -> a
firstRepeating = go S.empty where
    go _ [] = undefined
    go seen (x:xs) = if x `S.member` seen then x else go (S.insert x seen) xs

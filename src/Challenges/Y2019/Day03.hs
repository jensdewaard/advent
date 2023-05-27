module Challenges.Y2019.Day03 where

import Shared
import qualified Data.List as L

input :: Bool -> IO String
input _ = readFile "data/2019/03.txt"

solutionA :: String -> String
--solutionA _ = show 0
solutionA inp = show $ closest (wires inp) (wireIntersections inp)

-- 63655 is too high
solutionB :: String -> String
solutionB inp = show $ best (wires inp) (wireIntersections inp)
--solutionB inp = show $ (wireIntersections inp)

wireIntersections :: String -> [Coord]
wireIntersections inp = intersections $ wires inp

wires :: String -> [[Coord]]
wires inp = map (wireToCoords (0,0)) $ parseInput inp

closest :: [[Coord]] -> [Coord] -> Int
closest _ ns = minimum $ filter (>0) $ map manDist ns

-- best:  wires    -> intersections -> distance along wire
best :: [[Coord]] -> [Coord] -> Int
best (wa : wb : _) is = minimum $ filter (>0) $ map (\c -> pathToLength wa c + pathToLength wb c) is
best _  _ = 0

pathToLength :: [Coord] -> Coord -> Int
pathToLength (w : ws) c = if w == c then 0 else 1 + pathToLength ws c
pathToLength [] _ = 0

parseInput :: String -> [Wire]
parseInput x = map (map readDir . splitOn ",") (lines x)

intersections :: [[Coord]] -> [Coord]
intersections (a : b : _) = a `L.intersect` b
intersections _ = error "invalid number of wires"

manDist :: Coord -> Int
manDist (x, y) = abs x + abs y

type Wire = [DirAndLength]

data DirAndLength = DirLength Dir Int

readDir :: String -> DirAndLength
readDir ('U' : ss) = DirLength U (read ss)
readDir ('L' : ss) = DirLength L (read ss)
readDir ('R' : ss) = DirLength R (read ss)
readDir ('D' : ss) = DirLength D (read ss)
readDir _ = error "invalid direction vector"

wireToCoords :: Coord -> Wire -> [Coord]
wireToCoords s (DirLength _ 0 : ds) = wireToCoords s ds
wireToCoords s@(x,y) (DirLength U n : ds) = s : wireToCoords s' (DirLength U (n-1) : ds) where
    s' = (x, y - 1)
wireToCoords s@(x,y) (DirLength D n : ds) = s : wireToCoords s' (DirLength D (n-1) : ds) where
    s' = (x, y + 1)
wireToCoords s@(x,y) (DirLength L n : ds) = s : wireToCoords s' (DirLength L (n-1) : ds) where
    s' = (x - 1, y)
wireToCoords s@(x,y) (DirLength R n : ds) = s : wireToCoords s' (DirLength R (n-1) : ds) where
    s' = (x + 1, y)
wireToCoords s [] = [s]

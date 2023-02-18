module Challenges.Y2015.Day03 (input, solutionA, solutionB) where
import Shared (move, Coord, Dir (..))
import Data.List (nub)
import DSP.Basic (uninterleave)

input :: Bool -> IO String
input False = readFile "data/2015/03.txt"
input True = return "^v^v^v^v^v"

solutionA :: String -> String
solutionA = show . length . nub . routeSanta . parseInput

solutionB :: String -> String
solutionB = show 
    . length 
    . nub 
    . pconcat
    . pmap (routeSanta)
    . uninterleave 
    . parseInput

pmap :: (a -> b) -> (a,a) -> (b,b)
pmap f (x, y) = (f x, f y)

pconcat :: ([a], [a]) -> [a]
pconcat (xs, ys) = xs ++ ys

routeSanta :: [Dir] -> [Coord]
routeSanta = scanl (flip move) (0,0)

parseInput :: String -> [Dir]
parseInput = map parseDir

parseDir :: Char -> Dir
parseDir '>' = R
parseDir '<' = L
parseDir 'v' = D
parseDir '^' = U
parseDir _ = error "invalid direction"
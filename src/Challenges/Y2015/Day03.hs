module Challenges.Y2015.Day03 (input, solutionA, solutionB) where
import Shared (move, Coord, Dir (..))
import Data.List (nub)

input :: Bool -> IO String
input _ = readFile "data/2015/03.txt"

solutionA :: String -> String
solutionA = show . length . nub . routeSanta . parseInput

solutionB :: String -> String
solutionB = undefined

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
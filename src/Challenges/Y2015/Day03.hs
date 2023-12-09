module Challenges.Y2015.Day03 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve, move, Coord, Dir (..))
import Data.List (nub)
import DSP.Basic (uninterleave)

solutionA :: String -> String
solutionA = solve parseInput (length . nub . routeSanta)

solutionB :: String -> String
solutionB = solve parseInput (length
    . nub
    . pconcat
    . pmap routeSanta
    . uninterleave
    ) 

pmap :: (a -> b) -> (a,a) -> (b,b)
pmap f (x, y) = (f x, f y)

pconcat :: ([a], [a]) -> [a]
pconcat (xs, ys) = xs ++ ys

routeSanta :: [Dir] -> [Coord]
routeSanta = scanl (flip move) (0,0)

parseInput :: Parser [Dir]
parseInput = many1 dir

dir :: Parser Dir
dir = (char '>' >> return R)
    <|> (char '<' >> return L)
    <|> (char 'v' >> return D)
    <|> (char '^' >> return U)
module Challenges.Y2015.Day03 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.Coord (move, Coord, Dir (..))
import Common.Prelude
import Data.List (nub)
import Data.Bifunctor (bimap)
import Common.List (uninterleave)

solutionA :: String -> String
solutionA = solve parseInput (length . nub . routeSanta)

solutionB :: String -> String
solutionB = solve parseInput (length
    . nub
    . uncurry (++)
    . bimap routeSanta routeSanta
    . uninterleave
    ) 

routeSanta :: [Dir] -> [Coord]
routeSanta = scanl (flip move) (0,0)

parseInput :: Parser [Dir]
parseInput = many1 dir

dir :: Parser Dir
dir = (char '>' >> return R)
    <|> (char '<' >> return L)
    <|> (char 'v' >> return D)
    <|> (char '^' >> return U)
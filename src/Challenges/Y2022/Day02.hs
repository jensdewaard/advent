module Challenges.Y2022.Day02 (solutionA, solutionB) where

import Common.Prelude
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = parser game  ==> sum . map score
solutionB :: String -> String
solutionB = parser gameB ==> sum . map score'

parser :: Parser a -> Parser [a]
parser p = p `sepEndBy` newline

game :: Parser Round
game = do
    l <- move
    _ <- space
    r <- move
    return (l,r)

gameB :: Parser Round'
gameB = do
    l <- move
    _ <- space
    r <- result
    return (l,r)

move :: Parser Move
move = (char 'A' >> return Rock)
    <|> (char 'X' >> return Rock)
    <|> (char 'B' >> return Paper)
    <|> (char 'Y' >> return Paper)
    <|> (char 'C' >> return Scissors)
    <|> (char 'Z' >> return Scissors)

result :: Parser Result
result = (char 'X' >> return Lose)
    <|> (char 'Y' >> return Draw)
    <|> (char 'Z' >> return Win)

data Move = Rock | Paper | Scissors
data Result = Win | Lose | Draw

type Round = (Move, Move)
type Round' = (Move, Result)

score :: Round -> Int
score (Rock, Rock) = 1 + 3
score (Rock, Paper) = 2 + 6
score (Rock, Scissors) = 3 + 0
score (Paper, Rock) = 1 + 0
score (Paper, Paper) = 2 + 3
score (Paper, Scissors) = 3 + 6
score (Scissors, Rock) = 1 + 6
score (Scissors, Paper) = 2 + 0
score (Scissors, Scissors) = 3 + 3

score' :: Round' -> Int
score' (Rock, Draw) = 1 + 3
score' (Rock, Win) = 2 + 6
score' (Rock, Lose) = 3 + 0
score' (Paper, Lose) = 1 + 0
score' (Paper, Draw) = 2 + 3
score' (Paper, Win) = 3 + 6
score' (Scissors, Win) = 1 + 6
score' (Scissors, Lose) = 2 + 0
score' (Scissors, Draw) = 3 + 3

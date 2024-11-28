module Challenges.Y2019.Day15 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = solve parser (const "")
solutionB :: String -> String
solutionB = solve parser (const "")

parser :: Parser ()
parser = undefined
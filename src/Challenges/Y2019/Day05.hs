module Challenges.Y2019.Day05 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = solve parser (const "")
solutionB :: String -> String
solutionB = solve parser (const "")

parser :: Parser ()
parser = undefined
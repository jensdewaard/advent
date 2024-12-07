module Challenges.Y2024.Day19 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser)
import Control.Arrow ((>>>), (&&&))

solutionA :: String -> String
solutionA = solve parser (const "")
solutionB :: String -> String
solutionB = solve parser (const "")

parser :: Parser ()
parser = undefined
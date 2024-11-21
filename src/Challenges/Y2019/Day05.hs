module Challenges.Y2019.Day05 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Intcode

solutionA :: String -> String
solutionA = solve parseProgram (
    snd . runProgramWithInput 1 . flip ProgState 0 )
solutionB :: String -> String
solutionB = solve parser (const "")

parser :: Parser ()
parser = undefined
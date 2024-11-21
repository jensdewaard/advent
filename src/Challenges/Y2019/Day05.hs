module Challenges.Y2019.Day05 (solutionA, solutionB) where
import Common.Prelude
import Intcode

solutionA :: String -> String
solutionA = solve parseProgram (snd . runProgramWithInput 1 . flip ProgState 0 )
solutionB :: String -> String
solutionB = solve parseProgram (snd . runProgramWithInput 5 . flip ProgState 0 )
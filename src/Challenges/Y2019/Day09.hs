module Challenges.Y2019.Day09 (solutionA, solutionB) where
import Common.Prelude
import Intcode (parseProgram, runInterpreter, ProgState (outputs), mkProgramWithInput)

solutionA :: String -> String
solutionA = solve parseProgram (outputs . runInterpreter . mkProgramWithInput [1])
solutionB :: String -> String
solutionB = solve parseProgram (outputs . runInterpreter . mkProgramWithInput [2])
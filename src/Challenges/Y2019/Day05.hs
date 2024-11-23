module Challenges.Y2019.Day05 (solutionA, solutionB) where
import Common.Prelude
import Intcode

solutionA :: String -> String
solutionA = solve parseProgram (outputs . runInterpreter . mkProgramWithInput [1])
solutionB :: String -> String
solutionB = solve parseProgram (outputs . runInterpreter . mkProgramWithInput [5])

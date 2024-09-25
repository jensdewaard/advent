module Challenges.Y2016.Day23 (solutionA, solutionB) where
import Challenges.Y2016.Assembunny (runProgram, mkProgram, parseInstructionSet, a_)
import Common.Prelude

solutionA :: String -> String
solutionA = solve parseInstructionSet (a_ . runProgram . mkProgram [7,0,0,0])
solutionB :: String -> String
solutionB = solve parseInstructionSet length

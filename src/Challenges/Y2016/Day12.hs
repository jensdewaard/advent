{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE GADTs #-}
module Challenges.Y2016.Day12 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Challenges.Y2016.Assembunny (Instruction, runProgram, startState, instruction, a_)
import Common.Prelude

solutionA :: String -> String
solutionA = solve parser (a_ . runProgram (startState 0))
solutionB :: String -> String
solutionB = solve parser (a_ . runProgram (startState 1))


parser :: Parser [Instruction]
parser = instruction `sepEndBy1` newline


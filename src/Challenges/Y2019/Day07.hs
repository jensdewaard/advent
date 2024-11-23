module Challenges.Y2019.Day07 (solutionA, solutionB) where
import Common.Prelude
import Data.List (permutations)
import Intcode (runProgram, runProgramUntil, mkProgramWithInput, parseProgram, ProgState(..), OpProgram, nextInstruction, OpCode (..), Mode (..), runInterpreter)

solutionA :: String -> String
-- solutionA = solve parseProgram (\p -> let
--     settings = permutations [0..4]
--     in maximum $ map (\s -> let
--         pA = mkProgramWithInput p [head s,0]
--         outputA = head $ outputs $ runProgram pA
--         pB = mkProgramWithInput p [s !! 1,outputA]
--         outputB = head $ outputs $ runProgram pB
--         pC = mkProgramWithInput p [s !! 2,outputB]
--         outputC = head $ outputs $ runProgram pC
--         pD = mkProgramWithInput p [s !! 3,outputC]
--         outputD = head $ outputs $ runProgram pD
--         pE = mkProgramWithInput p [s !! 4,outputD]
--         outputE = head $ outputs $ runProgram pE
--         in outputE) settings)
solutionA = const "a"

-- runMachines :: [ProgState] -> Int -> [ProgState ]
-- runMachines m x = let
--         ma = runProgramUntil (OpOutput (Immediate, 0)) (head m) { inputs = inputs (head m) ++ [x], outputs = []}
--         mb = runProgramUntil (OpOutput (Immediate, 0)) (m !! 1) { inputs = inputs (m !! 1) ++ getOutput ma, outputs = []}
--         mc = runProgramUntil (OpOutput (Immediate, 0)) (m !! 2) { inputs = inputs (m !! 2) ++ getOutput mb, outputs = []}
--         md = runProgramUntil (OpOutput (Immediate, 0)) (m !! 3) { inputs = inputs (m !! 3) ++ getOutput mc, outputs = []}
--         me = runProgramUntil (OpOutput (Immediate, 0)) (m !! 4) { inputs = inputs (m !! 4) ++ getOutput md, outputs = []}
--     in [
--         ma,
--         mb,
--         mc,
--         md,
--         me
--     ]

solutionB :: String -> String
-- solutionB = solve parseProgram (\p -> let
--     settings = permutations [5..9]
--     run :: Int -> [ProgState] -> Int
--     run n ms = let
--         outE = last $ outputs $ last ms
--         nextSet = runMachines ms n
--         in (if any ((==OpFinished) . nextInstruction) ms
--             then outE 
--             else run outE nextSet)
--     in maximum $ map (undefined) settings)
solutionB = undefined
module Challenges.Y2019.Day07 (solutionA, solutionB) where
import Common.Prelude
import Data.List (permutations, singleton)
import Intcode (mkProgramWithInput, parseProgram, ProgState(..), Ref(..), runInterpreter, runInterpreterUntil, peekInstruction, OpCode (..), OpProgram, getMemory)

solutionA :: String -> String
solutionA = solve parseProgram (\p -> let
    settings = permutations [0..4]
    in maximum $ map (\s -> let
        pA = mkProgramWithInput [head s,0] p
        outputA = head $ outputs $ runInterpreter pA
        pB = mkProgramWithInput [s !! 1,outputA] p
        outputB = head $ outputs $ runInterpreter pB
        pC = mkProgramWithInput [s !! 2,outputB] p
        outputC = head $ outputs $ runInterpreter pC
        pD = mkProgramWithInput [s !! 3,outputC] p
        outputD = head $ outputs $ runInterpreter pD
        pE = mkProgramWithInput [s !! 4,outputD] p
        outputE = head $ outputs $ runInterpreter pE
        in outputE) settings)

getOutput :: ProgState -> [Int]
getOutput = singleton . head . outputs

runMachines :: [ProgState] -> Int -> [ProgState]
runMachines m x = let
        ma = runInterpreterUntil (OpOutput (Immediate 0)) (head m) { inputs = inputs (head m) ++ [x], outputs = []}
        mb = runInterpreterUntil (OpOutput (Immediate 0)) (m !! 1) { inputs = inputs (m !! 1) ++ getOutput ma, outputs = []}
        mc = runInterpreterUntil (OpOutput (Immediate 0)) (m !! 2) { inputs = inputs (m !! 2) ++ getOutput mb, outputs = []}
        md = runInterpreterUntil (OpOutput (Immediate 0)) (m !! 3) { inputs = inputs (m !! 3) ++ getOutput mc, outputs = []}
        me = runInterpreterUntil (OpOutput (Immediate 0)) (m !! 4) { inputs = inputs (m !! 4) ++ getOutput md, outputs = []}
    in [ma, mb, mc, md, me]

initMachines :: OpProgram -> [Int] -> [ProgState]
initMachines p ss = let
    ma = mkProgramWithInput (singleton $ head ss) p
    mb = mkProgramWithInput (singleton $ ss !! 1) p
    mc = mkProgramWithInput (singleton $ ss !! 2) p
    md = mkProgramWithInput (singleton $ ss !! 3) p
    me = mkProgramWithInput (singleton $ ss !! 4) p
    in [ma,mb,mc,md,me]

solutionB :: String -> String
solutionB = solve parseProgram (\p -> let
    settings = permutations [5..9]
    machines = map (initMachines p) settings
    possibleOutputs = map (run 0) machines
    in maximum possibleOutputs)

run :: Int -> [ProgState] -> Int
run inputA ms = let
    nextSet = runMachines ms inputA
    outLast = last $ outputs $ last ms
    outE = last $ outputs $ last nextSet
    in (if any ((==OpFinished) . (\ps -> peekInstruction (getMemory ps) (ptr ps))) ms
        then outLast
        else run outE nextSet)
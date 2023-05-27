module Challenges.Y2019.Day02 where
import Shared (splitOn)
import qualified Data.Bifunctor

solutionA :: String -> String
solutionA inp = show $ head $ runOpProgram 0 prog' where
    prog' = snd $ initMemory (readProg inp) (12, 1)

solutionB :: String -> String
solutionB inp = show $ answer $ fst $
    head $
    filter isCorrect $ map (Data.Bifunctor.second (runOpProgram 0) . initMemory prog) posSol where
    prog = readProg inp

answer :: (Int, Int) -> Int
answer (n, v) = 100 * n + v

initMemory :: OpProgram -> (Int, Int) -> ((Int, Int), OpProgram)
initMemory prog (noun, verb) = ((noun, verb), opReplace 2 verb $ opReplace 1 noun prog)

isCorrect :: ((Int, Int), OpProgram) -> Bool
isCorrect (_, p) = head p == 19690720

posSol :: [(Int, Int)]
posSol = [(x, y) | x <- [0..100], y <- [0..100]]

readInt :: String -> Int
readInt = read

readProg :: String -> OpProgram
readProg s = map readInt $ splitOn "," s

input :: Bool -> IO String
input True = return "1,9,10,3,2,3,11,0,99,30,40,50"
input False = readFile "data/2019/02.txt"

data OpCode =
    OpFinished
    | OpAdd Int Int Int
    | OpMult Int Int Int
    deriving Eq

readOpCode :: OpProgram -> OpCode
readOpCode (1 : opA : opB : res : _) = OpAdd opA opB res
readOpCode (2 : opA : opB : res : _) = OpMult opA opB res
readOpCode (99 : _) = OpFinished
readOpCode o = error ("invalid opcode: " ++ show o)

type OpProgram = [Int]

opReplace :: Int -> Int -> OpProgram -> OpProgram
opReplace 0 val (_ : os) = val : os
opReplace idx val (o : os) = o : opReplace (idx - 1) val os
opReplace _ _ [] = []

runOpProgram :: Int -> OpProgram -> OpProgram
runOpProgram idx prog = let
        c = readOpCode (take 4 (drop idx prog))
        prog' = runOpCode c prog
        in
        if c == OpFinished
            then prog
            else runOpProgram (idx + 4) prog'

runOpCode :: OpCode -> OpProgram -> OpProgram
runOpCode OpFinished prog = prog
runOpCode (OpAdd opA opB res) prog = opReplace res ((prog !! opA) + (prog !! opB)) prog
runOpCode (OpMult opA opB res) prog = opReplace res ((prog !! opA) * (prog !! opB)) prog
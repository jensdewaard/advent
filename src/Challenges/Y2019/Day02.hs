module Challenges.Y2019.Day02 where
import Shared (splitOn)

solutionA :: String -> String
solutionA inp = show $ runOpProgram 0 prog' where
    prog' = opReplace 2 2 $ opReplace 1 12 $ readProg inp

solutionB :: String -> String
solutionB = undefined

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
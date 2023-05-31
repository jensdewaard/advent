module Intcode where

import Shared (splitOn)

type OpProgram = [Int]

data OpCode =
    OpFinished
    | OpAdd Int Int Int
    | OpMult Int Int Int
    deriving Eq

parseProgram :: String -> OpProgram
parseProgram s = map read $ splitOn "," s

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

opReplace :: Int -> Int -> OpProgram -> OpProgram
opReplace 0 val (_ : os) = val : os
opReplace idx val (o : os) = o : opReplace (idx - 1) val os
opReplace _ _ [] = []

readOpCode :: OpProgram -> OpCode
readOpCode (1 : opA : opB : res : _) = OpAdd opA opB res
readOpCode (2 : opA : opB : res : _) = OpMult opA opB res
readOpCode (99 : _) = OpFinished
readOpCode o = error ("invalid opcode: " ++ show o)

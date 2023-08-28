module Intcode (ProgState (ProgState, memory, ptr), OpProgram, runOpProgram, parseProgram, opReplace ) where

import Shared (splitOn)

type OpProgram = [Int]

data OpCode =
    OpFinished
    | OpAdd Int Int Int
    | OpMult Int Int Int
    deriving Eq

runOpProgram = undefined

data ProgState = ProgState 
    { memory    :: OpProgram
    , ptr       :: Int
    }

parseProgram :: String -> OpProgram
parseProgram s = map read $ splitOn "," s

runOpCode :: OpCode -> ProgState -> ProgState
runOpCode OpFinished prog = prog
runOpCode (OpAdd opA opB res) prog@(ProgState { memory = mem, ptr = p } ) = prog { memory = opReplace res ((mem !! opA) + (mem !! opB)) mem, ptr = p+4 }
runOpCode (OpMult opA opB res) prog@(ProgState { memory = mem, ptr = p} ) = prog { memory = opReplace res ((mem !! opA) * (mem !! opB)) mem, ptr = p+4 }

opReplace :: Int -> Int -> OpProgram -> OpProgram
opReplace 0 val (_ : os) = val : os
opReplace idx val (o : os) = o : opReplace (idx - 1) val os
opReplace _ _ [] = []

parseInstruction :: Int -> OpCode 
parseInstruction = undefined

readInstruction :: ProgState -> OpCode
readInstruction = undefined

{-# LANGUAGE ViewPatterns #-}
module Intcode (ProgState (ProgState, memory, ptr), OpProgram, runOpProgram, parseProgram, opReplace ) where

import Text.ParserCombinators.Parsec

type OpProgram = [Int]

data OpCode =
    OpFinished
    | OpAdd Int Int Int
    | OpMult Int Int Int
    deriving Eq

runOpProgram :: ProgState -> ProgState
runOpProgram s = let 
    i = readInstruction s 
    s' = runOpCode i s
    in if i == OpFinished then s else runOpProgram s'


data ProgState = ProgState 
    { memory    :: OpProgram
    , ptr       :: Int
    } deriving Eq

parseProgram :: Parser OpProgram
parseProgram = (read <$> many1 digit) `sepBy1` char ','

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
readInstruction ProgState { memory = mem, ptr = p }
    | mem !! p == 99 = OpFinished
    | mem !! p == 1 = OpAdd (mem !! (p+1)) (mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 2 = OpMult (mem !! (p+1)) (mem !! (p+2)) (mem !! (p+3))

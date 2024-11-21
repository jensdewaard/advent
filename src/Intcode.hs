{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Intcode (ProgState (ProgState, memory, ptr), OpProgram, initMemory, parseProgram, runProgram, runProgramWithInput) where

import Text.ParserCombinators.Parsec
import Control.Monad.Reader
    ( MonadReader, ask, runReaderT, mapReaderT )
import Control.Monad.Writer.Class
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.Writer (runWriter)
import Common.Parsing (int)

type OpProgram = [Int]

data Mode = Immediate | Position deriving Eq
type Ref = (Mode, Int)

data OpCode =
    OpFinished
    | OpAdd Ref Ref Int
    | OpMult Ref Ref Int
    | OpInput Int
    | OpOutput Ref
    deriving Eq

runOpProgram :: (MonadReader Int m, MonadWriter [Int] m) => ProgState -> m ProgState
runOpProgram s = do
    let i = readInstruction s
    s' <- runOpCode i s
    if i == OpFinished
        then pure s
        else runOpProgram s'

runProgramWithInput :: Int -> ProgState -> (ProgState, [Int])
runProgramWithInput x p = let
    m = runOpProgram p
    endState = ((\ n -> runIdentity $ runReaderT n x) . mapReaderT (pure . runWriter)) m
    in endState

runProgram :: ProgState -> (ProgState, [Int])
runProgram = runProgramWithInput 0

initMemory :: OpProgram -> (Int, Int) -> ProgState
initMemory prog (noun, verb) = ProgState { memory = opReplace 2 verb $ opReplace 1 noun prog, ptr = 0}

data ProgState =  ProgState
    { memory    :: OpProgram
    , ptr       :: Int
    } deriving (Eq, Show)

parseProgram :: Parser OpProgram
parseProgram = int `sepBy1` char ','

get :: OpProgram -> Ref -> Int
get _ (Immediate, n) = n
get mem (Position, n) = mem !! n

runOpCode :: (MonadReader Int m, MonadWriter [Int] m) => OpCode -> ProgState -> m ProgState
runOpCode OpFinished prog = pure prog
runOpCode (OpAdd opA opB res) prog@(ProgState { memory = mem, ptr = p } ) = pure $ prog { memory = opReplace res (get mem opA + get mem opB) mem, ptr = p+4 }
runOpCode (OpMult opA opB res) prog@(ProgState { memory = mem, ptr = p} ) = pure $ prog { memory = opReplace res (get mem opA * get mem opB) mem, ptr = p+4 }
runOpCode (OpInput res) prog@(ProgState { memory = mem, ptr = p }) = do
  v <- ask
  pure $ prog { memory = opReplace res v mem, ptr = p + 2}
runOpCode (OpOutput res) prog@(ProgState { memory = mem, ptr = p}) = do
  _ <- writer (prog, [get mem res])
  pure $ prog { memory = mem, ptr = p + 2}


opReplace :: Int -> Int -> OpProgram -> OpProgram
opReplace 0 val (_ : os) = val : os
opReplace idx val (o : os) = o : opReplace (idx - 1) val os
opReplace _ _ [] = []

readInstruction :: ProgState -> OpCode
readInstruction ProgState { memory = mem, ptr = p }
    | mem !! p == 99 = OpFinished
    | mem !! p == 1 = OpAdd (Position, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 1001 = OpAdd (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 0101 = OpAdd (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 1101 = OpAdd (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 2 = OpMult (Position, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 1002 = OpMult (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 0102 = OpMult (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 1102 = OpMult (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 3 = OpInput (mem !! (p+1))
    | mem !! p == 4 = OpOutput (Position, mem !! (p+1))
    | mem !! p == 104 = OpOutput (Immediate, mem !! (p+1))
    | otherwise = error $ "undefined opcode: " ++ show (mem !! p)
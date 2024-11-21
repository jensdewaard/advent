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
    | OpJmpT Ref Ref
    | OpJmpF Ref Ref
    | OpLt Ref Ref Ref
    | OpEq Ref Ref Ref
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
runOpCode (OpJmpT p1 p2) prog@(ProgState { memory = mem, ptr = p}) = pure $ if get mem p1 /= 0 
  then prog { ptr = get mem p2 }
  else prog { ptr = p + 3 }
runOpCode (OpJmpF p1 p2) prog@(ProgState { memory = mem, ptr = p}) = pure $ if get mem p1 == 0 
  then prog { ptr = get mem p2 }
  else prog { ptr = p + 3 }
runOpCode (OpLt p1 p2 p3) prog@(ProgState { memory = mem, ptr = p}) = pure $ if get mem p1 < get mem p2 
  then prog { memory = opReplace (get mem p3) 1 mem, ptr = p + 4 }
  else prog { memory = opReplace (get mem p3) 0 mem, ptr = p + 4 }
runOpCode (OpEq p1 p2 p3) prog@(ProgState { memory = mem, ptr = p}) = pure $ if get mem p1 == get mem p2 
  then prog { memory = opReplace (get mem p3) 1 mem, ptr = p + 4 }
  else prog { memory = opReplace (get mem p3) 0 mem, ptr = p + 4 }


opReplace :: Int -> Int -> OpProgram -> OpProgram
opReplace 0 val (_ : os) = val : os
opReplace idx val (o : os) = o : opReplace (idx - 1) val os
opReplace _ _ [] = []

readInstruction :: ProgState -> OpCode
readInstruction ProgState { memory = mem, ptr = p }
    | mem !! p == 99 = OpFinished
    | mem !! p == 00001 = OpAdd (Position, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 01001 = OpAdd (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 00101 = OpAdd (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 01101 = OpAdd (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 00002 = OpMult (Position, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 01002 = OpMult (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 00102 = OpMult (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 01102 = OpMult (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 00003 = OpInput (mem !! (p+1))
    | mem !! p == 00004 = OpOutput (Position, mem !! (p+1))
    | mem !! p == 00104 = OpOutput (Immediate, mem !! (p+1))
    | mem !! p == 00005 = OpJmpT (Position, mem !! (p+1)) (Position, mem !! (p+2))
    | mem !! p == 00105 = OpJmpT (Immediate, mem !! (p+1)) (Position, mem !! (p+2))
    | mem !! p == 01005 = OpJmpT (Position, mem !! (p+1)) (Immediate, mem !! (p+2))
    | mem !! p == 01105 = OpJmpT (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2))
    | mem !! p == 00006 = OpJmpF (Position, mem !! (p+1)) (Position, mem !! (p+2))
    | mem !! p == 00106 = OpJmpF (Immediate, mem !! (p+1)) (Position, mem !! (p+2))
    | mem !! p == 01006 = OpJmpF (Position, mem !! (p+1)) (Immediate, mem !! (p+2))
    | mem !! p == 01106 = OpJmpF (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2))
    | mem !! p == 00007 = OpLt (Position, mem !! (p+1)) (Position, mem !! (p+2)) (Position, mem !! (p+3))
    | mem !! p == 00107 = OpLt (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (Position, mem !! (p+3))
    | mem !! p == 01007 = OpLt (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (Position, mem !! (p+3))
    | mem !! p == 01107 = OpLt (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (Position, mem !! (p+3))
    | mem !! p == 10007 = OpLt (Position, mem !! (p+1)) (Position, mem !! (p+2)) (Immediate, mem !! (p+3))
    | mem !! p == 10107 = OpLt (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (Immediate, mem !! (p+3))
    | mem !! p == 11007 = OpLt (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (Immediate, mem !! (p+3))
    | mem !! p == 11107 = OpLt (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (Immediate, mem !! (p+3))
    | mem !! p == 00008 = OpEq (Position, mem !! (p+1)) (Position, mem !! (p+2)) (Position, mem !! (p+3))
    | mem !! p == 00108 = OpEq (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (Position, mem !! (p+3))
    | mem !! p == 01008 = OpEq (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (Position, mem !! (p+3))
    | mem !! p == 01108 = OpEq (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (Position, mem !! (p+3))
    | mem !! p == 10008 = OpEq (Position, mem !! (p+1)) (Position, mem !! (p+2)) (Immediate, mem !! (p+3))
    | mem !! p == 10108 = OpEq (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (Immediate, mem !! (p+3))
    | mem !! p == 11008 = OpEq (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (Immediate, mem !! (p+3))
    | mem !! p == 11108 = OpEq (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (Immediate, mem !! (p+3))
    | otherwise = error $ "undefined opcode: " ++ show (mem !! p) ++ ", at position " ++ show p
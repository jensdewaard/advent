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

type OpProgram = [Int]

data OpCode =
    OpFinished
    | OpAdd Int Int Int
    | OpMult Int Int Int
    | OpInput Int
    | OpOutput Int
    deriving Eq

runOpProgram :: (MonadReader Int m, MonadWriter [Int] m) => ProgState -> m ProgState
runOpProgram s = do
    let i = readInstruction s
    s' <- runOpCode i s
    if i == OpFinished
        then pure s
        else runOpProgram s'

runProgramWithInput :: Int -> ProgState -> ProgState
runProgramWithInput x p = let
    m = runOpProgram p
    endState = ((\ n -> runIdentity $ runReaderT n x) . mapReaderT (pure . runWriter)) m
    in fst endState

runProgram :: ProgState -> ProgState
runProgram = runProgramWithInput 0

initMemory :: OpProgram -> (Int, Int) -> ProgState
initMemory prog (noun, verb) = ProgState { memory = opReplace 2 verb $ opReplace 1 noun prog, ptr = 0}

data ProgState =  ProgState
    { memory    :: OpProgram
    , ptr       :: Int
    } deriving Eq

parseProgram :: Parser OpProgram
parseProgram = parseValue `sepBy1` char ','

parseValue :: Read a => Parser a
parseValue = do
    read <$> many1 digit


runOpCode :: (MonadReader Int m, MonadWriter [Int] m) => OpCode -> ProgState -> m ProgState
runOpCode OpFinished prog = pure prog
runOpCode (OpAdd opA opB res) prog@(ProgState { memory = mem, ptr = p } ) = pure $ prog { memory = opReplace res (mem !! opA + mem !! opB) mem, ptr = p+4 }
runOpCode (OpMult opA opB res) prog@(ProgState { memory = mem, ptr = p} ) = pure $ prog { memory = opReplace res (mem !! opA * mem !! opB) mem, ptr = p+4 }
runOpCode (OpInput res) prog@(ProgState { memory = mem, ptr = p }) = do
  v <- ask
  pure $ prog { memory = opReplace res v mem, ptr = p + 2}
runOpCode (OpOutput res) prog@(ProgState { memory = mem, ptr = p}) = do
  let v = mem !! fromInteger (toInteger res)
  _ <- writer (prog, [v])
  pure $ prog { memory = mem, ptr = p + 2}


opReplace :: Int -> Int -> OpProgram -> OpProgram
opReplace 0 val (_ : os) = val : os
opReplace idx val (o : os) = o : opReplace (idx - 1) val os
opReplace _ _ [] = []

readInstruction :: ProgState -> OpCode
readInstruction ProgState { memory = mem, ptr = p }
    | mem !! p == 99 = OpFinished
    | mem !! p == 1 = OpAdd (mem !! (p+1)) (mem !! (p+2)) (mem !! (p+3))
    | mem !! p == 2 = OpMult (mem !! (p+1)) (mem !! (p+2)) (mem !! (p+3))
    | otherwise = error "undefined opcode"
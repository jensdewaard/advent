{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
module Intcode (runInterpreter, ProgState(..), OpProgram, OpCode(..), nextInstruction, Mode(..), initMemory, mkProgram, mkProgramWithInput, parseProgram, runProgram, runProgramUntil, opReplace) where

import Text.ParserCombinators.Parsec ( Parser, char, sepBy1 )
import Common.Parsing (int)
import Control.Monad.State

type OpProgram = [Int]

data Mode = Immediate | Position deriving Eq
data IntCodeErrors = WrongOpCode Int | OutOfBounds Int | NoInput deriving (Eq, Show)
type Ref = (Mode, Int)
type Pointer = Int


data OpCode =
    OpFinished
    | OpAdd Ref Ref Int
    | OpMult Ref Ref Int
    | OpInput Int
    | OpOutput Ref
    | OpJmpT Ref Ref
    | OpJmpF Ref Ref
    | OpLt Ref Ref Int
    | OpEq Ref Ref Int
    deriving Eq

data ProgState = ProgState
    { memory    :: OpProgram
    , ptr       :: Pointer
    , inputs    :: [Int]
    , outputs   :: [Int]
    } | FailedState IntCodeErrors deriving (Eq, Show)

class (MonadState ProgState m) => MonadPointer m where
  getPtr :: m Pointer
  modifyPtr :: (Pointer -> Pointer) -> m ()

instance Monad m => MonadPointer (StateT ProgState m) where
  modifyPtr :: (Pointer -> Pointer) -> StateT ProgState m ()
  modifyPtr f = do
    ps <- get
    let pointer = ptr ps
    let ps' = ps { ptr = f pointer }
    put ps'
  getPtr :: StateT ProgState m Pointer
  getPtr = gets ptr

class (MonadState ProgState m) => MonadMemory m where
  getMem :: m OpProgram
  modifyMem :: (OpProgram -> OpProgram) -> m ()

class (MonadState ProgState m) => MonadInput m where
  consumeInput :: m Int

instance Monad m => MonadInput (StateT ProgState m) where
  consumeInput :: StateT ProgState m Int
  consumeInput = do
    ps <- get
    let i = head $ inputs ps
    let ps' = ps { inputs = tail $ inputs ps }
    put ps'
    return i

class (MonadState ProgState m) => MonadOutput m where
  writeOutput :: Int -> m ()

instance Monad m => MonadOutput (StateT ProgState m) where
  writeOutput :: Int -> StateT ProgState m ()
  writeOutput o = do
    ps <- get
    let ps' = ps { outputs = outputs ps <> [o] }
    put ps'

instance Monad m => MonadMemory (StateT ProgState m) where
  modifyMem :: (OpProgram -> OpProgram) -> StateT ProgState m ()
  modifyMem f = do
    ps <- get
    let mem = memory ps
    let ps' = ps { memory = f mem }
    put ps'
  getMem :: StateT ProgState m OpProgram
  getMem = gets memory

empty :: a -> ProgState
empty _ = ProgState {
  memory = [],
  inputs = [],
  ptr = 0,
  outputs = []
}

mkProgram :: OpProgram -> ProgState
mkProgram m = ProgState {
  memory = m,
  inputs = [],
  outputs = [],
  ptr = 0
  }

mkProgramWithInput :: OpProgram -> [Int] -> ProgState
mkProgramWithInput is mem = (empty ()) { memory = mem, inputs = is }

runInterpreter :: ProgState -> ProgState
runInterpreter = execState runProgram 

runProgram :: (MonadPointer m, MonadMemory m, MonadInput m, MonadOutput m) =>  m ()
runProgram = runProgramUntil OpFinished

runProgramUntil :: (MonadPointer m, MonadMemory m, MonadInput m, MonadOutput m) => OpCode -> m ()
runProgramUntil op = do
      i <- nextInstruction
      runOpCode i
      if shouldFinish i op
        then return ()
        else runProgramUntil op

shouldFinish :: OpCode -> OpCode -> Bool
shouldFinish OpFinished _ = True
shouldFinish (OpAdd {}) (OpAdd {}) = True
shouldFinish (OpMult {}) (OpMult {}) = True
shouldFinish (OpInput  _) (OpInput _) = True
shouldFinish (OpOutput  _) (OpOutput _) = True
shouldFinish (OpJmpT _  _) (OpJmpT _ _) = True
shouldFinish (OpJmpF _  _) (OpJmpF _ _) = True
shouldFinish (OpLt {}) (OpLt {}) = True
shouldFinish (OpEq {}) (OpEq {}) = True
shouldFinish _ _ = False

initMemory :: OpProgram -> (Int, Int) -> ProgState
initMemory prog (noun, verb) = ProgState {
  memory = opReplace 2 verb $ opReplace 1 noun prog,
  ptr = 0,
  inputs =[],
  outputs = []
  }

parseProgram :: Parser OpProgram
parseProgram = int `sepBy1` char ','

fetch :: OpProgram -> Ref -> Int
fetch _ (Immediate, n) = n
fetch mem (Position, n) = if n >= length mem 
  then error ("cannot retrieve memory " ++ show n ++ " , " ++ show mem) 
  else mem !! n

runOpCode :: (MonadPointer m, MonadMemory m, MonadInput m, MonadOutput m) => OpCode -> m ()
runOpCode OpFinished = return ()
runOpCode (OpAdd opA opB res)  = do
  mem <- getMem
  modifyMem (opReplace res (fetch mem opA + fetch mem opB))
  modifyPtr (+4)
runOpCode (OpMult opA opB res) = do
  mem <- getMem
  modifyMem (opReplace res (fetch mem opA * fetch mem opB))
  modifyPtr (+4)
runOpCode (OpInput res) = do
    v <- consumeInput
    modifyMem (opReplace res v)
    modifyPtr (+2)
runOpCode (OpOutput res) = do
  mem <- getMem
  writeOutput (fetch mem res)
  modifyPtr (+2)
runOpCode (OpJmpT p1 p2) = do
  mem <- getMem
  if fetch mem p1 /= 0
  then modifyPtr (const $ fetch mem p2)
  else modifyPtr (+3)
runOpCode (OpJmpF p1 p2) = do
  mem <- getMem
  if fetch mem p1 == 0
  then modifyPtr (const $ fetch mem p2)
  else modifyPtr (+3)
runOpCode (OpLt p1 p2 p3) = do
  mem <- getMem
  if fetch mem p1 < fetch mem p2
  then do
    modifyMem (opReplace p3 1)
    modifyPtr (+4)
  else do
    modifyMem (opReplace p3 0)
    modifyPtr (+4)
runOpCode (OpEq p1 p2 p3) = do
  mem <- getMem
  if fetch mem p1 == fetch mem p2
  then do
    modifyMem (opReplace p3 1)
    modifyPtr (+4)
  else do
    modifyMem (opReplace p3 0)
    modifyPtr (+4)


opReplace :: Int -> Int -> OpProgram -> OpProgram
opReplace 0 val (_ : os) = val : os
opReplace idx val (o : os) = o : opReplace (idx - 1) val os
opReplace _ _ [] = []

nextInstruction :: (MonadMemory m, MonadPointer m) => m OpCode
nextInstruction = do
    mem <- getMem
    p <- getPtr
    if p >= length mem 
      then error "index out of bounds" 
      else case mem !! p of
        99 -> return OpFinished
        00001 -> return $ OpAdd (Position, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
        01001 -> return $ OpAdd (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
        00101 -> return $ OpAdd (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
        01101 -> return $ OpAdd (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
        00002 -> return $ OpMult (Position, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
        01002 -> return $ OpMult (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
        00102 -> return $ OpMult (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
        01102 -> return $ OpMult (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
        00003 -> return $ OpInput (mem !! (p+1))
        00004 -> return $ OpOutput (Position, mem !! (p+1))
        00104 -> return $ OpOutput (Immediate, mem !! (p+1))
        00005 -> return $ OpJmpT (Position, mem !! (p+1)) (Position, mem !! (p+2))
        00105 -> return $ OpJmpT (Immediate, mem !! (p+1)) (Position, mem !! (p+2))
        01005 -> return $ OpJmpT (Position, mem !! (p+1)) (Immediate, mem !! (p+2))
        01105 -> return $ OpJmpT (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2))
        00006 -> return $ OpJmpF (Position, mem !! (p+1)) (Position, mem !! (p+2))
        00106 -> return $ OpJmpF (Immediate, mem !! (p+1)) (Position, mem !! (p+2))
        01006 -> return $ OpJmpF (Position, mem !! (p+1)) (Immediate, mem !! (p+2))
        01106 -> return $ OpJmpF (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2))
        00007 -> return $ OpLt (Position, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
        00107 -> return $ OpLt (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
        01007 -> return $ OpLt (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
        01107 -> return $ OpLt (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
        10007 -> return $ OpLt (Position, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
        10107 -> return $ OpLt (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
        11007 -> return $ OpLt (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
        11107 -> return $ OpLt (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
        00008 -> return $ OpEq (Position, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
        00108 -> return $ OpEq (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
        01008 -> return $ OpEq (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
        01108 -> return $ OpEq (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
        10008 -> return $ OpEq (Position, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
        10108 -> return $ OpEq (Immediate, mem !! (p+1)) (Position, mem !! (p+2)) (mem !! (p+3))
        11008 -> return $ OpEq (Position, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
        11108 -> return $ OpEq (Immediate, mem !! (p+1)) (Immediate, mem !! (p+2)) (mem !! (p+3))
        _ -> error "no such opcode"
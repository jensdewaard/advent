{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
 {-# LANGUAGE UnicodeSyntax #-}
module Intcode (runInterpreter, address, Memory, runInterpreterUntil, peekInstruction, ProgState(..), OpProgram, Ref(..), OpCode(..), nextInstruction, initMemory, mkProgram, mkProgramWithInput, parseProgram, runProgram, runProgramUntil, opReplace, getMemory, memEdit, modifyMemory) where

import Text.ParserCombinators.Parsec ( Parser, char, sepBy1 )
import Common.Parsing (int)
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M

type OpProgram = [Int]

data IntCodeError = WrongOpCode Int | OutOfBounds Int | NoInput deriving (Eq, Show)
data Ref = Immediate Int | Position Int | Relative Int deriving Eq
type Pointer = Int

newtype Memory = Memory (Map Int Int) deriving (Eq, Show)

runRef :: Int -> Ref -> Int
runRef o (Relative n) = n + o
runRef _ (Immediate n) = n
runRef _ (Position n) = n

data OpCode =
    OpFinished
    | OpAdd Ref Ref Ref
    | OpMult Ref Ref Ref
    | OpInput Ref
    | OpOutput Ref
    | OpJmpT Ref Ref
    | OpJmpF Ref Ref
    | OpLt Ref Ref Ref
    | OpEq Ref Ref Ref
    | OpRBase Ref
    deriving Eq

data ProgState = ProgState
    { memory    :: Memory
    , ptr       :: Pointer
    , inputs    :: [Int]
    , outputs   :: [Int]
    , relativeBase :: Int
    } | FailedState IntCodeError deriving (Eq, Show)

class (MonadState ProgState m) => MonadPointer m where
  getPtr :: m Pointer
  modifyPtr :: (Pointer -> Pointer) -> m ()

instance Monad m => MonadPointer (StateT ProgState m) where
  modifyPtr :: (Pointer -> Pointer) -> StateT ProgState m ()
  modifyPtr f = do
    ps <- get
    put ps { ptr = f  $ ptr ps }
  getPtr :: StateT ProgState m Pointer
  getPtr = gets ptr

class (MonadState ProgState m) => MonadMemory m where
  getMem :: m Memory
  modifyMem :: (Memory -> Memory) -> m ()
  fetch :: Ref -> m Int

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

class (MonadState ProgState m) => MonadRelative m where
  getBase :: m Int
  modifyRelativeBase :: (Int -> Int) -> m ()

instance Monad m => MonadRelative (StateT ProgState m) where
  getBase :: StateT ProgState m Int
  getBase = gets relativeBase
  modifyRelativeBase :: (Int -> Int) -> StateT ProgState m ()
  modifyRelativeBase f = do
    ps <- get
    let ps' = ps { relativeBase = f $ relativeBase ps }
    put ps'

instance Monad m => MonadMemory (StateT ProgState m) where
  modifyMem :: (Memory -> Memory) -> StateT ProgState m ()
  modifyMem f = do
    ps <- get
    let mem = memory ps
    let ps' = ps { memory = f mem }
    put ps'
  getMem :: StateT ProgState m Memory
  getMem = gets memory
  fetch :: Ref -> StateT ProgState m Int
  fetch (Immediate n) = return n
  fetch (Relative n) = do
    Memory mem <- getMem
    offset <- getBase
    if (n + offset) < 0 then error "retrieving negative memory" else return $ (M.!) mem (n + offset)
  fetch (Position n) = do
    Memory mem <- getMem
    return $ M.findWithDefault 0 n mem 

empty ::  ProgState
empty = ProgState {
  memory = Memory mempty,
  inputs = [],
  ptr = 0,
  outputs = [],
  relativeBase = 0
}

mkProgram :: OpProgram -> ProgState
mkProgram m = empty { memory = Memory $ M.fromList (zip [0..] m) }

mkProgramWithInput :: [Int] -> OpProgram -> ProgState
mkProgramWithInput is mem = (mkProgram mem) { inputs = is }

runInterpreter :: ProgState -> ProgState
runInterpreter = execState (runProgramUntil OpFinished)

runInterpreterUntil :: OpCode -> ProgState -> ProgState
runInterpreterUntil op = execState (runProgramUntil op)

runProgram :: (MonadPointer m, MonadMemory m, MonadInput m, MonadOutput m, MonadRelative m) =>  m ()
runProgram = runProgramUntil OpFinished

runProgramUntil :: (MonadPointer m, MonadMemory m, MonadInput m, MonadOutput m, MonadRelative m) => OpCode -> m ()
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
initMemory prog (noun, verb) = let p = mkProgram prog in
  p { memory = memEdit 2 verb $ memEdit 1 noun $ memory p }

parseProgram :: Parser OpProgram
parseProgram = int `sepBy1` char ','

runOpCode :: (MonadPointer m, MonadMemory m, MonadInput m, MonadOutput m, MonadRelative m) => OpCode -> m ()
runOpCode OpFinished = return ()
runOpCode (OpAdd opA opB res)  = do
  p1 <- fetch opA
  p2 <- fetch opB
  o <- getBase
  modifyMem (memEdit (runRef o res) (p1 + p2))
  modifyPtr (+4)
runOpCode (OpMult opA opB res) = do
  p1 <- fetch opA
  p2 <- fetch opB
  o <- getBase
  modifyMem (memEdit (runRef o res) (p1 * p2))
  modifyPtr (+4)
runOpCode (OpInput res) = do
  v <- consumeInput
  o <- getBase
  modifyMem (memEdit (runRef o res) v)
  modifyPtr (+2)
runOpCode (OpOutput res) = do
  v <- fetch res
  writeOutput v
  modifyPtr (+2)
runOpCode (OpJmpT p1 p2) = do
  v1 <- fetch p1
  v2 <- fetch p2
  if v1 /= 0
  then modifyPtr (const v2)
  else modifyPtr (+3)
runOpCode (OpJmpF p1 p2) = do
  v1 <- fetch p1
  v2 <- fetch p2
  if v1 == 0
  then modifyPtr (const v2)
  else modifyPtr (+3)
runOpCode (OpLt p1 p2 p3) = do
  v1 <- fetch p1
  v2 <- fetch p2
  o <- getBase
  if v1 < v2
  then do
    modifyMem (memEdit (runRef o p3) 1)
    modifyPtr (+4)
  else do
    modifyMem (memEdit (runRef o p3) 0)
    modifyPtr (+4)
runOpCode (OpEq p1 p2 p3) = do
  v1 <- fetch p1
  v2 <- fetch p2
  o <- getBase
  if v1 == v2
  then do
    modifyMem (memEdit (runRef o p3) 1)
    modifyPtr (+4)
  else do
    modifyMem (memEdit (runRef o p3) 0)
    modifyPtr (+4)
runOpCode (OpRBase p1) = do
  v <- fetch p1
  modifyRelativeBase (+v)
  modifyPtr (+2)

memEdit :: Int -> Int -> Memory -> Memory
memEdit loc value (Memory m) = Memory $ M.alter (const $ Just value) loc  m

opReplace :: Int -> Int -> OpProgram -> OpProgram
opReplace 0 val (_ : os) = val : os
opReplace idx val (o : os) = o : opReplace (idx - 1) val os
opReplace _ _ [] = []

determineOpCode :: Int -> Ref -> Ref -> Ref -> OpCode
determineOpCode n = case n `mod` 100 of
  99 -> const $ const $ const OpFinished
  01 -> OpAdd
  02 -> OpMult
  03 -> \x _ _ -> OpInput x
  04 -> \x _ _ -> OpOutput x
  05 -> \x y _ ->  OpJmpT x y
  06 -> \x y _ -> OpJmpF x y
  07 -> OpLt
  08 -> OpEq
  09 -> \x _ _ -> OpRBase x
  x -> error ("no such opcode " ++ show x)

determineMode :: Int -> (Int -> Ref)
determineMode 0 = Position
determineMode 1 = Immediate
determineMode 2 = Relative
determineMode x = error ("unknown mode " ++ show x)

peekInstruction :: Memory -> Pointer -> OpCode
peekInstruction mem p = let
  n = address p mem
  op = determineOpCode n
  p1 = determineMode ((n `div`   100) `mod` 10) $ address (p + 1) mem 
  p2 = determineMode ((n `div`  1000) `mod` 10) $ address (p + 2) mem
  p3 = determineMode ((n `div` 10000) `mod` 10) $ address (p + 3) mem
  in op p1 p2 p3

nextInstruction :: (MonadMemory m, MonadPointer m) => m OpCode
nextInstruction = do
    m <- getMem
    Memory mem <- getMem
    p <- getPtr
    if p >= length mem
      then error "index out of bounds"
      else return $ peekInstruction m p

getMemory :: ProgState -> Memory
getMemory = memory

address :: Int -> Memory -> Int
address x (Memory m) = (M.!) m x

modifyMemory :: (Memory -> Memory) -> ProgState -> ProgState
modifyMemory f ps = ps { memory = f $ memory ps }
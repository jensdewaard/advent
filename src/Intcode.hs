{-# LANGUAGE InstanceSigs #-}
module Intcode (ProgState (ProgState, memory, ptr), OpProgram, runOpProgram, parseProgram, opReplace, LogM (..) ) where

import Text.ParserCombinators.Parsec

type OpProgram = [Int]

data OpCode =
    OpFinished
    | OpAdd Int Int Int
    | OpMult Int Int Int
    deriving Eq

runOpProgram :: Monad m => ProgState -> m ProgState
runOpProgram s = do 
    let i = readInstruction s 
    s' <- runOpCode i s
    if i == OpFinished 
        then pure s 
        else runOpProgram s'


data ProgState = ProgState 
    { memory    :: OpProgram
    , ptr       :: Int
    } deriving Eq

parseProgram :: Parser OpProgram
parseProgram = (read <$> many1 digit) `sepBy1` char ','

runOpCode :: Monad m => OpCode -> ProgState -> m ProgState
runOpCode OpFinished prog = pure prog
runOpCode (OpAdd opA opB res) prog@(ProgState { memory = mem, ptr = p } ) = pure $ prog { memory = opReplace res ((mem !! opA) + (mem !! opB)) mem, ptr = p+4 }
runOpCode (OpMult opA opB res) prog@(ProgState { memory = mem, ptr = p} ) = pure $ prog { memory = opReplace res ((mem !! opA) * (mem !! opB)) mem, ptr = p+4 }

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

data LogM a = LogM
    { output :: [String]
    , state :: a
    }

instance Functor LogM where
  fmap :: (a -> b) -> LogM a -> LogM b
  fmap f (LogM o a) = LogM o (f a)

instance Applicative LogM where
  pure :: a -> LogM a
  pure = LogM []
  (<*>) :: LogM (a -> b) -> LogM a -> LogM b
  (<*>) (LogM o f) (LogM o' a) = LogM (o ++ o') (f a)

instance Monad LogM where
  (>>=) :: LogM a -> (a -> LogM b) -> LogM b
  (>>=) (LogM o s) f = let (LogM o' s') = f s in LogM (o ++ o') s'
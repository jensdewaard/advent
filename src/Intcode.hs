{-# LANGUAGE InstanceSigs #-}
module Intcode (ProgState (ProgState, memory, ptr), OpProgram, runOpProgram, parseProgram, opReplace, LogM (..) ) where

import Text.ParserCombinators.Parsec

type OpProgram a = [a]
data Op = Multiply | Addition deriving Eq
data Value = Const Int | Var Char | Eqn Op Value Value deriving Eq

instance Num Value where
  (+) :: Value -> Value -> Value
  (+) (Const a) (Const b) = Const (a + b)
  (+) va vb = Eqn Addition va vb
  (*) :: Value -> Value -> Value
  (*) (Const a) (Const b) = Const (a * b)
  (*) va vb = Eqn Multiply va vb
  abs :: Value -> Value
  abs = undefined
  signum :: Value -> Value
  signum = undefined
  fromInteger :: Integer -> Value
  fromInteger = Const . fromInteger
  negate :: Value -> Value
  negate = undefined

data OpCode a =
    OpFinished
    | OpAdd a a a
    | OpMult a a a
    deriving Eq

runOpProgram :: (Eq a, Num a, Monad m, Ref a) => ProgState a -> m (ProgState a)
runOpProgram s = do
    let i = readInstruction s
    s' <- runOpCode i s
    if i == OpFinished
        then pure s
        else runOpProgram s'


data ProgState a =  ProgState
    { memory    :: OpProgram a
    , ptr       :: Int
    } deriving Eq

parseProgram :: Parser (OpProgram Int)
parseProgram = parseValue `sepBy1` char ','

parseValue :: Read a => Parser a
parseValue = do
    read <$> many1 digit

class Ref a where
    get :: [a] -> a -> a

instance Ref Int where
    get :: [Int] -> Int -> Int
    get l x = l !! x

instance Ref Value where
  get :: [Value] -> Value -> Value
  get l (Const n) = l !! n

runOpCode :: (Eq a, Num a, Monad m, Ref a) => OpCode a -> ProgState a -> m (ProgState a)
runOpCode OpFinished prog = pure prog
runOpCode (OpAdd opA opB res) prog@(ProgState { memory = mem, ptr = p } ) = pure $ prog { memory = opReplace res (get mem opA + get mem opB) mem, ptr = p+4 }
runOpCode (OpMult opA opB res) prog@(ProgState { memory = mem, ptr = p} ) = pure $ prog { memory = opReplace res (get mem opA * get mem opB) mem, ptr = p+4 }

opReplace :: (Eq a, Num a) => a -> a -> OpProgram a -> OpProgram a
opReplace 0 val (_ : os) = val : os
opReplace idx val (o : os) = o : opReplace (idx - 1) val os
opReplace _ _ [] = []

readInstruction :: (Eq a, Num a) => ProgState a -> OpCode a
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
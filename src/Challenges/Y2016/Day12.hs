{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE GADTs #-}
module Challenges.Y2016.Day12 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.Prelude
import Common.Parsing (int)
import Data.Map (Map)
import qualified Data.Map as Map

solutionA :: String -> String
solutionA = solve parser (a_ . runProgram (startState 0))
solutionB :: String -> String
solutionB = solve parser (a_ . runProgram (startState 1))

runProgram :: ProgramState -> [Instruction] ->  ProgramState
runProgram ps@ProgramState{ptr = p} is =
    if p >= length is || p < 0 then ps 
    else let ps' = exec ps (is !! p) in runProgram ps' is

a_ :: ProgramState -> Int
a_ ProgramState { memory = m } = m Map.! A

data ProgramState = ProgramState { 
    ptr :: Int,
    memory :: Map Register Int
    } deriving (Eq, Show)

startState :: Int -> ProgramState
startState i = ProgramState { ptr = 0, memory = m} where
    m = Map.fromList [(A,0),(B,0), (C,i), (D,0)]

exec :: ProgramState -> Instruction -> ProgramState
exec m (Half r) = m {
    ptr = succ $ ptr m,
    memory = Map.adjust (`div` 2) r (memory m)
}
exec m (Triple r) = m {
    ptr = succ $ ptr m,
    memory = Map.adjust (*3) r (memory m)
}
exec m (Increment r) = m {
    ptr = succ $ ptr m,
    memory = Map.adjust succ r (memory m)
}
exec m (Decrement r) = m {
    ptr = succ $ ptr m,
    memory = Map.adjust pred r (memory m)
                         }
exec m (Jump o) = m {
    ptr = ptr m + o
}
exec m (JumpIfEven r o) = m {
    ptr = ptr m + if even (memory m Map.! r) then o else 1
}
exec m (JumpIfOne r o) = m {
    ptr = ptr m + if memory m Map.! r == 1 then o else 1
}
exec m (JumpNotZeroReg r o) = m {
    ptr = ptr m + if (/=0) (memory m Map.! r) then o else 1
                             }
exec m (JumpNotZeroVal v o) = m {
    ptr = ptr m + if v/=0 then o else 1
                             }
exec m (CopyReg r r') = m {
    ptr = succ $ ptr m,
    memory = Map.adjust (const (memory m Map.! r)) r' (memory m)
                          }
exec m (CopyVal v r) = m {
    ptr = succ $ ptr m,
    memory = Map.adjust (const v) r (memory m)
                         }

data Register = A | B | C | D deriving (Eq, Ord, Show)
data Instruction = Half Register
    | Triple Register
    | Increment Register
    | Decrement Register
    | CopyReg Register Register
    | CopyVal Int Register
    | Jump Int
    | JumpIfEven Register Int
    | JumpIfOne Register Int
    | JumpNotZeroReg Register Int
    | JumpNotZeroVal Int Int
    deriving (Eq, Ord, Show)

parser :: Parser [Instruction]
parser = instruction `sepEndBy1` newline

instruction :: Parser Instruction
instruction =
    (do _ <- string "inc " ; Increment <$> register)
    <|> (do _ <- string "hlf "; Half <$> register)
    <|> (do _ <- string "tpl "; Triple <$> register)
    <|> (do _ <- string "dec "; Decrement <$> register)
    <|> try (do _ <- string "jmp "; Jump <$> (optional (char '+') >> int))
    <|> try (do
        _ <- string "jie "
        r <- register
        _ <- string ", "
        JumpIfEven r <$> (optional (char '+') >> int)
        )
    <|> try (do
        _ <- string "jio "
        r <- register
        _ <- string ", "
        JumpIfOne r <$> (optional (char '+') >> int)
        )
    <|> try (do
        _ <- string "jnz "
        r <- register
        _ <- string " "
        JumpNotZeroReg r <$> int)
    <|> try (do
        _ <- string "jnz "
        v <- int
        _ <- string " "
        JumpNotZeroVal v <$> int)
    <|> try (do
        _ <- string "cpy "
        r <- register
        _ <- string " "
        CopyReg r <$> register)
    <|> try (do
        _ <- string "cpy "
        v <- int
        _ <- string " "
        CopyVal v <$> register)

register :: Parser Register
register = (char 'a' >> return A) <|> 
    (char 'b' >> return B) <|> 
        (char 'c' >>  return C) <|> 
            (char 'd' >> return D)

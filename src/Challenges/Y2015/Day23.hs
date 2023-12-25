{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE GADTs #-}
module Challenges.Y2015.Day23 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve)
import Parsing (int)
import Data.Map (Map)
import qualified Data.Map as Map

solutionA :: String -> String
solutionA = solve parser (b . runProgram startState)
solutionB :: String -> String
solutionB = solve parser (b . runProgram startStateB)

runProgram :: ProgramState -> [Instruction] ->  ProgramState
runProgram ps@ProgramState{ptr = p} is =
    if p >= length is || p < 0 then ps 
    else let ps' = exec ps (is !! p) in runProgram ps' is

b :: ProgramState -> Int
b ProgramState { ptr = _, memory = m } = m Map.! B

data ProgramState = ProgramState { 
    ptr :: Int,
    memory :: Map Register Int
    } deriving (Eq, Show)

startState = ProgramState { ptr = 0, memory = m} where
    m = Map.fromList [(A,0),(B,0)]

startStateB = ProgramState { ptr = 0, memory = m} where
    m = Map.fromList [(A,1),(B,0)]

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
exec m (Jump o) = m {
    ptr = ptr m + o
}
exec m (JumpIfEven r o) = m {
    ptr = ptr m + if even (memory m Map.! r) then o else 1
}
exec m (JumpIfOne r o) = m {
    ptr = ptr m + if memory m Map.! r == 1 then o else 1
}

data Register = A | B deriving (Eq, Ord, Show)
data Instruction = Half Register
    | Triple Register
    | Increment Register
    | Jump Int
    | JumpIfEven Register Int
    | JumpIfOne Register Int
    deriving (Eq, Ord, Show)

parser :: Parser [Instruction]
parser = instruction `sepEndBy1` newline

instruction :: Parser Instruction
instruction =
    (do _ <- string "inc " ; Increment <$> register; )
    <|> (do _ <- string "hlf "; Half <$> register;)
    <|> (do _ <- string "tpl "; Triple <$> register;)
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

register :: Parser Register
register = (char 'a' >> return A) <|> (char 'b' >> return B)
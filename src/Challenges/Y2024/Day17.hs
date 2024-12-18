module Challenges.Y2024.Day17 (solutionA, solutionB) where

import Common.Parsing (int)
import Common.Prelude (solve)
import Control.Arrow ((&&&), (>>>))
import Data.Bits (shiftL, xor)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec (Parser, char, newline, sepBy, string)

solutionA :: String -> String
-- solutionA = solve parser (runUntilHalt >>> reverse . outputs)
solutionA = solve parser (runUntilHalt >>> program)

solutionB :: String -> String
-- solutionB = solve parser (runOnA 241475241472349 >>> reverse . outputs)
solutionB = solve parser (\cpu -> earliestClone (program cpu) cpu)

-- solutionB = solve parser (runOnA 729 >>> reverse . outputs)

runOnA :: Int -> CpuState -> CpuState
runOnA a = runUntilHalt . setRegister A a

earliestClone :: [Int] -> CpuState -> Int
earliestClone wanted cpu =
  minimum
    [ a'
      | a' <- [1 .. 1000],
        let cpu' = runUntilHalt (setRegister A a' cpu),
        let out = take 3 (outputs cpu'),
        out == take 3 (reverse wanted)
    ]

data Register = A | B | C deriving (Eq, Show, Ord)

class HasPC a where
  getPC :: a -> Int
  modifyPC :: (Int -> Int) -> a -> a

data CpuState = CpuState
  { program :: [Int],
    programCounter :: Int,
    registers :: Map Register Int,
    outputs :: [Int],
    halted :: Bool
  }
  deriving (Show)

mkCpu :: [Int] -> Map Register Int -> CpuState
mkCpu p regs = CpuState p 0 regs [] False

data Operand
  = Lit Int
  | RegA
  | RegB
  | RegC

data Instruction
  = Adv Operand
  | Bxl Operand
  | Bst Operand
  | Jnz Operand
  | Bxc Operand
  | Out Operand
  | Bdv Operand
  | Cdv Operand

instance HasPC CpuState where
  getPC = programCounter
  modifyPC f cpu = cpu {programCounter = f (programCounter cpu)}

runUntilHalt :: CpuState -> CpuState
runUntilHalt cpu
  | halted cpu = cpu
  | otherwise = runUntilHalt (runCycle cpu)

runCycle :: CpuState -> CpuState
runCycle cpu =
  let (i1, i2, cpu') = fetch cpu
      instruction = decode i1 i2
   in execute instruction cpu'

fetch :: CpuState -> (Int, Int, CpuState)
fetch cpu =
  let pc = getPC cpu
      v1 = program cpu !! pc
      v2 = program cpu !! (pc + 1)
      cpu' = modifyPC (+ 2) cpu
   in if pc < length (program cpu)
        then (v1, v2, cpu')
        else (0, 0, cpu {halted = True})

combo :: Int -> Operand
combo 4 = RegA
combo 5 = RegB
combo 6 = RegC
combo 7 = error "reserved operand do not use"
combo n = Lit n

decode :: Int -> Int -> Instruction
decode 0 o = Adv (combo o)
decode 1 o = Bxl (Lit o)
decode 2 o = Bst (combo o)
decode 3 o = Jnz (Lit o)
decode 4 o = Bxc (Lit o)
decode 5 o = Out (combo o)
decode 6 o = Bdv (combo o)
decode 7 o = Cdv (combo o)
decode o _ = error ("invalid opcode " ++ show o)

getRegister :: Register -> CpuState -> Int
getRegister r cpu = fromJust $ M.lookup r (registers cpu)

setRegister :: Register -> Int -> CpuState -> CpuState
setRegister r n cpu = cpu {registers = M.update (const $ Just n) r (registers cpu)}

getValue :: Operand -> CpuState -> Int
getValue (Lit n) _ = n
getValue RegA cpu = fromJust $ M.lookup A (registers cpu)
getValue RegB cpu = fromJust $ M.lookup B (registers cpu)
getValue RegC cpu = fromJust $ M.lookup C (registers cpu)

execute :: Instruction -> CpuState -> CpuState
execute _ cpu
  | halted cpu = cpu
execute (Adv o) cpu =
  let numer = getRegister A cpu
      denom = 2 ^ getValue o cpu
   in setRegister A (numer `div` denom) cpu
execute (Bxl o) cpu =
  let v = getValue o cpu `xor` getRegister B cpu
   in setRegister B v cpu
execute (Bst o) cpu =
  let v = getValue o cpu `mod` 8
   in setRegister B v cpu
execute (Jnz o) cpu =
  if getRegister A cpu == 0
    then cpu
    else modifyPC (const (getValue o cpu)) cpu
execute (Bxc _) cpu =
  let v = getRegister B cpu `xor` getRegister C cpu
   in setRegister B v cpu
execute (Out o) cpu =
  let v = getValue o cpu `mod` 8
   in cpu {outputs = v : outputs cpu}
execute (Bdv o) cpu =
  let numer = getRegister A cpu
      denom = 2 ^ getValue o cpu
   in setRegister B (numer `div` denom) cpu
execute (Cdv o) cpu =
  let numer = getRegister A cpu
      denom = 2 ^ getValue o cpu
   in setRegister C (numer `div` denom) cpu

parser :: Parser CpuState
parser = do
  a <- string "Register A: " *> int <* newline
  b <- string "Register B: " *> int <* newline
  c <- string "Register C: " *> int <* newline
  _ <- newline
  ops <- string "Program: " *> int `sepBy` char ','
  return $ mkCpu ops (M.fromList [(A, a), (B, b), (C, c)])

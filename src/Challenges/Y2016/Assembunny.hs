module Challenges.Y2016.Assembunny (runProgram, mkProgram, Instruction, parseInstructionSet, a_) where
import Common.Parsing (int)
import Data.Map (Map)
import Data.Maybe (isJust, fromJust)
import Common.List (replace)
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

runProgram :: ProgramState ->  ProgramState
runProgram ps@ProgramState{ptr = p} =
    if p >= length (ins ps) || p < 0
       then ps
       else let ps' = exec (ins ps !! p) ps in runProgram ps'

mkProgram :: [Int] -> [Instruction] -> ProgramState
mkProgram mems instructions = ProgramState {
    ptr = 0,
    ins = instructions,
    memory = Map.fromList $ zip [A,B,C,D] mems
    }

data ProgramState = ProgramState {
    ptr :: Int,
    ins :: [Instruction],
    memory :: Map Register Int
    } deriving (Eq, Show)

data Register = A | B | C | D deriving (Eq, Ord, Show)
data Instruction = Increment Register
    | Decrement Register
    | CopyReg Register Register
    | CopyVal Int Register
    | CopyInv1 Int Int
    | CopyInv2 Register Int
    | JumpNotZeroReg Register Int
    | JumpNotZeroVal Int Int
    | JumpNotZeroInv1 Register Register
    | JumpNotZeroInv2 Int Register
    | Toggle Register
    deriving (Eq, Ord, Show)

noop :: ProgramState -> ProgramState
noop m = m { ptr = succ $ ptr m }

exec :: Instruction -> ProgramState -> ProgramState
exec (Increment r) m = m {
    ptr = succ $ ptr m,
    memory = Map.adjust succ r (memory m)
}
exec (Decrement r) m = m {
    ptr = succ $ ptr m,
    memory = Map.adjust pred r (memory m)
                         }
exec (JumpNotZeroReg r o) m = m {
    ptr = ptr m + if (/=0) (memory m Map.! r) then o else 1
                             }
exec (JumpNotZeroVal v o) m = m {
    ptr = ptr m + if v/=0 then o else 1
                             }
exec (JumpNotZeroInv1 _ _) m = noop m
exec (JumpNotZeroInv2 _ _) m = noop m
exec (CopyReg r r') m = m {
    ptr = succ $ ptr m,
    memory = Map.adjust (const (memory m Map.! r)) r' (memory m)
                          }
exec (CopyVal v r) m = m {
    ptr = succ $ ptr m,
    memory = Map.adjust (const v) r (memory m)
                         }
exec (Toggle i)     m =  let
        ix = (ptr m + memory m Map.! i)
        mti = if ix > 0 && ix < length (ins m) then Just $ toggle (ins m !! ix) else Nothing
                         in if isJust mti then m {
                             ptr = succ $ ptr m,
                             ins = replace ix (fromJust mti) (ins m)
                             } else noop m
exec (CopyInv1 _ _) m = noop m
exec (CopyInv2 _ _) m = noop m

toggle :: Instruction -> Instruction
toggle (Increment r) = Decrement r
toggle (Decrement r) = Increment r
toggle (Toggle    r) = Increment r
toggle (JumpNotZeroReg  r n) = CopyInv2 r n
toggle (JumpNotZeroVal  v n) = CopyInv1 v n
toggle (JumpNotZeroInv1 r s) = CopyReg r s
toggle (JumpNotZeroInv2 r s) = CopyVal r s
toggle (CopyReg         r s) = JumpNotZeroInv1 r s
toggle (CopyVal         v r) = JumpNotZeroInv2 v r
toggle (CopyInv1        v n) = JumpNotZeroVal v n
toggle (CopyInv2        r n) = JumpNotZeroReg r n

instruction :: Parser Instruction
instruction =
    (do _ <- string "inc " ; Increment <$> register)
    <|> (do _ <- string "dec "; Decrement <$> register)
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
        _ <- string "jnz "
        v <- register
        _ <- string " "
        JumpNotZeroInv1 v <$> register)
    <|> try (do
        _ <- string "jnz "
        v <- int
        _ <- string " "
        JumpNotZeroInv2 v <$> register)
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
    <|> try (do
        _ <- string "tgl "; Toggle <$> register)


register :: Parser Register
register = (char 'a' >> return A) <|>
    (char 'b' >> return B) <|>
        (char 'c' >>  return C) <|>
            (char 'd' >> return D)

a_ :: ProgramState -> Int
a_ ProgramState { memory = m } = m Map.! A

parseInstructionSet :: Parser [Instruction]
parseInstructionSet = instruction `sepEndBy` newline

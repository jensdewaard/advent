module Challenges.Y2024.Day03 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, char, string, (<|>), try, anyChar, manyTill, optional, sepEndBy1, lookAhead, eof)
import Control.Arrow ((>>>))
import Common.Parsing (int)
import Control.Monad (void)
import Text.Parsec.Char (string')

solutionA :: String -> String
solutionA = solve parser (foldl sumMul 0)
solutionB :: String -> String
solutionB = solve parser (foldl sumMulB (True, 0) >>> snd)

sumMul :: Int -> Instruction -> Int
sumMul n Do = n
sumMul n Dont = n
sumMul n (Mul x y) = n + x * y

sumMulB :: (Bool, Int) -> Instruction -> (Bool, Int)
sumMulB (_, n) Do = (True, n)
sumMulB (_, n) Dont = (False, n)
sumMulB (True, n) (Mul x y) = (True, n + x * y)
sumMulB (False, n) (Mul _ _) = (False, n)

data Instruction = Do | Dont | Mul Int Int deriving Show

parser :: Parser [Instruction]
parser = optional garbage >> instruction `sepEndBy1` garbage

garbage :: Parser ()
garbage = void (manyTill anyChar (void (lookAhead instruction) <|> eof))

instruction :: Parser Instruction
instruction = try doI <|> try dont <|> try mul

doI :: Parser Instruction
doI = string' "do()" >> return Do

dont :: Parser Instruction
dont = string' "don't()" >> return Dont

mul :: Parser Instruction
mul = do
    _ <- string "mul("
    x <- int
    _ <- char ','
    y <- int
    _ <- char ')'
    return $ Mul x y
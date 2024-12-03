module Challenges.Y2024.Day03 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, char, string, alphaNum, (<|>), many, try, space, anyChar)
import Control.Arrow ((>>>))
import Common.Parsing (int, symbol, brackets)
import Data.Maybe (catMaybes)

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
parser = do
    x <- many (try mul <|> try doI <|> try dont <|> garbage)
    return $ catMaybes x

garbage :: Parser (Maybe Instruction)
garbage = anyChar >> return Nothing

doI :: Parser (Maybe Instruction)
doI = do
    _ <- string "do()"
    return $ Just Do

dont :: Parser (Maybe Instruction)
dont = do
    _ <- string "don't()"
    return $ Just Dont

mul :: Parser (Maybe Instruction)
mul = do
    _ <- string "mul("
    x <- int
    _ <- char ','
    y <- int
    _ <- char ')'
    return $ Just $ Mul x y
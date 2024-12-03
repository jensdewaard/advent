module Challenges.Y2024.Day03 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, char, string, alphaNum, (<|>), many, try, space)
import Control.Arrow ((>>>))
import Common.Parsing (int, symbol, brackets)
import Data.Maybe (catMaybes)

solutionA :: String -> String
solutionA = solve parser (map sumMul >>> sum)
solutionB :: String -> String
-- solutionB = solve parser (sumMulB True)
solutionB = solve parser (sumMulB True)

sumMul :: Instruction -> Int
sumMul Do = 0
sumMul Dont = 0
sumMul (Mul x y) = x * y

sumMulB :: Bool -> [Instruction] -> Int
sumMulB _ [] = 0
sumMulB _ (Do:is) = sumMulB True is
sumMulB _ (Dont:is) = sumMulB False is
sumMulB count (Mul x y:is) = (if count then x * y else 0) + sumMulB count is

data Instruction = Do | Dont | Mul Int Int deriving Show

parser :: Parser [Instruction]
parser = do
    x <- many (try mul <|> try doI <|> try dont <|> garbage)
    return $ catMaybes x

garb :: Parser Char
garb = do
    alphaNum <|> symbol <|> brackets <|> char ','  
    <|> space <|> char '_' 
    <|> char '/' <|> char '\\' <|> char '\'' 

garbage :: Parser (Maybe Instruction)
garbage = do
    _ <- garb
    return Nothing

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
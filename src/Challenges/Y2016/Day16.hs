module Challenges.Y2016.Day16 (solutionA, solutionB) where

import Common.Prelude
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = solve parser (checksum . gen 272)

solutionB :: String -> String
solutionB = solve parser (checksum . gen 35651584)

parser :: Parser [Bit]
parser = many1 bit

bit :: Parser Bit
bit = (char '1' >> return I) <|> (char '0' >> return O)

curve :: [Bit] -> [Bit]
curve a = a ++ [O] ++ map flipB (reverse a)

gen :: Int -> [Bit] -> [Bit]
gen l i = if length i >= l then take l i else gen l (curve i)

data Bit = O | I deriving (Eq)

instance Show Bit where
  show I = "1"
  show O = "0"

flipB :: Bit -> Bit
flipB O = I
flipB I = O

checksum :: [Bit] -> [Bit]
checksum a
  | odd $ length a = a
  | otherwise = checksum $ ch' a
  where
    ch' [] = []
    ch' [_] = error "ch' called on list of odd length"
    ch' (b1 : b2 : bs) = if b1 == b2 then I : ch' bs else O : ch' bs

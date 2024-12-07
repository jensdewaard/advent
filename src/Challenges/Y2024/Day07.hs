{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Challenges.Y2024.Day07 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, newline, string, sepEndBy1, sepBy1, char)
import Control.Arrow ((>>>))
import Data.Maybe (mapMaybe)
import Common.Parsing (int)
import Control.Applicative (asum)

solutionA :: String -> String
solutionA = solve parser (mapMaybe (canCalibrate [(+),(*)]) >>> sum)
solutionB :: String -> String
solutionB = solve parser (mapMaybe (canCalibrate [(+),(*), concatOp]) >>> sum)

data Equation = Eqn Int [Int] deriving (Eq, Show)

type Operator = Int -> Int -> Int
canCalibrate :: [Operator] -> Equation -> Maybe Int
canCalibrate _ (Eqn _ []) = Nothing
canCalibrate _ (Eqn total [a]) = if a == total then Just total else Nothing
canCalibrate ops (Eqn total (v:w:vs)) = asum (map (\op -> canCalibrate ops (Eqn total (op v w:vs))) ops)

concatOp :: Int -> Int -> Int
concatOp a b = let
    s = show a
    t = show b in read (s <> t)

parser :: Parser [Equation]
parser = equation `sepEndBy1` newline where
    equation = do
        total <- int
        _ <- string ": "
        ns <- int `sepBy1` char ' '
        return (Eqn total ns)
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Challenges.Y2019.Day02 (solutionA, solutionB) where

import Common.Prelude ( (==>) )
import Intcode
    ( Memory, parseProgram, runInterpreter, initMemory, mkProgram, getMemory, memEdit, modifyMemory, address)
import Data.Bifunctor (second)

solutionA :: String -> String
solutionA = parseProgram ==> address 0 . getMemory . runInterpreter . modifyMemory (memEdit 2 2 . memEdit 1 12) . mkProgram

solutionB :: String -> String
solutionB = parseProgram ==> (\prog ->
    answer
    $ fst
    $ head
    $ filter isCorrect
    $ map (\s -> second (getMemory . runInterpreter) (s, initMemory prog s)) posSol
    )

answer :: (Int, Int) -> Int
answer (n, v) = 100 * n + v


isCorrect :: ((Int, Int), Memory) -> Bool
isCorrect (_, p) = address 0 p == 19690720

posSol :: [(Int, Int)]
posSol = [(x, y) | x <- [0..100], y <- [0..100]]
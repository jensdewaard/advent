{-# LANGUAGE GADTs #-}
module Challenges.Y2019.Day02 (solutionA, solutionB) where

import Common.Prelude
import Intcode
import Data.Bifunctor (second)

solutionA :: String -> String
solutionA = parseProgram ==> (head . memory . state . runOpProgram . mkProg)

mkProg :: OpProgram Int -> ProgState Int
mkProg = snd . flip initMemory (12,2)


solutionB :: String -> String
solutionB = parseProgram ==> (\prog ->
    answer
    $ fst
    $ head
    $ filter isCorrect
    $ map (second (state . runOpProgram) . initMemory prog) posSol)

answer :: (Int, Int) -> Int
answer (n, v) = 100 * n + v

initMemory :: OpProgram Int -> (Int, Int) -> ((Int, Int), ProgState Int)
initMemory prog (noun, verb) = ((noun, verb), ProgState { memory = opReplace 2 verb $ opReplace 1 noun prog, ptr = 0})

isCorrect :: ((Int, Int), ProgState Int) -> Bool
isCorrect (_, p) = head (memory p) == 19690720

posSol :: [(Int, Int)]
posSol = [(x, y) | x <- [0..100], y <- [0..100]]
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Challenges.Y2019.Day02 (solutionA, solutionB) where

import Common.Prelude ( (==>) )
import Intcode
    ( ProgState(..), OpProgram, parseProgram, runProgram, initMemory )
import Data.Bifunctor (second)

solutionA :: String -> String
solutionA = parseProgram ==> head . memory . fst . runProgram . mkProg

mkProg :: OpProgram -> ProgState
mkProg = flip initMemory (12,2)


solutionB :: String -> String
solutionB = parseProgram ==> (\prog ->
    answer
    $ fst
    $ head
    $ filter isCorrect
    $ map (\s -> second (memory . fst . runProgram) (s, initMemory prog s)) posSol
    )

answer :: (Int, Int) -> Int
answer (n, v) = 100 * n + v


isCorrect :: ((Int, Int), OpProgram) -> Bool
isCorrect (_, p) = head p == 19690720

posSol :: [(Int, Int)]
posSol = [(x, y) | x <- [0..100], y <- [0..100]]
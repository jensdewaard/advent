module Challenges.Y2019.Day02 (input, solutionA, solutionB) where

import Intcode
import qualified Data.Bifunctor

solutionA :: String -> String
solutionA inp = show $ head $ memory $ runOpProgram prog' where
    prog' = snd $ initMemory (parseProgram inp) (12, 2)

solutionB :: String -> String
solutionB inp = show $ answer $ fst $
    head $
    filter isCorrect $ map (Data.Bifunctor.second runOpProgram . initMemory prog) posSol where
    prog = parseProgram inp

answer :: (Int, Int) -> Int
answer (n, v) = 100 * n + v

initMemory :: OpProgram -> (Int, Int) -> ((Int, Int), ProgState)
initMemory prog (noun, verb) = ((noun, verb), ProgState { memory = opReplace 2 verb $ opReplace 1 noun prog, ptr = 0})

isCorrect :: ((Int, Int), ProgState) -> Bool
isCorrect (_, p) = head (memory p) == 19690720

posSol :: [(Int, Int)]
posSol = [(x, y) | x <- [0..100], y <- [0..100]]

input :: Bool -> IO String
input True = return "1,9,10,3,2,3,11,0,99,30,40,50"
input False = readFile "data/2019/02.txt"

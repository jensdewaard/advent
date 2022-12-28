module Main (main) where

import Data.Maybe
import qualified Challenges.Y2022.Day01 as D1

main :: IO ()
main = do
    let day = 1
    contents <- readFile "data/2022/01.txt"
    let input = parse day contents
    print $ solveA day input
    print $ solveB day input

parse :: Int -> (String -> [D1.Elf])
parse 1 = fromJust . D1.maybeFromEither . D1.parseFile

solveA :: Int -> ([D1.Elf] -> Int)
solveA 1 = D1.partA

solveB :: Int -> ([D1.Elf] -> Int)
solveB 1 = D1.partB

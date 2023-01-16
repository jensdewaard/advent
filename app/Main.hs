module Main (main) where

import qualified Challenges.Y2022.Day16 as D16
import Data.Graph.Inductive (labfilter, prettyPrint)
import Challenges.Y2022.Day16 (rmNode, toMatrix, solveA)
import FloydWarshall (floydwarshall)

main :: IO ()
main = do
    contents <- readFile "data/2022/01.txt"
    let input = D16.parse contents
    let g = D16.toGraph D16.world
    let n = 70070
    let g' = rmNode g n
    let g'' = rmNode g' 71071
    let g''' = rmNode g'' 73073
    prettyPrint g
    -- putStrLn "------"
    -- prettyPrint $ g'
    putStrLn "------"
    prettyPrint $ g'''
    -- print $ D16.solveB input
    let dm = toMatrix g'''
    putStrLn "------"
    print dm
    let wfi = floydwarshall dm
    putStrLn "------"
    print wfi
    putStrLn "------"
    print $ D16.solveA D16.world
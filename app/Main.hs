module Main (main) where

import qualified Challenges.Y2022.Day16 as D16
import qualified Challenges.Y2022.Day16 as D16
import Data.Graph.Inductive (labfilter, prettyPrint)
import Challenges.Y2022.Day16 (rmNode)

main :: IO ()
main = do
    contents <- readFile "data/2022/01.txt"
    let input = D16.parse contents
    let g = D16.toGraph D16.world
    let n = 70070
    let g' = rmNode g n
    let g'' = rmNode g' 66066
    prettyPrint g
    putStrLn "------"
    prettyPrint $ g'
    putStrLn "------"
    prettyPrint $ g''
    -- print $ D16.solveB input

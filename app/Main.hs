module Main (main) where

import qualified Challenges.Y2022.Day15 as D15

main :: IO ()
main = do
    contents <- readFile "data/2022/01.txt"
    let input = D15.parse contents
    print $ D15.solveA input
    print $ D15.solveB input

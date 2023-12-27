module Challenges.Y2023.Day13 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.Prelude ((==>))
import Data.List (transpose)

solutionA :: String -> String
solutionA = parser ==> (foldl sumM 0 . concatMap (solve (all ((==0) . uncurry mirrored))))
solutionB :: String -> String
solutionB = parser ==> (foldl sumM 0 . concatMap (solve solveB))

type Field = [String]

data Mirror = Hor Int | Vert Int deriving (Eq, Show)

sumM :: Int -> Mirror -> Int
sumM n (Vert m) = n + m
sumM n (Hor m)  = n + 100*m

solveB :: [(String, String)] -> Bool
solveB ss = 1 == sum (map (uncurry mirrored) ss)

solve :: ([(String, String)] -> Bool) -> Field -> [Mirror]
solve f s = [m | n <- [1..(length (s !! 1) - 1)],
               let ss = map (splitAt n) s,
               f ss,
               let m = Vert n]
            ++ [m | let s' = transpose s,
                    n <- [1..(length (s' !! 1) - 1)],
                    let ss = map (splitAt n) s',
                    f ss,
                    let m = Hor n]

mirrored :: Eq a => [a] -> [a] -> Int
mirrored [] _ = 0
mirrored _ [] = 0
mirrored as (b:bs) = mirrored (init as) bs + if last as == b then 0 else 1 

parser :: Parser [Field]
parser = do field `sepBy1` newline
    where field = do many1 (char '.' <|> char '#') `sepEndBy1` newline
module Challenges.Y2015.Day14 (solutionA, solutionB) where

import Text.ParserCombinators.Parsec
import Common.Prelude
import Common.List (occur)

solutionA :: String -> String
solutionA = solve parser $ maximum . map (snd . distAt 2503)
solutionB :: String -> String
solutionB =  solve parser (\rs -> maximum2 [] $ occur $ map fst $ concatMap (leadersAt rs) [1..2503])

parser :: Parser [Reindeer]
parser = sepEndBy1 reindeer newline where
    reindeer = do
        n <- many1 letter
        _ <- string " can fly "
        s <- many1 digit
        _ <- string " km/s for "
        f <- many1 digit
        _ <- string " seconds, but then must rest for "
        r <- many1 digit
        _ <- string " seconds."
        return (n, read s, read f, read r)

--               Name |Speed|Flight|Rest
type Reindeer = (String, Int, Int, Int)

cycleTime :: Reindeer -> Int
cycleTime (_,_,f,r) = f + r

leadersAt :: [Reindeer] -> Int -> [(String, Int)]
leadersAt rs t = maximum2 [] $ map (distAt t) rs

distAt :: Int -> Reindeer -> (String, Int)
distAt t r@(n, s, f, _)
  | t > cycleTime r = (n, s * f + snd (distAt (t - cycleTime r) r))
  | t > f = (n,s * f)
  | otherwise = (n,s * t)

maximum2 :: Ord b => [(a,b)] -> [(a,b)] -> [(a,b)]
maximum2 [] [] = error "empty list"
maximum2 ps [] = ps
maximum2 [] (p:cs) = maximum2 [p] cs
maximum2 cur@((_,b):_) ((a',b'):cs) = case compare b' b of
    GT -> maximum2 [(a',b')] cs
    LT -> maximum2 cur cs
    EQ -> maximum2 (cur ++ [(a',b')]) cs
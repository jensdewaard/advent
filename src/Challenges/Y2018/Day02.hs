module Challenges.Y2018.Day02 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, many1, letter, sepEndBy1, newline)
import Control.Arrow ((>>>), (&&&))
import Common.List (occur)

solutionA :: String -> String
solutionA = solve parser (map occur
    >>> map (any ((==2) . snd) &&& any ((==3) . snd))
    >>> count
    >>> uncurry (*)
    )

count :: [(Bool, Bool)] -> (Int, Int)
count = go (0,0) where
    go (n,m) [] = (n,m)
    go (n,m) ((True,False):bs) = go (n+1, m) bs
    go (n,m) ((True,True):bs) = go (n+1, m+1) bs
    go (n,m) ((False,True):bs) = go (n, m+1) bs
    go (n,m) ((False,False):bs) = go (n, m) bs

solutionB :: String -> String
solutionB = solve parser (findClosest >>> uncurry same)

findClosest :: [String] -> (String, String)
findClosest ss = head $ [(s,t) | s <- ss, t <- ss, difference s t == 1]

same :: String -> String -> String
same [] _ = []
same _ [] = []
same (s:ss) (t:tt) = if s == t then s : same ss tt else same ss tt

difference :: String -> String -> Int
difference [] [] = 0
difference [] s = length s
difference s [] = length s
difference (c:cs) (d:ds) = (if c == d then 0 else 1) + difference cs ds

parser :: Parser [String]
parser = many1 letter `sepEndBy1` newline
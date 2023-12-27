module Challenges.Y2015.Day11 (solutionA, solutionB) where

import Data.Char (ord, chr)

solutionA :: String -> String
solutionA = until isValid increment . increment

solutionB :: String -> String
solutionB = solutionA . solutionA

increment :: String -> String
increment s
    | last s == 'z' = increment (init s) ++ ['a']
    | otherwise = init s ++ [incrementC $ last s]

incrementC :: Char -> Char
incrementC 'z' = 'a'
incrementC 'h' = 'j'
incrementC 'n' = 'p'
incrementC 'k' = 'm'
incrementC c = (chr . (+1) . ord) c

isValid :: String -> Bool
isValid s = hasNoIOL s && hasTwoPair s && hasStraight s

hasPair :: Eq a => [a] -> Bool
hasPair [] = False
hasPair [_] = False
hasPair (a:a':as) = a == a' || hasPair (a':as)

hasTwoPair :: Eq a => [a] -> Bool
hasTwoPair [] = False
hasTwoPair [_] = False
hasTwoPair (a:a':as) = (a == a' && hasPair as) || hasTwoPair (a':as)

hasStraight :: String -> Bool
hasStraight [] = False
hasStraight [_] = False
hasStraight [_,_] = False
hasStraight (c:c':c'':cs) = (precedes c c' && precedes c' c'') || hasStraight (c':c'':cs) where
    precedes :: Char -> Char -> Bool
    precedes x y = ord y - ord x == 1

hasNoIOL :: String -> Bool
hasNoIOL s = 'i' `notElem` s && 'o' `notElem` s && 'l' `notElem` s
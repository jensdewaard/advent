module Challenges.Y2015.Day05 (solutionA, solutionB, input) where

import Data.List (isInfixOf)

solutionA :: String -> String
solutionA = show . length . filter id . map isNiceA . lines
solutionB :: String -> String
solutionB = show . length . filter id . map isNiceB . lines
input :: Bool -> IO String
input False = readFile "data/2015/05.txt"
input True = return "ieodomkazucvgmuy"

hasThreeVowels :: String -> Bool
hasThreeVowels = (>= 3) . length . filter isVowel

hasRepeatedElem :: Eq a => [a] -> Bool
hasRepeatedElem [] = False
hasRepeatedElem [_] = False
hasRepeatedElem [a, a'] = a == a'
hasRepeatedElem (a:a':as) = a == a' || hasRepeatedElem (a':as)

containsForbidden :: String -> Bool
containsForbidden [] = False
containsForbidden [_] = False
containsForbidden ('a':'b':_) = True
containsForbidden ('c':'d':_) = True
containsForbidden ('p':'q':_) = True
containsForbidden ('x':'y':_) = True
containsForbidden (_:cs) = containsForbidden cs

hasDoublePair :: String -> Bool
hasDoublePair [] = False
hasDoublePair [_] = False
hasDoublePair (a:a':as) = isInfixOf [a,a'] as || hasDoublePair (a':as)

hasRepeatedSpaced :: Eq a => [a] -> Bool
hasRepeatedSpaced [] = False
hasRepeatedSpaced [_] = False
hasRepeatedSpaced [_, _] = False
hasRepeatedSpaced (a:b:c:ds) = a == c || hasRepeatedSpaced (b:c:ds)

isNiceA :: String -> Bool
isNiceA s = hasThreeVowels s && hasRepeatedElem s && not (containsForbidden s)

isNiceB :: String -> Bool
isNiceB s = hasDoublePair s && hasRepeatedSpaced s

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'u' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel _ = False
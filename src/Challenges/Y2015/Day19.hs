{-# LANGUAGE TupleSections #-}
module Challenges.Y2015.Day19 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.Prelude
import Data.List (isPrefixOf, nub, sortBy)
import Data.Tuple (swap)
import Common.Search (dfsUntil)

solutionA :: String -> String
solutionA = solve parser (\(s, rs) -> length $ nub $ concatMap (apply s) rs)
solutionB :: String -> String
solutionB = solve parser (\(s, rs) -> solveB' (sortBy (flip compareRule) $ map swap rs) (s, 0))
--solutionB = solve parser (\(s, rs) -> sortBy (flip compareRule) $ map swap rs)


compareRule :: Rule -> Rule -> Ordering
compareRule (s1, _) (s2, _)
    | length s1 > length s2 = GT
    | length s1 < length s2 = LT
    | otherwise = s1 `compare` s2

solveB' :: [Rule] -> (String, Int) -> Int
solveB' rules (input, n) = minimum $ map snd $ filter (\x -> fst x == "e") $ dfsUntil ((=="e") . fst) (applyFirst rules) [(input,n)]

type Rule = (String, String)

applyFirst :: [Rule] -> (String, Int) -> [(String, Int)]
applyFirst rs (s,i) = map (,i+1) $ concatMap (apply s) rs

apply :: String -> Rule -> [String]
apply [] _ = []
apply source@(h:ts) rule@(start, replace)
    | length start > length source = []
    | start `isPrefixOf` source = (replace ++ drop (length start) source) : map (h :) (apply ts rule)
    | otherwise = map (h :) $ apply ts rule

parser :: Parser (String, [Rule])
parser = do
    rs <- rule `sepEndBy1` newline
    _ <- newline
    start <- many letter
    return (start, rs)
    where
        rule :: Parser Rule
        rule = do
            s <- many1 letter
            _ <- string " => "
            r <- many1 letter
            return (s,r)
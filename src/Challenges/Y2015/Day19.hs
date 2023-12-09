module Challenges.Y2015.Day19 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve)
import Data.List (isInfixOf)

solutionA :: String -> String
solutionA = solve parser id
solutionB :: String -> String
solutionB = solve parser (const "")

type Rule = (String, String)

applies :: String -> Rule -> Bool
applies s (r,_) = s `isInfixOf` r

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
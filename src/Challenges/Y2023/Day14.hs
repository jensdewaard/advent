module Challenges.Y2023.Day14 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.Prelude
import Common.List (findCycle, splitOn)
import Data.List (transpose, intercalate, sort)
--import Data.List.Split (splitOn)

solutionA :: String -> String
solutionA = solve parser (weight . tiltNorth)
solutionB :: String -> String
solutionB = solve parser solveB

solveB :: [String] -> Int
solveB world = let (hd, cyc) = findCycle $ iterate doCycle world
    in weight $ iterate doCycle world !! (length hd + ((1000000000 - length hd) `mod` length cyc))

weight :: [String] -> Int
weight ss = sum $ zipWith (*) [1..] (reverse $ map (length . filter (=='O')) ss)

doCycle :: [String] -> [String]
doCycle = tiltEast . tiltSouth . tiltWest . tiltNorth

tilt :: ([String] -> [String]) -> ([Char] -> [Char]) -> [String] -> [String]
tilt mf lf = mf . map ((intercalate "#" . map (lf . sort)) . splitOn "#") . mf

tiltEast :: [String] -> [String]
tiltEast = tilt id id
tiltWest :: [String] -> [String]
tiltWest = tilt id reverse
tiltNorth :: [String] -> [String]
tiltNorth = tilt transpose reverse
tiltSouth :: [String] -> [String]
tiltSouth = tilt transpose id

parser :: Parser [String]
parser = do many1 (char '.' <|> char 'O' <|> char '#') `sepEndBy` newline
module Challenges.Y2022.Day15 (solutionA, solutionB) where

import Common.Prelude
import Common.Coord (Coord, dist)
import Common.Interval (Interval (Empty), fromPair, union, length)
import Text.ParserCombinators.Parsec
import Common.Parsing (int)

solutionA :: String -> String
solutionA = solve parseInput (Common.Interval.length . foldl union Empty . map fst . concatMap (yIs 2000000 . coverage))

solutionB :: String -> String
solutionB = solve parseInput (\sbs -> freq $ head $ findJust $ concatMap (map (consider sbs) . border) sbs)
--solutionB = solve parseInput (filter (\(y,is) -> List.length is > 1) . Map.toList . foldl foldCoverage Map.empty . yLimit 0 4000000 . concatMap coverage)

yIs :: Int -> [(Interval, Int)] -> [(Interval, Int)]
yIs _ [] = []
yIs y (p:ps) = if snd p == y then p : yIs y ps else yIs y ps

findJust :: [Maybe a] -> [a]
findJust [] = []
findJust ((Just a):_) = [a]
findJust (Nothing:as) = findJust as

freq :: Coord -> Int
freq (x,y) = x * 4000000+y

coverage :: (Sensor, Beacon) -> [(Interval, Int)]
coverage (s@(sx, sy), b) = let
    d = dist s b in
    [(fromPair (sx - d + abs (sy - y), sx + d - abs (sy - y)), y) |
        y <- [(sy - d)..(sy + d)]
        ]

consider :: [(Sensor, Beacon)] -> Coord -> Maybe Coord
consider ss c@(x,y)
    | x < 0 = Nothing
    | x > 4000000 = Nothing
    | y < 0 = Nothing
    | y > 4000000 = Nothing
    | otherwise = if any (check c) ss then Nothing else Just c

check :: Coord -> (Sensor, Beacon) -> Bool
check c (s,b) = dist s c <= dist s b 

border :: (Sensor, Beacon) -> [Coord]
border (s@(sx,sy), b) = let
    d = dist s b + 1 in
    [(x,y) | x <- [(sx - d)..(sx + d)],
             y <- [(sy - d)..(sy + d)],
             dist (x,y) s == d]

-- data
type Sensor = Coord
type Beacon = Coord

-- parsing
parseInput :: Parser [(Sensor, Beacon)]
parseInput = sepBy1 (do
        s <- sensor
        _ <- string ": "
        b <- beacon
        return (s, b)
    ) newline

sensor :: Parser Sensor
sensor = do
    _ <- string "Sensor at x="
    x <- int
    _ <- string ", y="
    y <- int
    return (x, y)

beacon :: Parser Beacon
beacon = do
    _ <- string "closest beacon is at x="
    x <- int
    _ <- string ", y="
    y <- int
    return (x,y)
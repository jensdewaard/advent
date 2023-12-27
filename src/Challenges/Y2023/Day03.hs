module Challenges.Y2023.Day03 where
import Text.ParserCombinators.Parsec

import Common.Prelude
import Common.Coord (Coord)
import Data.Char (isDigit)

type Tag = (Coord, String)

solutionA :: String -> String
solutionA = solve parseInput (sum . map (readInt . snd) . filterPartNumbers)
--solutionA = solve parseInput show
solutionB :: String -> String
solutionB = solve parseInput (sum . map mult . filterGears)

readInt :: String -> Int
readInt = read

isPart :: Tag -> Bool
isPart  = not . isNumber

mult :: [Tag] -> Int
mult ts = let
    x = readInt $ snd $ head ts
    y = readInt $ snd $ last ts
    in x * y

isGear :: Tag -> Bool
isGear (_,s) = s == "*"

isNumber :: Tag -> Bool
isNumber (_, s)= isDigit $ head s

filterGears :: [Tag] -> [[Tag]]
filterGears ts = filter (\ns -> length ns == 2) $ -- filter to 2 numbers
    map (\g -> filter (\p -> isNumber p && isNearby p g) ts)  -- maps gears to all nearby numbers
    $ filter isGear ts -- filter gears from tag list


filterPartNumbers :: [Tag] -> [Tag]
filterPartNumbers ts = filter isNumber $ filter (hasNearbyPartIn ps) ts where 
    ps = filter isPart ts

hasNearbyPartIn :: [Tag] -> Tag -> Bool
hasNearbyPartIn ps p = any (isNearby p) ps

isNearby :: Tag -> Tag -> Bool
isNearby ((y1, x1), n) ((y2, x2), _) = abs (y2 - y1) <= 1 &&
    x2 >= (x1 - 1) && x2 < (x1 + length n + 1)

parseInput :: Parser [Tag]
parseInput = (optional $ many1 $ char '.') >> endBy1 parseTag (optional $ many1 $ (char '.' <|> newline))

parseTag :: Parser Tag
parseTag = parsePartNumber <|> parsePart

parsePartNumber :: Parser Tag
parsePartNumber = do
    n <- many1 digit
    l <- getPosition
    return ((sourceLine l - 1, sourceColumn l - 1 - length n), n)

parsePart :: Parser Tag
parsePart = do
    c <- (char '=' <|> char '#' <|> char '*' <|> char '+' <|> char '$' <|> char '/' <|> char '%' <|> char '&' <|> char '-' <|> char '@')
    l <- getPosition
    return ((sourceLine l - 1, sourceColumn l - 2),[c])
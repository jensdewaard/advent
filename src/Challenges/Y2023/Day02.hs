module Challenges.Y2023.Day02 where
import Text.ParserCombinators.Parsec

import Shared (solve)
import qualified Data.Map as Map
import Data.Map (Map)

solutionA :: String -> String
solutionA = solve parseInput (sum . map gameId . filter isValidGame)

solutionB :: String -> String
solutionB = solve parseInput (sum . map (power . minGame))

data Color = Red | Green | Blue deriving (Show, Eq)

instance Ord Color where
    Red `compare` Green = LT
    Green `compare` Blue = LT
    Red `compare` Blue = LT
    Blue `compare` Red = GT
    Blue `compare` Green = GT
    Green `compare` Red = GT
    _ `compare` _ = EQ

type CubeSet = Map Color Int
data Game = Game Int [CubeSet] deriving (Show)

gameId :: Game -> Int
gameId (Game i _) = i

maxBlue :: Int
maxBlue = 14
maxRed :: Int
maxRed = 12
maxGreen :: Int
maxGreen = 13

minGame :: Game -> CubeSet
minGame (Game _ cs) = foldl minSet Map.empty cs

minSet :: CubeSet -> CubeSet -> CubeSet
minSet s1 s2 = let
    r1 = Map.findWithDefault 0 Red s1
    g1 = Map.findWithDefault 0 Green s1
    b1 = Map.findWithDefault 0 Blue s1
    r2 = Map.findWithDefault 0 Red s2
    g2 = Map.findWithDefault 0 Green s2
    b2 = Map.findWithDefault 0 Blue s2
    in Map.fromList [(Red, max r1 r2), (Green, max g1 g2), (Blue, max b1 b2)]

power :: CubeSet -> Int
power s = let
    r = Map.findWithDefault 0 Red s
    g = Map.findWithDefault 0 Green s
    b = Map.findWithDefault 0 Blue s
    in r*g*b

isValidGame :: Game -> Bool
isValidGame (Game _ cs) = all isValidSet cs

isValidSet :: CubeSet -> Bool
isValidSet s = let 
    r = Map.findWithDefault 0 Red s
    g = Map.findWithDefault 0 Green s
    b = Map.findWithDefault 0 Blue s
    in r <= maxRed && g <= maxGreen && b <= maxBlue

parseInput :: Parser [Game]
parseInput = sepBy1 parseGame newline

parseGame :: Parser Game
parseGame = do
    _ <- string "Game "
    i <- many1 digit
    _ <- string ": "
    cs <- sepBy1 parseSet (char ';')
    return (Game (read i) cs)

parseSet :: Parser CubeSet
parseSet = do 
    kv <- sepBy1 parseGrab (string ", ")
    return $ Map.fromList kv


parseGrab :: Parser (Color, Int)
parseGrab = do
    _ <- try spaces
    n <- many1 digit
    _ <- spaces
    c <- parseColor
    return (c, read n)

parseColor :: Parser Color
parseColor = do
    (try $ string "blue" >> return Blue)
    <|> (try $ string "red" >> return Red)
    <|> (try $ string "green" >> return Green)


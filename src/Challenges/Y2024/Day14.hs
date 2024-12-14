{-# LANGUAGE TupleSections #-}
module Challenges.Y2024.Day14 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, sepEndBy, newline, char, string)
import Control.Arrow ((>>>))
import Common.Vector2 (Vector2 (..), vector2)
import Common.Parsing (int)
import Data.List (nub)

solutionA :: String -> String
solutionA = solve parser (map (moveRobot 100) >>> quadrants >>> safetyFactor)
solutionB :: String -> String
solutionB = solve parser (map (moveRobot 2812) >>> (2812,) >>> iterate iter
    >>> dropWhile (\(n,rs) -> (maximum (robotsPerY rs) < 32) && n < 8500)
    >>> head >>> fst
    )

width :: Int
width = 101
-- width = 11
height :: Int
height = 103
-- height = 7

robotsAtY :: Int -> [Robot] -> Int
robotsAtY k rs = length $ nub $ map position $ filter ((==k). y . position) rs

robotsPerY :: [Robot] -> [Int]
robotsPerY rs = map (`robotsAtY` rs) [0..height - 1]

iter :: (Int, [Robot]) -> (Int, [Robot])
iter (n, rs) = (n+1, map (moveRobot 1) rs)

parser :: Parser [Robot]
parser = robot `sepEndBy` newline where
    robot = do
        _ <- string "p="
        px <- int
        _ <- char ','
        py <- int
        _ <- string " v="
        vx <- int
        _ <- char ','
        Robot (vector2 px py) . vector2 vx <$> int

moveRobot :: Int -> Robot -> Robot
moveRobot steps (Robot pos vel) = let
    v' = ((*steps) <$> vel)
    p' = (+) <$> pos <*> v'
    pos' = liftA2 mod p' (vector2 width height)
    in Robot pos' vel

quadrants :: [Robot] -> (Int, Int, Int, Int)
quadrants rs = let
    ps = map position rs
    halfX = width `div` 2
    halfY = height `div` 2
    q1 = length $ filter (\v -> x v < halfX && y v < halfY) ps
    q2 = length $ filter (\v -> x v < halfX && y v > halfY) ps
    q3 = length $ filter (\v -> x v > halfX && y v < halfY) ps
    q4 = length $ filter (\v -> x v > halfX && y v > halfY) ps
    in (q1,q2,q3,q4)

safetyFactor :: (Int, Int, Int, Int) -> Int
safetyFactor (a,b,c,d) = a*b*c*d

data Robot = Robot
    {   position :: Vector2 Int
    ,   velocity :: Vector2 Int
    } deriving (Eq, Show)
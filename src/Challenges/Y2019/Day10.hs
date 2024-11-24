module Challenges.Y2019.Day10 (solutionA, solutionB) where
import Common.Prelude
import Common.Vector2 (Vector2(..), vector2, angleI, manhattan)
import Text.ParserCombinators.Parsec
import Data.Maybe (isJust, fromJust)
import Data.List (nub, maximumBy, sortOn)
import Data.List.Extra (groupOn)
import Data.Function (on)
import Control.Arrow ((>>>), (&&&))

solutionA :: String -> String
solutionA = solve parser (\as -> length $ visibleFrom as $ maximumBy (compare `on` (length . visibleFrom as)) as)
solutionB :: String -> String
solutionB = solve parser (\as -> let
    winner = bestAsteroid as
    in target winner as)

bestAsteroid :: [Vector2 Int] -> Vector2 Int
bestAsteroid as = maximumBy (compare `on` (length . visibleFrom as)) as

visibleFrom :: [Vector2 Int] -> Vector2 Int -> [Vector2 Int]
visibleFrom asteroids from = nub $ map (normalize' from) asteroids

target :: Vector2 Int -> [Vector2 Int] -> Int
target base = sortOn (manhattan base)
           >>> drop 1
           >>> map (angleTo base &&& id)
           >>> sortOn fst
           >>> groupOn fst
           >>> map (map snd)
           >>> runLaser
           >>> drop (200-1)
           >>> head
           >>> \v -> 100 * x v + y v

angleTo :: Vector2 Int -> Vector2 Int -> Float
angleTo base v@(Vector2 c r)
    | c == col && r < row = negate pi
    | otherwise           = angleI $ cast $ normalize' base v
    where Vector2 col row = base

normalize' :: Vector2 Int -> Vector2 Int -> Vector2 Int
normalize' v w
    | v == w = Vector2 0 0
normalize' (Vector2 col row) (Vector2 col' row') = vector
  where
    divisor  = gcd dc dr
    vector   = Vector2 (dc `div` divisor) (dr `div` divisor)
    (dc, dr) = (col'-col,
                row'-row)

cast :: Vector2 Int -> Vector2 Int
cast (Vector2 c r) = Vector2 r (-c)

runLaser :: [[Vector2 a]] -> [Vector2 a]
runLaser [] = []
runLaser ((a : as) : bs) = a : runLaser (bs ++ ([as | not (null as)]))
runLaser ([] : as) = runLaser as

parser :: Parser [Vector2 Int]
parser = (map fromJust <$> filter isJust) . concat <$> (many1 asteroid `sepEndBy` newline) where
    asteroid :: Parser (Maybe (Vector2 Int))
    asteroid = (char '.' >> return Nothing)
        <|> (char '#' >> do
        pos <- getPosition
        return $ Just $ vector2 (sourceColumn pos - 2) (sourceLine pos - 1)
        )


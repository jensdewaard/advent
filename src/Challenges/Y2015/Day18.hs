module Challenges.Y2015.Day18 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve)
import Common.Coord (Coord)
import Data.Map (Map)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as Map
import Data.Bifunctor (second)

solutionA :: String -> String
solutionA = solve parser (\m -> length $ Map.filter (>=1) $ iterate stepA m !! 100)
solutionB :: String -> String
solutionB = solve parser (\m -> length $ Map.filter (>=1) $ iterate stepB m !! 100)

stepA :: Map Coord Int -> Map Coord Int
stepA m = Map.mapWithKey step' m
    where
        step' :: Coord -> Int -> Int
        step' c 0 = if on (neighbours m c) == 3 then 1 else 0
        step' c _ = case on (neighbours m c) of
            2 -> 1
            3 -> 1
            _ -> 0

stepB :: Map Coord Int -> Map Coord Int
stepB m = Map.mapWithKey step' m
    where
        step' :: Coord -> Int -> Int
        step' (0,0) _ = 1 
        step' (0,99) _ = 1
        step' (99,0) _ = 1
        step' (99,99) _ = 1
        step' c 0 = if on (neighbours m c) == 3 then 1 else 0
        step' c _ = case on (neighbours m c) of
            2 -> 1
            3 -> 1
            _ -> 0

on :: [(Coord, Int)] -> Int
on cs = length $ filter (\(_,i) -> i == 1) cs

neighbours :: Map Coord Int -> Coord -> [(Coord, Int)]
neighbours m (x,y) = let cs = [(x-1,y-1), (x,y-1), (x+1, y-1), (x-1,y), (x+1,y), (x-1,y+1), (x, y+1), (x+1,y+1)]
    in map (second fromJust) $ filter (isJust . snd) $ map (\c -> (c, Map.lookup c m)) cs

parser :: Parser (Map Coord Int)
parser = do
    ls <- concat <$> sepEndBy1 (many1 light) newline
    return $ Map.fromList ls
    where
        light :: Parser (Coord, Int)
        light = do
            pos <- getPosition
            i <- (char '#' >> return 1) <|> (char '.' >> return 0)
            let c = (sourceColumn pos - 1, sourceLine pos - 1)
            return (c,i)
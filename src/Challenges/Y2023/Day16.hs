module Challenges.Y2023.Day16 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Shared (solve)
import Common.Coord (Coord, Dir (..), move)
import Common.Search (bfs)

solutionA :: String -> String
solutionA = solve parser (path ((1,1), R))
solutionB :: String -> String
-- 8294 is too low
solutionB = solve parser (\w -> maximum $ map (`path` w) starts)

path :: (Coord, Dir) -> Map Coord Char -> Int
path cd w = length $ nub $ map fst $ bfs (walk w) [cd]

starts :: [(Coord, Dir)]
starts = [((1,n), R) | n <- [1..110]] ++
    [((110,n), L) | n <- [1..110]] ++
    [((n,1), D) | n <- [1..110]] ++
    [((n,110), U) | n <- [1..110]]

walk :: Map Coord Char -> (Coord, Dir) -> [(Coord, Dir)]
walk world (h,D) = case Map.lookup h world of
    Nothing -> []
    Just '.' -> [(move D h, D) | isJust (Map.lookup (move D h) world)]
    Just '|' -> [(move D h, D) | isJust (Map.lookup (move D h) world)]
    Just '/' -> [(move L h, L) | isJust (Map.lookup (move L h) world)]
    Just '\\' -> [(move R h, R)| isJust (Map.lookup (move R h) world)]
    Just '-' -> [(move L h, L) | isJust (Map.lookup (move L h) world)] ++
                [(move R h, R) | isJust (Map.lookup (move R h) world)]
    _ -> error "unknown char"
walk world (h,U) = case Map.lookup h world of
    Nothing -> []
    Just '.' -> [(move U h, U) | isJust (Map.lookup (move U h) world)]
    Just '/' -> [(move R h, R) | isJust (Map.lookup (move R h) world)]
    Just '\\' -> [(move L h, L)| isJust (Map.lookup (move L h) world)]
    Just '-' -> [(move L h, L) | isJust (Map.lookup (move L h) world)]
                ++ [(move R h, R) | isJust (Map.lookup (move R h) world)]
    Just '|' -> [(move U h, U) | isJust (Map.lookup (move U h) world)]
    _ -> error "unknown char"
walk world (h, L) = case Map.lookup h world of
    Nothing -> []
    Just '.' -> [(move L h, L) | isJust (Map.lookup (move L h) world)]
    Just '/' -> [(move D h, D) | isJust (Map.lookup (move D h) world)]
    Just '\\' -> [(move U h, U) | isJust (Map.lookup (move U h) world)]
    Just '-' -> [(move L h, L) | isJust (Map.lookup (move L h) world)]
    Just '|' -> [(move U h, U) | isJust (Map.lookup (move U h) world)]
                ++ [(move D h, D) | isJust (Map.lookup (move D h) world)]
    _ -> error "unknown char"
walk world (h, R) = case Map.lookup h world of
    Nothing -> []
    Just '.' -> [(move R h, R) | isJust (Map.lookup (move R h) world)]
    Just '/' -> [(move U h, U) | isJust (Map.lookup (move U h) world)]
    Just '\\' -> [(move D h, D) | isJust (Map.lookup (move D h) world)]
    Just '-' -> [(move R h, R) | isJust (Map.lookup (move R h) world)]
    Just '|' -> [(move U h, U) | isJust (Map.lookup (move U h) world)]
            ++ [(move D h, D) | isJust (Map.lookup (move D h) world)]
    _ -> error "unknown char"


parser :: Parser (Map Coord Char)
parser = do
    ps <- concat <$> many1 position `sepEndBy1` newline
    return $ Map.fromList ps

position :: Parser (Coord, Char)
position = do
    pos <- getPosition
    let p = (sourceColumn pos, sourceLine pos)
    c <- oneOf "./\\-|"
    return (p,c)

{-# Language TupleSections #-}
module Challenges.Y2015.Day13 (solutionA, solutionB) where

import Data.Graph.Inductive.Basic (undir)
import Data.Graph.Inductive.Graph (DynGraph, Path, nodes, insNode, insEdges)
import Data.Graph.Inductive.NodeMap (mkNode_, fromGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (delete, find, nub)
import Shared (solve, fst3, trd, mkLGraph, pathLength, hamiltonian)
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = solve parser (show . tspWith' maximum . undir)
solutionB :: String -> String
solutionB = solve parser (show . tspWith' maximum . undir. extendWithMe)

tspWith' :: (DynGraph gr, Real b) => ([b] -> b) -> gr a b -> b
tspWith' f g = f $ map (pathLength g) $ hamiltonianR g

hamiltonianR :: DynGraph gr => gr a b -> [Path]
hamiltonianR = map (\l -> l ++ [head l]) . hamiltonian

extendWithMe :: DynGraph gr => gr String Int -> gr String Int
extendWithMe g = insEdges es $ insNode n g where
    es = map (fst n, ,0) $ nodes g
    n = mkNode_ (fromGraph g) "me"

parser :: Parser (Gr String Int)
parser = do
    ls <- sepBy1 parseLine newline
    let names = nub $ map fst3 ls
    let happydiffs = merge ls
    return $ mkLGraph names happydiffs where
        parseSign = try (string " would lose " >> return (-1)) <|> try (string " would gain " >> return 1)
        parseLine = do
            n1 <- many1 letter
            s <- parseSign
            x <- many1 digit
            _ <- string " happiness units by sitting next to "
            n2 <- many1 letter
            _ <- string "."
            return (n1, n2, read x * s)

merge :: [(String, String, Int)] -> [(String, String, Int)]
merge [] = []
merge [a] = [a]
merge ((a,b, n):es) = (a,b,n'):merge (deleteM e es) where
    e = find (\(a',b',_) -> a == b' && b == a') es
    n' = n + maybe 0 trd e
    deleteM Nothing ls = ls
    deleteM (Just x) ls = delete x ls



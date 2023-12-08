{-# LANGUAGE TupleSections #-}
module Challenges.Y2023.Day08 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve, fst3, endsWith)
import Data.List (find, sort)

solutionA :: String -> String
solutionA = solve parseInput solveA
solutionB :: String -> String
solutionB = solve parseInput solveB

solveA :: ([Dir], [Node]) -> Int
solveA (ds, ns) = solvePuzzle (ds,ns) atEndA ("AAA",0)

solveB :: ([Dir], [Node]) -> Int
solveB (ds, ns) = foldl lcm 1 $ map (solvePuzzle (ds,ns) atEndB) (findStarts ns)

solvePuzzle :: ([Dir], [Node]) -> ((String, Int) -> Bool) -> (String, Int) -> Int
solvePuzzle (ds, ns) atEnd start = snd $ until atEnd (walk ds ns) start

walk :: [Dir] -> [Node] -> (String, Int) -> (String, Int)
walk ds ns (n, i) = let
    cd = (cycle ds !! i)
    cn = findNode n ns
    nn = selectNext cd cn
    in (nn, i+1)

atEndA :: (String, Int) -> Bool
atEndA ("ZZZ", _) = True
atEndA _ = False

atEndB :: (String, Int) -> Bool
atEndB = endsWith 'Z' . fst

findNode :: String -> [Node] -> Node
findNode s ns = case find (\n -> fst3 n == s) ns of
    Just x -> x
    Nothing -> error ("cannot find node " ++ s)


findStarts :: [Node] -> [(String, Int)]
findStarts = map (,0) . filter (endsWith 'A') . map fst3

selectNext :: Dir -> Node -> String
selectNext L (_, l, _) = l
selectNext R (_, _, r) = r

data Dir = L | R deriving (Eq, Show)
type Node = (String, String, String)

parseInput :: Parser ([Dir], [Node])
parseInput = do
    ds <- many1 dir
    _ <- newline >> newline
    ns <- node `sepEndBy1` newline
    return (ds, sort ns)

node :: Parser Node
node = do
    lbl <- many1 alphaNum
    _ <- string " = ("
    left <- many1 alphaNum
    _ <- string ", "
    right <- many1 alphaNum
    _ <- string ")"
    return (lbl, left, right)

dir :: Parser Dir
dir = (char 'L' >> return L) <|> (char 'R' >> return R)
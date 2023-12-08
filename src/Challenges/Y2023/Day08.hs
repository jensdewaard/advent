module Challenges.Y2023.Day08 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve, fst3)
import Data.List (find, sort)
import Data.Maybe (fromJust)

solutionA :: String -> String
solutionA = solve parseInput solveA
solutionB :: String -> String
solutionB = undefined

solveA :: ([Dir], [Node]) -> Int
solveA (ds, ns) = snd $ until atEnd (walk ds ns) ("AAA", 0)

walk :: [Dir] -> [Node] -> (String, Int) -> (String, Int)
walk ds ns (n, i) = let
    cd = (cycle ds !! i)
    cn = findNode n ns
    nn = selectNext cd cn
    in (nn, i+1)

atEnd :: (String, Int) -> Bool
atEnd ("ZZZ", _) = True
atEnd _ = False

findNode :: String -> [Node] -> Node
findNode s = fromJust . find (\n -> fst3 n == s)

selectNext :: Dir -> Node -> String
selectNext L (_, l, _) = l
selectNext R (_, _, r) = r

data Dir = L | R deriving (Eq, Show)
type Node = (String, String, String)

parseInput :: Parser ([Dir], [Node])
parseInput = do
    ds <- many1 dir
    _ <- newline >> newline
    ns <- node `sepBy1` newline
    return (ds, sort ns)

node :: Parser Node
node = do
    lbl <- many1 letter
    _ <- string " = ("
    left <- many1 letter
    _ <- string ", "
    right <- many1 letter
    _ <- string ")"
    return (lbl, left, right)

dir :: Parser Dir
dir = (char 'L' >> return L) <|> (char 'R' >> return R)
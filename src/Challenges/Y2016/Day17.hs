module Challenges.Y2016.Day17 (solutionA, solutionB) where
import Common.Prelude
import Common.Coord
import Common.Search (bfsUntil, dfs)
import Data.List (singleton)
import Data.String (fromString)
import Text.ParserCombinators.Parsec hiding (State)
import Data.Digest.Pure.MD5 (md5)

solutionA :: String -> String
solutionA = solve parser (\k -> showDirs $ snd $ head $ filter predicate $ bfsUntil predicate (adj k) start)
solutionB :: String -> String
solutionB = solve parser (\k -> maximum $ map (length . snd) (filter predicate $ dfs (adj k) start))

start :: [State]
start = [((1,1), [])]

predicate :: State -> Bool
predicate ((4,4), _) = True
predicate _ = False

type State = (Coord, [Dir])
adj :: String -> State -> [State]
adj _ ((4,4), _) = []
adj _ ((0,_), _) = []
adj _ ((_,0), _) = []
adj _ ((5,_), _) = []
adj _ ((_,5), _) = []
adj key (c,ds)   = let h = show $ md5 $ fromString (key ++ showDirs ds)  in
    (if isOpen $ head h then singleton (above c, ds ++ [U]) else []) ++
    (if isOpen $ h !! 1 then singleton (below c, ds ++ [D]) else []) ++
    (if isOpen $ h !! 2 then singleton (left c, ds ++ [L]) else []) ++
    (if isOpen $ h !! 3 then singleton (right c, ds ++ [R]) else [])

showDirs :: [Dir] -> String
showDirs = concatMap show

isOpen :: Char -> Bool
isOpen 'b' = True
isOpen 'c' = True
isOpen 'd' = True
isOpen 'e' = True
isOpen 'f' = True
isOpen _ = False

parser :: Parser String
parser = return "edjrjqaa"

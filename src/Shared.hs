{-# LANGUAGE FlexibleInstances #-}

module Shared (solve, 
chunksOf, endsWith, 

fromRight, mkLGraph, indexNodes, 
fstEq, simpl, until1, 
hasPair, hasTwoPair, 
hamiltonian, tsp, tspWith, 
validPath, pathLength,

trd, fst3, snd3, mapl, mapr,
allEqual, longest, mapIf, prepend) where

import Data.Graph.Inductive.Graph
    ( Graph(mkGraph), LEdge, edgeLabel, hasEdge, nodes, out, Path )
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (find, permutations)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Text.ParserCombinators.Parsec

solve :: Show b => Parser a -> (a -> b) -> String -> String
solve parser f = show . f . parse' parser

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n rs = take n rs : chunksOf n (drop n rs)

parse' :: Parser a -> String -> a
parse' p i = case parse p "advent" i of
                  Left err -> error ("could not run parser " ++ show err)
                  Right val -> val

-- splitOn :: String -> String -> [String]
-- splitOn d s = map T.unpack $ T.splitOn (T.pack d) (T.pack s)

endsWith :: Eq a => a -> [a] -> Bool
endsWith a as = last as == a

-- rectFromTo :: Coord -> Coord -> [Coord]
-- rectFromTo (x1,y1) (x2, y2) = concatMap (\x -> map (x,) (enumFromTo y1 y2)) (enumFromTo x1 x2)

fromRight :: Either a b -> b
fromRight (Left _) = error "fromRight from Left"
fromRight (Right r) = r

mkLGraph :: (Eq a) => [a] -> [(a,a,b)] -> Gr a b
mkLGraph ns es = let
    nds = map swap (indexNodes ns)
    mkEdge :: Eq a => [a] -> (a,a,b) -> LEdge b
    mkEdge nds' (n1, n2, e) = (simpl $ find (fstEq n1) (indexNodes nds'), simpl $ find (fstEq n2) (indexNodes nds'), e)
    edgs = map (mkEdge ns) es
    in mkGraph nds edgs

indexNodes :: [a] -> [(a,Int)]
indexNodes ns = zip ns [1..]

fstEq :: Eq a => a -> (a,b) -> Bool
fstEq y x = fst x == y

simpl :: Maybe (a, Int) -> Int
simpl = snd . fromJust

until1 :: (a -> Bool) -> (a -> a) -> a -> a
until1 p f x = until p f (f x)

hasPair :: Eq a => [a] -> Bool
hasPair [] = False
hasPair [_] = False
hasPair (a:a':as) = a == a' || hasPair (a':as)

hasTwoPair :: Eq a => [a] -> Bool
hasTwoPair [] = False
hasTwoPair [_] = False
hasTwoPair (a:a':as) = (a == a' && hasPair as) || hasTwoPair (a':as)

hamiltonian :: Graph gr => gr a b -> [Path]
-- ^ The 'hamiltonian' function returns all hamiltonian paths in the graph.
hamiltonian g = filter (validPath g) $ permutations $ nodes g

tsp :: (Graph gr, Real b) => gr a b -> b
tsp = tspWith minimum

tspWith :: (Graph gr, Real b) => ([b] -> b) -> gr a b -> b
tspWith f g = f $ map (pathLength g) $ hamiltonian g

{- | The 'validPath' function returns true if there are edges in the given 
    graph between all nodes in the given path.
    -}
validPath :: Graph gr => gr a b -> Path -> Bool
validPath _ [] = True
validPath _ [_] = True
validPath g (a:b:ns) = hasEdge g (a,b) && validPath g (b:ns)

pathLength :: (Graph gr, Real b) => gr a b -> Path -> b
pathLength _ [] = 0
pathLength _ [_] = 0
pathLength g (a:b:ns) = l + pathLength g (b:ns) where
    l =  edgeLabel $ head $ filter (\(_ ,b', _) -> b' == b) $ out g a

trd :: (a, b, c) -> c
trd (_,_,c) = c

fst3 :: (a, b, c) -> a
fst3 (a,_,_) = a

snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b

mapl :: (a -> c) -> [(a,b)] -> [(c,b)]
mapl _ [] = []
mapl f ((a,b):acs) = (f a, b) : mapl f acs

mapr :: (b -> c) -> [(a,b)] -> [(a,c)]
mapr _ [] = []
mapr f ((a,b):acs) = (a, f b) : mapr f acs

allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) (tail xs)

-- | Return the longest list. Returns the first if there are multiple lists of the same length.
longest :: [[a]] -> [a]
longest [] = error "longest on empty list"
longest ls = let l = maximum $ map length ls in head $ filter (\l' -> length l' == l) ls 

-- | Applies a function f to a member a of a list, if predicate p holds, otherwise keeps 
--   the original value of a.
mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf _ _ [] = []
mapIf p f (x:xs) = (if p x then f x else x) : mapIf p f xs 

prepend :: ([a],[b]) -> ([a],[b]) -> ([a],[b])
prepend (a,b) (as,bs) = (a ++ as, b ++ bs)
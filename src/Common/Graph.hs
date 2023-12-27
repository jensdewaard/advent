module Common.Graph (mkLGraph, hamiltonian, tsp, tspWith, validPath, pathLength) where

import Data.Graph.Inductive (Path, Graph (mkGraph), nodes)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (permutations, find)
import Data.Graph.Inductive.Graph (edgeLabel, hasEdge, out, LEdge)
import Data.Maybe (fromJust)

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

mkLGraph :: (Eq a) => [a] -> [(a,a,b)] -> Gr a b
mkLGraph ns es = let
    nds = zip [1..] ns
    mkEdge :: Eq a => [a] -> (a,a,b) -> LEdge b
    mkEdge nds' (n1, n2, e) = (simpl $ find (fstEq n1) (zip nds' [1..]), simpl $ find (fstEq n2) (zip nds' [1..]), e)
    edgs = map (mkEdge ns) es
    in mkGraph nds edgs

fstEq :: Eq a => a -> (a,b) -> Bool
fstEq y x = fst x == y

simpl :: Maybe (a, Int) -> Int
simpl = snd . fromJust
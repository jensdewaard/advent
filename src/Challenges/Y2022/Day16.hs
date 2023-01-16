module Challenges.Y2022.Day16 (parse, solveA, solveB, bestOf, world, toLists, toGraph, rmNode, toMatrix) where
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Graph.Inductive ((&), Gr, Node, Edge, LNode, LEdge, Graph (mkGraph), inn, out, labNodes, delNode, insEdges, hasEdge, nodes, labEdges, toEdge, edgeLabel, labfilter, context)
import FloydWarshall (Weight (Weight, Inf), fromWeight, floydwarshall)
import Data.Foldable (find, maximumBy)

world :: [Valve]
world = [
    Valve{label = "AA", flow = 0, next = ["DD", "II", "BB"]},
    Valve{label = "BB", flow = 13, next = ["CC", "AA"]},
    Valve{label = "CC", flow = 2, next = ["DD", "BB"]},
    Valve{label = "DD", flow = 20, next = ["CC", "AA", "EE"]},
    Valve{label = "EE", flow = 3, next = ["FF", "DD"]},
    Valve{label = "FF", flow = 0, next = ["EE", "GG"]},
    Valve{label = "GG", flow = 0, next = ["FF", "HH"]},
    Valve{label = "HH", flow = 22, next = ["GG"]},
    Valve{label = "II", flow = 0, next = ["AA", "JJ"]},
    Valve{label = "JJ", flow = 21, next = ["II"]}
    ]

toMatrix :: Gr Int Int -> [[Weight]]
toMatrix g = map buildRow ns
    where 
        ns = nodes g
        buildRow :: Node -> [Weight]
        buildRow n = map (getEdgeWeight n) ns
        getEdgeWeight :: Node -> Node -> Weight
        getEdgeWeight n m = if (n == m) then (Weight 0) else
            if hasEdge g (n, m) then Weight (findEdgeLabel (labEdges g) (n,m)) else Inf
        findEdgeLabel :: [LEdge Int] -> Edge -> Int
        findEdgeLabel [] _ = error "no such edge"
        findEdgeLabel (e:es) e' = if toEdge e == e' then edgeLabel e else findEdgeLabel es e'

labelToInt :: String -> Int
labelToInt l = 1000 * ord (l !! 0) + ord (l !! 1)

toLists :: [Valve] -> ([LNode Int], [LEdge Int])
toLists [] = ([], [])
toLists (v : vs) = ((labelToInt $ label v, flow v) : (fst $ toLists vs), 
                    (map (\n -> (labelToInt $ label v, labelToInt n, 1)) (next v)) ++ (snd $ toLists vs))

rmNode :: Gr Int Int -> Node -> Gr Int Int
rmNode g n = insEdges newEdges $ delNode n g where
    newEdges = [(i, o, w1+w2) | (i,_,w1) <- inn g n, (_,o,w2) <- out g n, 
        not $ hasEdge g (i,o),
        i /= o]

rmAllZero :: Gr Int Int -> Gr Int Int
rmAllZero g = foldl rmNode g (nodes $ labfilter (\l -> l == 0) g) where

toGraph :: [Valve] -> Gr Int Int
toGraph vs = mkGraph ns es where
    (ns, es) = toLists vs

parse :: String -> [Valve]
parse _ = []

solveA :: [Valve] -> Int
solveA _ = let
    time = 30
    g = toGraph world
    startNode = fromJust $ find (\(n, _) -> n == 65065) (labNodes g)
    g' = (context g (fst startNode)) & (rmAllZero g)
    dm = toMatrix g'
    dists = floydwarshall dm
    ns = labNodes g'
    ttr :: LNode Int -> LNode Int -> Int
    ttr cur n = fromWeight $ dists !! (fromJust $ indexOf ns cur) !! (fromJust $ indexOf ns n)
    go :: [LNode Int] -> Int -> [(Int, Int)] -> LNode Int -> [(Int, Int)]
    go _ 0 open _ = open
    go _ 1 open cur = (1, snd cur) : open
    go unopened t open cur = open' ++ bestOption where
        options :: [LNode Int]
        options = filter (\n -> snd n <= t - 1) unopened
        options' :: [[(Int, Int)]]
        options' = map (\n -> go (del unopened cur) (t - (ttr cur n)) open cur) options
        bestOption :: [(Int, Int)]
        bestOption = maximumBy maximumPressure options'
        open' = (t - 1, snd cur) : open
    in
        pressure $ go (del ns startNode) time [] startNode

del :: Eq a => [a] -> a -> [a]
del [] _ = []
del (a:as) a' = if a == a' then as else a : del as a'

maximumPressure :: [(Int, Int)] -> [(Int, Int)] -> Ordering
maximumPressure a b = compare a' b' where
    a' :: Int
    a' = pressure a
    b' :: Int
    b' = pressure b

pressure :: [(Int, Int)] -> Int
pressure = sum . map product

solveB :: [Valve] -> Int
solveB _ = 0


bestOf :: [[Valve]] -> [Valve]
bestOf [] = []
bestOf [a] = a
bestOf (a : b : vs) = if a' > b' then bestOf (a : vs) else bestOf (b : vs)
    where 
        a' = released a
        b' = released b

released :: [Valve] -> Int
released = product . map flow

data Valve = Valve {
    label :: String,
    flow :: Int,
    next :: [String]
} deriving (Show, Eq)

indexOf :: Eq a => [a] -> a -> Maybe Int
indexOf as a = indexOf' 0 as a where
    indexOf' :: Eq a => Int -> [a] -> a -> Maybe Int
    indexOf' _ [] _ = Nothing
    indexOf' i (a':as') a'' = if a' == a'' then Just i else indexOf' (i+1) as' a''
module Challenges.Y2022.Day16 (parse, solveA, solveB, bestOf, world, toLists, toGraph, rmNode) where
import Data.Char (ord)
import Data.Maybe (fromJust)
import qualified Data.MemoCombinators as Memo
import Data.List (subsequences)
import Data.Graph.Inductive (Gr, Node, LNode, LEdge, Graph (mkGraph), inn, out, delNode, insEdges, hasEdge)

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

labelToInt :: Label -> Int
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

toGraph :: [Valve] -> Gr Int Int
toGraph vs = mkGraph nodes edges where
    (nodes, edges) = toLists vs

parse :: String -> [Valve]
parse _ = []

solveA :: [Valve] -> Int
solveA _ = maximum $ map released $ subsequences $ filter (\v -> flow v /= 0) world

solveB :: [Valve] -> Int
solveB _ = 0

valveMemo :: Memo.Memo Valve
valveMemo = Memo.wrap (fromJust . getValve world) label (Memo.list Memo.char)

goMemo :: Int -> [Valve] -> Valve -> [Valve]
goMemo = Memo.memo3 Memo.integral (Memo.list valveMemo) valveMemo (go world)

go :: [Valve] -> Int -> [Valve] -> Valve -> [Valve]
go _ 0 open _ = open
go _ 1 open cur = cur : open
go w n open cur
    | cur `elem` open || flow cur == 0 = bestOf $ map (go w (n-1) open) (map getV $ next cur)
    | otherwise = bestOf $ (map (go w (n-2) (cur : open)) (map getV $ next cur)) ++ (map (go w (n-1) open) (map getV $ next cur))
    where 
        getV = fromJust . getValve w

type Label = String

bestOf :: [[Valve]] -> [Valve]
bestOf [] = []
bestOf [a] = a
bestOf (a : b : vs) = if a' > b' then bestOf (a : vs) else bestOf (b : vs)
    where 
        a' = released a
        b' = released b

released :: [Valve] -> Int
released = product . map flow

getValve :: [Valve] -> Label -> Maybe Valve
getValve [] _ = Nothing
getValve (v:vs) l = if label v == l then Just v else getValve vs l

data Valve = Valve {
    label :: Label,
    flow :: Int,
    next :: [Label]
} deriving (Show, Eq)

valid :: [Valve] -> Bool
valid [] = True
valid [_] = True
valid (u : v: vs) = if label v `elem` next u then valid (v:vs) else False

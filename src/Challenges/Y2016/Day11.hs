{-# LANGUAGE TupleSections #-}
module Challenges.Y2016.Day11 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Data.List (subsequences, nub, sort)
import Common.List (deleteAll)
import Common.Search (dijkstraOn)
import Data.Maybe (fromJust)

solutionA :: String -> String
solutionA = solve parser (snd . fromJust . searchSolution)
solutionB :: String -> String
solutionB = solve parserB (snd . fromJust . searchSolution)

searchSolution :: Building -> Maybe (Building, Int)
searchSolution b = dijkstraOn
    represent
    (map (, 1) . takeElevator)
    [(b, 0)]
    (+)
    (const 1)
    allOnFourth

data Material = El | Di | Pm | Co | Cm | Ru | Pu | H | L deriving (Eq, Ord, Show)
data MachineType = Gen | Chip deriving (Eq, Ord, Show)
type Object = (Material, MachineType)
type Building = (Int, [Object], [Object], [Object], [Object])

takeElevator :: Building -> [Building]
takeElevator b@(e, first, second, third, fourth) =
    [(e', first', second', third', fourth') |
    let curFlor = getObjects b,
    os <- possibleMoves curFlor,
    e' <- ([e + 1 | e < 4]) ++ ([e - 1 | e > 1 && not (emptyBelow e b)]),
    let first' = sort $ updateFloor first os 1 e',
    let second' = sort $ updateFloor second os 2 e',
    let third' = sort $ updateFloor third os 3 e',
    let fourth' = sort $ updateFloor fourth os 4 e',
    all safe [first', second', third', fourth']
    ]

possibleMoves :: [Object] -> [[Object]]
possibleMoves = filter (\l -> not (null l) && length l <= 2 && safe l) 
    . subsequences 

emptyBelow :: Int -> Building -> Bool
emptyBelow 1 _ = True
emptyBelow 2 (_, first, _, _ ,_) = null first
emptyBelow 3 b@(_, _, second, _ ,_) = null second && emptyBelow 2 b
emptyBelow 4 b@(_, _, _, third, _) = null third && emptyBelow 3 b
emptyBelow n b@(_, _, _, _, fourth) = null fourth && emptyBelow (n-1) b

updateFloor :: [Object] -> [Object] -> Int -> Int -> [Object]
updateFloor current other ownNum newNum = if ownNum == newNum
    then nub $ current ++ other
    else deleteAll other current

getObjects :: Building -> [Object]
getObjects (1, first, _, _, _) = first
getObjects (2, _, second, _, _) = second
getObjects (3, _, _, third, _) = third
getObjects (4, _, _, _, fourth) = fourth
getObjects _ = error "invalid building configuration"

allOnFourth :: Building -> Bool
allOnFourth (_, first, second, third, _) = null first && null second && null third

safe :: [Object] -> Bool
safe [] = True
safe os = all (\c -> corresponding c `elem` os || null (generators os)) (chips os)

corresponding :: Object -> Object
corresponding (m, Chip) = (m, Gen)
corresponding (m, Gen) = (m, Chip)

generators :: [Object] -> [Object]
generators = filter (\(_,t) -> t == Gen)

chips :: [Object] -> [Object]
chips = filter (\(_,t) -> t == Chip)

represent :: Building -> (Int, [(Maybe Int, Maybe Int)])
represent b@(flr, _, _, _, _) = (flr, sort $ map (f . toPairs) mats) where
    f ((m1, t1), (m2, t2)) = (find b m1 t1, find b m2 t2)
    toPairs m = ((m, Gen), (m, Chip))
    mats = [Pm, Co, Cm, Ru, Pu, H, L, El, Di]

find :: Building -> Material -> MachineType -> Maybe Int
find (_, first, second, third, fourth) m t
    | (m,t) `elem` first = Just 1
    | (m,t) `elem` second = Just 2
    | (m,t) `elem` third = Just 3
    | (m,t) `elem` fourth = Just 4
    | otherwise = Nothing

parser :: Parser Building
parser = do
    return (1,
        [(Pm, Gen), (Pm,Chip)],
        [(Co,Gen), (Cm,Gen), (Ru, Gen), (Pu, Gen)],
        [(Co,Chip), (Cm, Chip), (Ru, Chip), (Pu,Chip)],
        []
        )

parserB :: Parser Building
parserB = do
    return (1,
        [(Pm, Gen), (Pm,Chip), (Di, Chip), (Di, Gen), (El, Chip), (El, Gen)],
        [(Co,Gen), (Cm,Gen), (Ru, Gen), (Pu, Gen)],
        [(Co,Chip), (Cm, Chip), (Ru, Chip), (Pu,Chip)],
        []
        )

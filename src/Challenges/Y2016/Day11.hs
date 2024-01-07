{-# LANGUAGE TupleSections #-}
module Challenges.Y2016.Day11 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Data.List (subsequences, nub)
import Common.List (deleteAll)
import Common.Search (dijkstra)

solutionA :: String -> String
solutionA = solve parser (searchSolution)
solutionB :: String -> String
solutionB = solve parser (const "")

searchSolution :: Building -> Maybe (Building, Int)
searchSolution b = dijkstra
    (map (,1) . takeElevator)
    [(b,0)]
    (+)
    (const 1)
    allOnFourth


data Material = Pm | Co | Cm | Ru | Pu | H | L deriving (Eq, Ord, Show)
data MachineType = Gen | Chip deriving (Eq, Ord, Show)
type Object = (Material, MachineType)
type Building = (Int, [Object], [Object], [Object], [Object])

takeElevator :: Building -> [Building]
takeElevator b@(e, first, second, third, fourth) =
    [(e', first', second', third', fourth') |
    let curFlor = getObjects b,
    os <- filter (\l -> not (null l) && length l <= 2) $ subsequences curFlor,
    e' <- ([e + 1 | e < 4]) ++ ([e - 1 | e > 1]),
    not (null first && e' == 1),
    not (null second && null first && e' == 2),
    let first' = updateFloor first os 1 e',
    let second' = updateFloor second os 2 e',
    let third' = updateFloor third os 3 e',
    let fourth' = updateFloor fourth os 4 e',
    safeBuilding (e', first', second', third', fourth')
    ]

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

safeBuilding :: Building -> Bool
safeBuilding (_, first, second, third, fourth) = all safeFloor [first, second, third, fourth]

safeFloor :: [Object] -> Bool
safeFloor [] = True
safeFloor os = all (\c -> isPresent os (corresponding c) || null (generators os)) (chips os)

isPresent :: [Object] -> Object -> Bool
isPresent os o = o `elem` os

corresponding :: Object -> Object
corresponding (m, Chip) = (m, Gen)
corresponding (m, Gen) = (m, Chip)

generators :: [Object] -> [Object]
generators = filter (\(_,t) -> t == Gen)

chips :: [Object] -> [Object]
chips = filter (\(_,t) -> t == Chip)


parser :: Parser Building
parser = do
    return (1,
        [(Pm, Gen), (Pm,Chip)],
        [(Co,Gen), (Cm,Gen), (Ru, Gen), (Pu, Gen)],
        [(Co,Chip), (Cm, Chip), (Ru, Chip), (Pu,Chip)],
        []
        )

parser' :: Parser Building
parser' = do
    return (1,
        [(H, Chip), (L,Chip)],
        [(H,Gen)],
        [(L,Gen)],
        []
        )
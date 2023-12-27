module Challenges.Y2023.Day10 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.List (longest)
import Common.Prelude
import Common.Coord (Coord, Dir (..), dist, above, below, left, right, direction)
import Common.Search (bfs)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (delete)
import Data.Set (Set)
import qualified Data.Set as Set

solutionA :: String -> String
solutionA = solve parser (flip div 2 . length . findLoop)
solutionB :: String -> String
solutionB = solve parser (Set.size . solveB)

solveB :: Map Coord Pipe -> Set Coord
solveB m = let
    p = findLoop m
    coords = Set.fromList $ map fst p
    candidates = Set.fromList (concatMap (rightof m) p) `Set.difference` coords
    contained = Set.fromList (bfs (neighbours m coords) (Set.toList candidates))
    in contained

-- inner :: Set Coord
-- inner = Set.fromList (bfsN (openNeighbors input pipe) (Set.toList candidates))

findLoop :: Map Coord Pipe -> [(Coord, Dir)]
findLoop m = let s = start m in
  longest $ filter (not . null) $ startFollow m s Start

data Pipe = Hor | Vert | NE | NW | SW | SE | Start | Ground deriving (Eq, Show)

startFollow :: Map Coord Pipe -> Coord -> Pipe -> [[(Coord, Dir)]]
startFollow m c Start = map (\n -> follow m c n (Map.lookup n m)) [right c, above c, below c, left c]
startFollow _ _ _ = error "can only call StartFollow on Start pipe"

follow :: Map Coord Pipe -> Coord -> Coord -> Maybe Pipe -> [(Coord, Dir)]
follow _ _ _ (Just Ground) = []
follow _ _ _ Nothing = []
follow m prev c q
    | not $ connects (prev, fromJust $ Map.lookup prev m) (c, fromJust q) = []
follow m prev c (Just Hor) = (c, direction prev c) : concatMap (\n -> follow m c n (Map.lookup n m)) (delete prev [left c, right c])
follow m prev c (Just Vert) = (c, direction prev c) : concatMap (\n -> follow m c n (Map.lookup n m)) (delete prev [above c, below c])
follow m prev c (Just NE) = (c, direction prev c) : concatMap (\n -> follow m c n (Map.lookup n m)) (delete prev [above c, right c])
follow m prev c (Just NW) = (c, direction prev c) : concatMap (\n -> follow m c n (Map.lookup n m)) (delete prev [above c, left c])
follow m prev c (Just SW) = (c, direction prev c) : concatMap (\n -> follow m c n (Map.lookup n m)) (delete prev [below c, left c])
follow m prev c (Just SE) = (c, direction prev c) : concatMap (\n -> follow m c n (Map.lookup n m)) (delete prev [below c, right c])
follow _ prev c (Just Start) = [(c, direction prev c)]

connects :: (Coord, Pipe) -> (Coord,Pipe) -> Bool
connects (_, Ground) _ = False
connects _ (_, Ground) = False
connects (c@(cx,cy),p) (d@(dx,dy),q)
    | dist c d > 1 = False
    | cy < dy = hasSouth p && hasNorth q
    | cx < dx = hasEast p && hasWest q
    | cy > dy = hasNorth p && hasSouth q
    | cx > dx = hasWest p && hasEast q
    | otherwise = False

hasNorth :: Pipe -> Bool
hasNorth Start = True
hasNorth Vert = True
hasNorth NE = True
hasNorth NW = True
hasNorth _ = False

hasSouth :: Pipe -> Bool
hasSouth Start = True
hasSouth Vert = True
hasSouth SE = True
hasSouth SW = True
hasSouth _ = False

hasWest :: Pipe -> Bool
hasWest Start = True
hasWest Hor = True
hasWest NW = True
hasWest SW = True
hasWest _ = False

hasEast :: Pipe -> Bool
hasEast Start = True
hasEast Hor = True
hasEast NE = True
hasEast SE = True
hasEast _ = True

start :: Map Coord Pipe -> Coord
start m = start' $ Map.assocs m where
    start' :: [(Coord, Pipe)] -> Coord
    start' [] = error "no start node found"
    start' ((c,p):cs) = if p == Start then c else start' cs

parser :: Parser (Map Coord Pipe)
parser = do
    ps <- concat <$> sepEndBy1 (many1 pipe) newline
    return (Map.fromList ps)

pipe :: Parser  (Coord, Pipe)
pipe = do
    pos <- getPosition
    let c = (sourceColumn pos - 1, sourceLine pos - 1)
    p <- (char '.' >> return Ground) <|>
        (char '|' >> return Vert) <|>
        (char '-' >> return Hor)<|>
        (char 'L' >> return NE)<|>
        (char 'J' >> return NW) <|>
        (char 'F' >> return SE) <|>
        (char '7' >> return SW) <|>
        (char 'S' >> return Start)
    return (c, p)


--- B
neighbours :: Map Coord a -> Set.Set Coord -> Coord -> [Coord]
neighbours input p x = [y | y <- [above x, left x, right x, below x], Map.member y input , Set.notMember y p]

rightof :: Map Coord Pipe -> (Coord, Dir) -> [Coord]
rightof input (x,dir) =
  case input Map.! x of
    Hor | dir == R -> [below x]
    Hor | dir == L -> [above x]
    Vert | dir == U -> [right x]
    Vert | dir == D -> [left x]
    SE | dir == L -> [above x, left x]
    NW | dir == R -> [below x, right x]
    SW | dir == U -> [above x, right x]
    NE | dir == D -> [below x, left x]
    _ -> []

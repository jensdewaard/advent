module Challenges.Y2023.Day10 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve, Coord, distanceC)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor (second)
import Data.Maybe (isJust, fromJust)
import Data.List (delete)

solutionA :: String -> String
solutionA = solve parser (maxWalk . maximum .map length . solveA)
solutionB :: String -> String
solutionB = solve parser (const "")

maxWalk :: Int -> Int
maxWalk n = n `div` 2

solveA :: Map Coord Pipe -> [[Coord]]
solveA m = filter (not . null) $ startFollow m s Start where s = start m

data Pipe = Hor | Vert | NE | NW | SW | SE | Start deriving (Eq, Show)

startFollow :: Map Coord Pipe -> Coord -> Pipe -> [[Coord]]
startFollow m c@(x,y) Start = map (\n -> follow m c n (Map.lookup n m)) [(x-1,y), (x,y-1),(x+1,y),(x,y+1)]
startFollow _ _ _ = error "can only call StartFollow Start pipe"

follow :: Map Coord Pipe -> Coord -> Coord -> Maybe Pipe -> [Coord]
follow m prev c q
    | not $ connects (prev, Map.lookup prev m) (c, q) = []
follow _ _ _ Nothing = []
follow m prev c@(x,y) (Just Hor) = c : concatMap (\n -> follow m c n (Map.lookup n m)) (delete prev [(x-1,y), (x+1, y)])
follow m prev c@(x,y) (Just Vert) = c : concatMap (\n -> follow m c n (Map.lookup n m)) (delete prev [(x,y-1), (x, y+1)])
follow m prev c@(x,y) (Just NE) = c : concatMap (\n -> follow m c n (Map.lookup n m)) (delete prev [(x,y-1), (x+1, y)])
follow m prev c@(x,y) (Just NW) = c : concatMap (\n -> follow m c n (Map.lookup n m)) (delete prev [(x,y-1), (x-1, y)])
follow m prev c@(x,y) (Just SW) = c : concatMap (\n -> follow m c n (Map.lookup n m)) (delete prev [(x,y+1), (x-1, y)])
follow m prev c@(x,y) (Just SE) = c : concatMap (\n -> follow m c n (Map.lookup n m)) (delete prev [(x,y+1), (x+1, y)])
follow _ _rev c (Just Start)  = [c]

connects :: (Coord, Maybe Pipe) -> (Coord,Maybe Pipe) -> Bool
connects (_, Nothing) _ = False
connects _ (_, Nothing) = False
connects (c@(cx,cy),Just p) (d@(dx,dy),Just q)
    | distanceC c d > 1 = False
    | p == Start || q == Start = True
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
    return (Map.fromList $ map (second fromJust) $ filter (isJust . snd) ps)

pipe :: Parser  (Coord, Maybe Pipe)
pipe = do
    pos <- getPosition
    let c = (sourceColumn pos - 1, sourceLine pos - 1)
    p <- (char '.' >> return Nothing) <|>
        (char '|' >> return (Just Vert)) <|>
        (char '-' >> return (Just Hor))<|>
        (char 'L' >> return (Just NE))<|>
        (char 'J' >> return (Just NW)) <|>
        (char 'F' >> return (Just SE)) <|>
        (char '7' >> return (Just SW)) <|>
        (char 'S' >> return (Just Start))
    return (c, p)


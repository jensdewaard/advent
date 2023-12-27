module Challenges.Y2019.Day03 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec

import Common.Prelude
import Common.Coord (Coord, Dir (..))
import qualified Data.List as L

solutionA :: String -> String
--solutionA _ = show 0
solutionA = solve parseInput closest

solutionB :: String -> String
solutionB = solve parseInput (best . wires)

wires :: [Wire] -> [[Coord]]
wires = map (wireToCoords (0,0))

closest :: [Wire] -> Int
closest = minimum . filter (>0) . map manDist . intersections . wires

best :: [[Coord]] -> Int
best w@(wa : wb : _) = minimum . filter (>0) . map (\c -> pathToLength wa c + pathToLength wb c) $ intersections w
best _ = 0

pathToLength :: [Coord] -> Coord -> Int
pathToLength (w : ws) c = if w == c then 0 else 1 + pathToLength ws c
pathToLength [] _ = 0

parseInput :: Parser [Wire]
parseInput = do
    a <- sepBy1 dir (char ',')
    _ <- newline
    b <- sepBy1 dir (char ',')
    return [a,b]

intersections :: [[Coord]] -> [Coord]
intersections (a : b : _) = a `L.intersect` b
intersections _ = error "invalid number of wires"

-- | The Manhattan distance from the Origin (0,0) to the Coordinate.
manDist :: Coord -> Int
manDist (x, y) = abs x + abs y

type Wire = [DirAndLength]

data DirAndLength = DirLength Dir Int

dir :: Parser DirAndLength
dir = do
    d <- (char 'U' >> return U) <|> (char 'L' >> return L) <|> (char 'R' >> return R) <|> (char 'D' >> return D)
    n <- read <$> many1 digit
    return $ DirLength d n

wireToCoords :: Coord -> Wire -> [Coord]
wireToCoords s (DirLength _ 0 : ds) = wireToCoords s ds
wireToCoords s@(x,y) (DirLength U n : ds) = s : wireToCoords s' (DirLength U (n-1) : ds) where
    s' = (x, y - 1)
wireToCoords s@(x,y) (DirLength D n : ds) = s : wireToCoords s' (DirLength D (n-1) : ds) where
    s' = (x, y + 1)
wireToCoords s@(x,y) (DirLength L n : ds) = s : wireToCoords s' (DirLength L (n-1) : ds) where
    s' = (x - 1, y)
wireToCoords s@(x,y) (DirLength R n : ds) = s : wireToCoords s' (DirLength R (n-1) : ds) where
    s' = (x + 1, y)
wireToCoords s [] = [s]

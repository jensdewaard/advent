{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Challenges.Y2016.Day22 (solutionA, solutionB) where
import Common.Prelude
import Common.Parsing (int)
import Common.Coord (Coord, dist, cardinal, showMapWith)
import Text.ParserCombinators.Parsec

solutionA :: String -> String
-- 1163196 is too high
-- 1164240 is the number of possible pairs
solutionA = solve parser (\ns -> length $ [(a,b) | a <- ns, b <- ns, viable a b])
solutionB :: String -> String
solutionB = solve parser (showMapWith draw . map (\n -> (coord n, n)))

parser :: Parser [Node]
parser = do
    _ <- string "root@ebhq-gridcenter# df -h" >> newline
    _ <- string "Filesystem              Size  Used  Avail  Use%" >> newline
    node `sepEndBy` newline

node :: Parser Node
node = do
    _ <- string "/dev/grid/node-x"
    x <- int
    _ <- string "-y"
    y <- int
    _ <- spaces
    s <- int
    _ <- char 'T'
    _ <- spaces
    u <- int
    _ <- char 'T'
    _ <- spaces
    a <- int
    _ <- char 'T'
    _ <- spaces
    p <- int
    _ <- char '%'
    return $ Node x y s u a p

draw :: Node -> Char
draw n
    | coord n == (0,0) = 'E'
    | coord n == (35,0) = 'S'
    | used n == 0  = '_'
    | size n >= 250 = '#'
    | otherwise    = '.'

canAbsorb :: Node -> Node -> Bool
canAbsorb n m = size n >= size m

inBounds :: Coord -> Bool
inBounds (x,y) = x >= 0 && y >= 0 && x <= 35 && y <= 29

getNode :: [Node] -> Coord -> Node
getNode [] _ = error "no nodes"
getNode (n : ns) (x,y) = if xCoord n == x && yCoord n == y then n else getNode ns (x,y)


viable :: Node -> Node -> Bool
viable a b = used a > 0 && differentNodes a b && used a <= avail b

viableA :: Node -> Node -> Bool
viableA a b = viable a b && adjacent a b

differentNodes :: Node -> Node -> Bool
differentNodes a b = xCoord a /= xCoord b || yCoord a /= yCoord b

adjacent :: Node -> Node -> Bool
adjacent a b = dist (xCoord a, yCoord a) (xCoord b, yCoord b) == 1

data Node where
  Node :: {xCoord :: Int, yCoord :: Int, size :: Int, used :: Int, avail :: Int, perc :: Int} -> Node
  deriving (Eq, Show)

coord :: Node -> Coord
coord n = (xCoord n, yCoord n)

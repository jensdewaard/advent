module Common.Coord (Coord, Dir(..), move, dist, above, below, left, right, direction) where

type Coord = (Int, Int)

data Dir = U | D| L| R deriving (Show, Eq)

move :: Dir -> Coord -> Coord
move U c = above c
move D c = below c
move L c = left c
move R c = right c

dist :: Coord -> Coord -> Int
dist (px, py) (qx,qy) = abs (px - qx) + abs (py - qy)

above :: Coord -> Coord
above (x,y) = (x,y-1)

below :: Coord -> Coord
below (x,y) = (x, y+1)

left :: Coord -> Coord
left (x,y) = (x-1, y)

right :: Coord -> Coord
right (x,y) = (x+1,y)

direction :: Coord -> Coord -> Dir
direction x y
    | y == above x = U
    | y == below x = D
    | y == right x = R
    | y == left x = L
    | otherwise = error "coords not adjacent"

cardinal :: Coord -> [Coord]
cardinal c = [above c, right c, below c, left c]

diag :: Coord -> [Coord]
diag c = [above $ left c, above $ right c, below $ left c, below $ right c]
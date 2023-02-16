{-# LANGUAGE BinaryLiterals #-}
module Challenges.Y2022.Day17 (minmax, solveA, input) where
import Data.Maybe (fromJust)
import Data.Foldable ( Foldable(toList) )
import Shared (Coord)

data Dir = L | R | D deriving (Eq, Show)
type Surface = [Int]

--type Rock = S.Set Coord
data World = World {
    surface :: Surface,
    fallingRock :: Maybe Rock,
    stackHeight :: Int,
    rockIndex :: Int,
    jetIndex :: Int
}

world :: World
world = mkWorld $ replicate 7 0

mkWorld :: [Int] -> World
mkWorld s = World {surface=s, stackHeight=0, fallingRock = Nothing, rockIndex = 0, jetIndex=0}

input :: [Dir]
input = [R,R,R,L,L,R,L,R,R,L,L,L,R,R,L,R,R,R,L,L,L,R,R,R,L,L,L,R,L,L,L,R,R,L,R,R,L,L,R,R]

wouldCollide :: Rock -> Surface -> Bool
wouldCollide ((x,y),SquareRock) w = True

move :: Dir -> Rock -> Rock
move L ((x, y), s) = ((x-1, y), s)
move R ((x, y), s) = ((x+1, y), s)
move D ((x, y), s) = ((x, y - 1), s)

minmax :: (Int, Int)
minmax = (l, t) where
    is = scanl step 0 input
    l = minimum is
    t = maximum is
    step :: Int -> Dir -> Int
    step i L = i - 1
    step i R = i + 1
    step i D = i

solveA :: [Dir] -> Int
solveA ds = undefined

sim :: [Int -> Rock] -> [Dir] -> World -> World
sim [] _ _ = error "no rocks given"
sim _ [] _ = error "no jet streams"
sim rs _ w@World{fallingRock=Nothing} = w{fallingRock=r, rockIndex=(rockIndex w) + 1 } where
    r = Just $ rs !! (rockIndex w) $ (stackHeight w + 3)
sim _ ds w = w{fallingRock = mr, jetIndex=(jetIndex w) + 1, surface=stack', stackHeight = sh} where
        r = fromJust $ fallingRock w
        dir = ds !! (jetIndex w)
        r' = move dir r
        collisionLR = undefined
        r'' = if collisionLR then r else r'
        r''' = move D r''
        collisionD = undefined
        mr = if collisionD then Nothing else Just r'''
        stack' = undefined
        sh = undefined

--- Rocks
-- flatRock :: Int -> Rock
-- flatRock y = S.fromList [(-1, y), (0,y), (1,y), (2,y)]

-- plusRock :: Int -> Rock
-- plusRock y = S.fromList [(0, y+2), (-1, y+1), (0, y+1), (1, y+1), (0, y)]

-- elRock :: Int -> Rock
-- elRock y = S.fromList [(-1, y), (0, y), (1, y), (1, y+1), (1, y+2)]

-- thinRock :: Int -> Rock
-- thinRock y = S.fromList [(-1, y), (-1, y+1), (-1, y+2), (-1, y+3)]

-- squareRock :: Int -> Rock
-- squareRock y = S.fromList [(-1, y), (-1, y+1), (0, y), (0, y+1)]
-- rocks :: [Int -> Rock]
-- rocks = [flatRock, plusRock, elRock, thinRock, squareRock]

type Rock = (Coord, RockShape)
data RockShape = FlatRock | PlusRock | ElRock | ThinRock | SquareRock


normalizeSurface :: [Int] -> ([Int], Int)
normalizeSurface is = (map (\n -> n-m) is, m)
    where m = 0
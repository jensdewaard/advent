module Challenges.Y2019.Day12 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Common.Vector3 (Vector3 (x, y, z), vector3)
import Common.Parsing (int)
import Control.Arrow ((>>>))

solutionA :: String -> String
solutionA = solve parser (step 1000 >>> energy)
solutionB :: String -> String
-- solutionB = solve parser (\ms -> [gravity (pos m1) (pos m2) | m1 <- ms, m2 <- ms ])
solutionB = const ""


gravity :: Vector3 Int -> Vector3 Int -> Vector3 Int
gravity v w = order <$> (compare <$> w <*> v) where
    order LT = -1
    order EQ = 0
    order GT = 1

step :: Int -> [Moon] -> [Moon]
step n ms = iterate (\ moons -> map (timestep moons) moons) ms !! n

energy :: [Moon] -> Int
energy = sum . map energyMoon where
    energyMoon :: Moon -> Int
    energyMoon (Moon p v) = (abs (x p) + abs (y p) + abs (z p)) * (abs (x v) + abs (y v) + abs (z v)) 

timestep :: [Moon] -> Moon -> Moon
timestep ms m = let
    newVelocity = mconcat $ vel m : [gravity (pos m) (pos m1) | m1 <- ms ]
    in Moon (pos m + newVelocity) newVelocity


data Moon = Moon { pos :: Vector3 Int, vel :: Vector3 Int } deriving (Eq, Show)
parser :: Parser [Moon]
parser = moon `sepEndBy1` newline where
    moon :: Parser Moon
    moon = do
        _ <- string "<x="
        a <- int
        _ <- string ", y="
        b <- int
        _ <- string ", z="
        c <- int
        _ <- string ">"
        return $ Moon (vector3 a b c) mempty

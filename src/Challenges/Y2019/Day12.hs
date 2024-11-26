{-# LANGUAGE RankNTypes #-}

module Challenges.Y2019.Day12 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Common.Vector3 (Vector3 (x, y, z), vector3)
import Common.Parsing (int)
import Control.Arrow ((>>>))

solutionA :: String -> String
solutionA = solve parser (run >>> (!!1000) >>> fmap energy)
solutionB :: String -> String
solutionB = solve parser (traverse transpose >>> fmap (period timestep') >>> foldl lcm 1)

period :: (Eq a, Num a) => ([(a,a)] -> [(a,a)]) -> [(a, a)] -> Int
period f ms = go 1 (f ms) where
    go n xs = if xs == ms then n else go (n +1) (f xs)

gravity :: Num a => a -> a -> a
gravity v w = signum (v - w)

run :: Num a => [(a, a)] -> [[(a, a)]]
run = iterate timestep'

energy :: (Foldable f, Num (f Int)) => (f Int, f Int) -> Int
energy(p , v) = sum (abs p) * sum (abs v)

timestep' :: Num a => [(a, a)] -> [(a, a)]
timestep' = map timePosition . timeSpeed

timePosition :: Num a => (a,a) -> (a,a)
timePosition (p, v) = (p + v, v)

timeSpeed :: Num a => [(a, a)] -> [(a, a)]
timeSpeed moons = let
    -- newSpeed ms m = sum $ m : [signum (m - m1) | m1 <- ms]
    newSpeed ms m = foldr (\(other, _) (x, v) -> (x, v + gravity other x)) m ms
    -- in map (\m -> snd m : [gravity (fst m) (fst m1) | m1 <- ms ]) ms
    in map (newSpeed moons) moons

transpose :: (Vector3 Int, Vector3 Int) -> Vector3 (Int, Int)
transpose (v, w) = vector3 (x v, x w) (y v, y w) (z v , z w)


type Moon = (Vector3 Int, Vector3 Int)
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
        return (vector3 a b c,mempty)

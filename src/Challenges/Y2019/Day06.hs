module Challenges.Y2019.Day06 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec

import Data.Tree (unfoldTree, Tree, levels)
import Common.Search (simple)
import Data.Maybe (fromJust)

solutionA :: String -> String
solutionA = solve parser (sum . zipWith (curry mult) [0..] . map length . levels . buildOrbitTree)
solutionB :: String -> String
solutionB = solve parser (\obs -> distance obs (head $ parent obs $ Object "YOU") (head $ parent obs $ Object "SAN"))

mult :: (Int,Int) -> Int
mult (a,b) = a * b

newtype Object = Object String deriving (Eq, Show, Ord)
data Orbit = Orbit Object Object deriving (Eq, Show, Ord)

orbitee :: Orbit -> Object
orbitee (Orbit o1 _) = o1

orbiter :: Orbit -> Object
orbiter (Orbit _ o2) = o2

buildOrbitTree :: [Orbit] -> Tree Object
buildOrbitTree obs = let
    in unfoldTree (\o -> (o, children obs o)) (Object "COM")

children :: [Orbit] -> Object -> [Object]
children obs o = map orbiter $ filter ((==o) . orbitee) obs

parent :: [Orbit] -> Object -> [Object]
parent obs o = map orbitee $ filter ((==o) . orbiter) obs 

distance :: [Orbit] -> Object -> Object -> Int
distance obs start needle = snd $ fromJust $ simple
    (\o -> parent obs o ++ children obs o )
    start
    (==needle)


parser :: Parser [Orbit]
parser = (do
    o1 <- many1 alphaNum
    _ <- char ')'
    o2 <- many1 alphaNum
    return $ Orbit (Object o1) (Object o2))
    `sepEndBy` newline
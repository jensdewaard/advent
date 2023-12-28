module Challenges.Y2016.Day01 (solutionA, solutionB) where
import Common.Prelude
import Common.Coord (Dir (..), Coord, moveN, origin, dist)
import Text.ParserCombinators.Parsec 
import qualified Data.Set as Set

solutionA :: String -> String
solutionA = parser ==> dist origin . snd . foldl move (U, origin)
solutionB :: String -> String
solutionB = parser' ==> dist origin . firstDouble . scanl move (U, origin)

firstDouble :: [(Dir, Coord)] -> Coord
firstDouble = go Set.empty
    where 
        go _ [] = error "no duplicates"
        go seen ((_,c):cs)
            | Set.member c seen = c
            | otherwise = go (Set.insert c seen) cs


move :: (Dir, Coord) -> (Dir -> Dir, Int) -> (Dir, Coord)
move (d,c) (f, n)= (d',c') where
    d' = f d
    c' = moveN n d' c

turnLeft :: Dir -> Dir
turnLeft U = L
turnLeft L = D
turnLeft D = R
turnLeft R = U

turnRight :: Dir -> Dir
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

parser :: Parser [(Dir -> Dir, Int)]
parser = instruction `sepBy` string ", "
    where instruction = do
            f <- (char 'L' >> return turnLeft) <|> (char 'R' >> return turnRight)
            n <- read <$> many1 digit
            return (f,n)

parser' :: Parser [(Dir -> Dir, Int)]
parser' = concat <$> instruction `sepBy` string ", "
    where instruction = do
            f <- (char 'L' >> return turnLeft) <|> (char 'R' >> return turnRight)
            n <- read <$> many1 digit
            return $ (f,1) : replicate (n-1) (id,1)
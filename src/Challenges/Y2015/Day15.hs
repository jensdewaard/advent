module Challenges.Y2015.Day15 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve)
import Data.List (transpose)

solutionA :: String -> String
solutionA = solve parser (maximum . scores 100 (const True))

solutionB :: String -> String
solutionB _ = ""

data Ingredient = Ingredient 
    { name          :: String
    , capacity      :: Int
    , durability    :: Int
    , flavor        :: Int
    , texture       :: Int
    , calories      :: Int
    }

scores :: Int -> [Ingredient] -> [Int]
scores total ings =
    [ product . map (max 0 . sum) $ transpose totes
    | ms <- partitions (length ings) total
    , let totes = zipWith (\n i -> map (n*) (scorings <*> pure i)) ms ings
    , zipWith (\n i -> n * calories i) ms ings
    ]
    where scorings = [capacity, durability, flavor, texture]


instance Show Ingredient where
    show (Ingredient n cp d f t cl) = n ++ ": capacity " ++ show cp ++ ", durability " ++ show d ++ ", flavor " ++ show f ++ ", texture " ++ show t ++ ", calories " ++ show cl ++ "\n"

parser :: Parser [Ingredient]
parser = sepBy1 ingredient newline where
    ingredient = do
        n <- many1 letter
        _ <- string ": capacity "
        cp <- int
        _ <- string ", durability "
        d <- int
        _ <- string ", flavor "
        f <- int
        _ <- string ", texture "
        t <- int
        _ <- string ", calories "
        cl <- int
        return $ Ingredient n cp d f t cl

partitions :: Int -> Int -> [[Int]]
partitions 1 t = [[t]]
partitions n t = [ x : xs | x <- [0..t], xs <- partitions (n-1) $ t-x ]

sign :: Parser (Int -> Int)
sign = try (char '-' >> return negate) <|> return id

int :: Parser Int
int = do
    f <- sign
    n <- many1 digit
    return (f $ read n)
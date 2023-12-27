module Challenges.Y2015.Day15 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.Prelude
import Common.Parsing (int)

solutionA :: String -> String
solutionA = solve parser (\is -> maximum $ map (eqn is) combos)

combos :: [[Int]]
combos = [[a, b, c, d] |
            a <- [0 .. 100],
            b <- [0 .. 100],
            c <- [0 .. 100],
            d <- [0 .. 100],
            a + b + c + d == 100]

solutionB :: String -> String
solutionB = solve parser (\is -> maximum $ map (eqnCal is) combos)

eqn :: [Ingredient] -> [Int] -> Int
eqn ins amounts = let
    cap = max 0 $ sum $ zipWith (*) (map capacity ins) amounts
    dura = max 0 $ sum $ zipWith (*) (map durability ins) amounts
    flav = max 0 $ sum $ zipWith (*) (map flavor ins) amounts
    text = max 0 $ sum $ zipWith (*) (map texture ins) amounts
    in cap * dura * flav * text

eqnCal :: [Ingredient] -> [Int] -> Int
eqnCal ins amounts = let
    cap = max 0 $ sum $ zipWith (*) (map capacity ins) amounts
    dura = max 0 $ sum $ zipWith (*) (map durability ins) amounts
    flav = max 0 $ sum $ zipWith (*) (map flavor ins) amounts
    text = max 0 $ sum $ zipWith (*) (map texture ins) amounts
    cal = sum $ zipWith (*) (map calories ins) amounts
    in cap * dura * flav * text * (if cal == 500 then 1 else 0)

data Ingredient = Ingredient
    { name          :: String
    , capacity      :: Int
    , durability    :: Int
    , flavor        :: Int
    , texture       :: Int
    , calories      :: Int
    }

instance Eq Ingredient where (==) i j = name i == name j
instance Ord Ingredient where
    compare i j = compare (name i) (name j)

instance Show Ingredient where
    show (Ingredient n cp d f t cl) = "("++ n ++ ": capacity " ++ show cp ++ ", durability " ++ show d ++ ", flavor " ++ show f ++ ", texture " ++ show t ++ ", calories " ++ show cl ++ ")"

parser :: Parser [Ingredient]
parser = sepEndBy1 ingredient newline where
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
        Ingredient n cp d f t <$> int
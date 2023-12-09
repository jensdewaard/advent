module Challenges.Y2015.Day16 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve)
import Parsing (int)
import Data.Set (fromList, isSubsetOf, Set, singleton)

solutionA :: String -> String
solutionA = solve parser (head . map fst . filter (\(_, fs) -> fs `isSubsetOf` knownFacts))
solutionB :: String -> String
solutionB = solve parser (head . map fst . filter (\(_, facts) -> all' facts fit))

data Fact = Children Int
    | Cats Int
    | Samoyeds Int
    | Pomeranians Int
    | Akitas Int
    | Vizslas Int
    | Goldfish Int
    | Trees Int
    | Cars Int
    | Perfumes Int deriving (Eq, Ord)

all' :: Set a -> (a -> Bool) -> Bool
all' = flip all

fit :: Fact -> Bool
fit (Cats n) = n > 7
fit (Trees n) = n > 3
fit (Pomeranians n) = n < 3
fit (Goldfish n) = n < 5
fit f = singleton f `isSubsetOf` knownFacts

knownFacts :: Set Fact
knownFacts = fromList [
    Children 3,
    Cats 7,
    Samoyeds 2,
    Pomeranians 3,
    Akitas 0,
    Vizslas 0,
    Goldfish 5,
    Trees 3,
    Cars 2,
    Perfumes 1
    ]

parser :: Parser [(Int, Set Fact)]
parser = sepEndBy1 sue newline

sue :: Parser (Int, Set Fact)
sue = do
    _ <- string "Sue "
    n <- int
    _ <- string ": "
    fs <- fact `sepBy` string ", "
    return (n, fromList fs)

fact :: Parser Fact
fact = (do _ <- try $ string "goldfish: "; Goldfish <$> int)
    <|> (do _ <- try $ string "children: "; Children <$> int)
    <|> (do _ <- try $ string "cats: "; Cats <$> int)
    <|> (do _ <- try $ string "samoyeds: "; Samoyeds <$> int)
    <|> (do _ <- try $ string "pomeranians: "; Pomeranians <$> int)
    <|> (do _ <- try $ string "akitas: "; Akitas <$> int)
    <|> (do _ <- try $ string "vizslas: "; Vizslas <$> int)
    <|> (do _ <- try $ string "trees: "; Trees <$> int)
    <|> (do _ <- try $ string "cars: "; Cars <$> int)
    <|> (do _ <- try $ string "perfumes: "; Perfumes <$> int)

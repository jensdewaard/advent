{-# Language ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
module Challenges.Y2023.Day07 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.Prelude
import Data.List (nub, sortBy)
import Data.Bifunctor (first)
import Data.Ord (comparing, Down (Down))

solutionA :: String -> String
solutionA = solve parseInput (winnings compare)
solutionB :: String -> String
solutionB = solve parseInput (winnings compareHandJoker . map (first upgrade))

winnings :: ((Hand, Int) -> (Hand, Int) -> Ordering) -> [(Hand, Int)] -> Int
winnings st = sum . zipWith (curry (\(rank,(_,bid)) -> rank * bid)) [1..] . sortBy st

upgrade :: Hand -> Hand
upgrade (Hand (t,cs))
    | J `notElem` cs = Hand (t,cs)
upgrade (Hand (FiveOfAKind, cs)) = Hand(FiveOfAKind, cs)
upgrade (Hand (FourOfAKind, cs)) = Hand(FiveOfAKind, cs)
upgrade (Hand (FullHouse, cs)) = Hand (FiveOfAKind, cs)
upgrade (Hand (ThreeOfAKind, cs)) = Hand (FourOfAKind, cs)
upgrade (Hand (TwoPair, cs@(countCard J -> 1))) = Hand (FullHouse, cs)
upgrade (Hand (TwoPair, cs)) = Hand (FourOfAKind, cs)
upgrade (Hand (OnePair, cs)) = Hand (ThreeOfAKind, cs)
upgrade (Hand (HighCard, cs)) = Hand (OnePair, cs)

countCard :: Card -> [Card] -> Int
countCard _ [] = 0
countCard x (c:cs) = if x == c then 1 + countCard x cs else countCard x cs

-- Data Structures
newtype Hand = Hand (HandType, [Card]) deriving (Eq, Show)
data HandType = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard deriving (Eq, Show)
data Card = A | K | Q | J | T | Number Int deriving (Eq, Show)

instance Ord Hand where
    compare :: Hand -> Hand -> Ordering
    Hand (t1, c1) `compare` Hand (t2, c2) = compare t1 t2 <> compare c1 c2

instance Ord HandType where
    FiveOfAKind `compare` FiveOfAKind = EQ
    FiveOfAKind `compare` _ = GT
    FourOfAKind `compare` FiveOfAKind = LT
    FourOfAKind `compare` FourOfAKind = EQ
    FourOfAKind `compare` _ = GT
    FullHouse `compare` FiveOfAKind = LT
    FullHouse `compare` FourOfAKind = LT
    FullHouse `compare` FullHouse = EQ
    FullHouse `compare` _ = GT
    ThreeOfAKind `compare` ThreeOfAKind = EQ
    ThreeOfAKind `compare` TwoPair = GT
    ThreeOfAKind `compare` OnePair = GT
    ThreeOfAKind `compare` HighCard = GT
    ThreeOfAKind `compare` _ = LT
    TwoPair `compare` TwoPair = EQ
    TwoPair `compare` OnePair = GT
    TwoPair `compare` HighCard = GT
    TwoPair `compare` _ = LT
    OnePair `compare` OnePair = EQ
    OnePair `compare` HighCard = GT
    OnePair `compare` _ = LT
    HighCard `compare` HighCard = EQ
    HighCard `compare` _ = LT

instance Ord Card where
    A `compare` x = if x == A then EQ else GT
    K `compare` A = LT
    K `compare` x = if x == K then EQ else GT
    Q `compare` A = LT
    Q `compare` K = LT
    Q `compare` x = if x == Q then EQ else GT
    J `compare` A = LT
    J `compare` K = LT
    J `compare` Q = LT
    J `compare` x = if x == J then EQ else GT
    T `compare` A = LT
    T `compare` K = LT
    T `compare` Q = LT
    T `compare` J = LT
    T `compare` x = if x == T then EQ else GT
    Number n `compare` Number m = n `compare` m
    Number _ `compare` _ = LT

compareHandJoker :: (Hand, Int) -> (Hand, Int) -> Ordering
compareHandJoker (Hand (t1, cs1), _) (Hand (t2, cs2), _) =
    if t1 == t2 then compareCards cs1 cs2 else compare t1 t2

compareCards :: [Card] -> [Card] -> Ordering
compareCards (a:as) (b:bs) = case compareJoker a b of
    GT -> GT
    LT -> LT
    EQ -> compareCards as bs
compareCards _ _ = EQ

compareJoker :: Card -> Card -> Ordering
compareJoker J J = EQ
compareJoker J _ = LT
compareJoker _ J = GT
compareJoker x y = compare x y

-- Parsers
parseInput :: Parser [(Hand, Int)]
parseInput = sepEndBy1 hand newline

hand :: Parser (Hand, Int)
hand = do
    cs <- count 5 card
    _ <- space
    bid <- read <$> many1 digit
    return (determine (sortBy (comparing Down) cs) cs, bid)

card :: Parser Card
card = do
    s <- alphaNum
    return (case s of
        'A' -> A
        'K' -> K
        'Q' -> Q
        'J' -> J
        'T' -> T
        x -> Number (read [x])
        )

determine :: [Card] -> [Card] -> Hand
determine [a, b, c, d, e] orig
    | length (nub orig) == 5 = Hand (HighCard, orig)
    | a == b && b == c && c == d && d == e = Hand (FiveOfAKind, orig)
    | a == b && b == c && c /= d && d == e = Hand (FullHouse, orig)
    | a == b && b /= c && c == d && d == e = Hand (FullHouse, orig)
    | a == b && b == c && c == d && d /= e = Hand (FourOfAKind, orig)
    | a /= b && b == c && c == d && d == e = Hand (FourOfAKind, orig)
    | a == b && b == c && c /= d && c /= e = Hand (ThreeOfAKind, orig)
    | a /= b && b == c && c == d && d /= e = Hand (ThreeOfAKind, orig)
    | a /= c && b /= c && c == d && d == e = Hand (ThreeOfAKind, orig)
    | a == b && c == d = Hand (TwoPair, orig)
    | a == b && d == e = Hand (TwoPair, orig)
    | b == c && d == e = Hand (TwoPair, orig)
    | a == b = Hand (OnePair, orig)
    | b == c = Hand (OnePair, orig)
    | c == d = Hand (OnePair, orig)
    | d == e = Hand (OnePair, orig)
determine _ _ = error "invalid number of cards in hand"
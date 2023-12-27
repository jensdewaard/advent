module Challenges.Y2023.Day04 where
import Text.ParserCombinators.Parsec

import Data.List (intersect, union, sortBy)
import Common.Prelude

solutionA :: String -> String
solutionA = solve parseInput (sum . map score)

solutionB :: String -> String
solutionB = solve parseInput (sum . map snd . copies . initCards)

cardId :: Card -> Int
cardId (i,_,_) = i

initCards :: [Card] -> [(Card, Int)]
initCards cs = zip cs $ repeat 1

inc :: Int -> Int -> [(Card, Int)] -> [(Card, Int)]
inc _ 0 cs = cs
inc m n ((c,k):cs) = (c,k+m) : inc m (n-1) cs

copies :: [(Card, Int)] -> [(Card, Int)]
copies [] = []
copies (p@(c@(_,ws,hs),i):cs) = let n = length $ intersect ws hs in
    p : copies (inc i n cs)

score :: Card -> Int
score (_, ws, hs) = case length $ intersect ws hs of
    0 -> 0
    n -> 2^(n-1)

type Card = (Int, [Int], [Int])

parseInput :: Parser [Card]
parseInput = sepBy1 parseCard newline

parseCard :: Parser Card
parseCard = do
    _ <- string "Card" >> optional spaces
    n <- many1 digit
    _ <- string ": " >> optional spaces
    -- winners <- manyTill (many1 digit) (try $ string " | ")
    winners <- sepEndBy1 (many1 digit) (many1 $ char ' ')
    _ <- string "|" >> optional spaces
    haves <- sepBy (many1 digit) (many1 $ char ' ')
    return (read n, map read winners, map read haves)
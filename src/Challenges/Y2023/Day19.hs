{-# LANGUAGE TupleSections #-}
module Challenges.Y2023.Day19 (solutionA, solutionB) where

import Common.Prelude (solve)
import Common.Search (dfs)
import Data.Bifunctor (first)
import Data.Map (Map, fromList)
import Data.Maybe (maybeToList)
import Common.Parsing (int)
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

solutionA :: String -> String
solutionA = solve parser (sum . map (addUp . fst) . filterFinal . uncurry processAll . first addEnds)
solutionB :: String -> String
solutionB = solve parser (const "")

addUp :: Item -> Int
addUp (Item xval mval aval sval) = xval + mval + aval + sval

processAll :: Map String Workflow -> [Item] -> [(Item, Workflow)]
processAll ws = concatMap (\i -> dfs (process ws i) (start ws i))

addEnds :: Map String Workflow -> Map String Workflow
addEnds = Map.insert "A" ("A", []) . Map.insert "R" ("R", [])

type Workflow = (String, [Rule])

filterFinal :: [(Item, Workflow)] -> [(Item, Workflow)]
filterFinal = filter isAorR

isAorR :: (Item, Workflow) -> Bool
isAorR w = lbl == "A" where lbl = fst $ snd w

start :: Map String Workflow -> Item -> [(Item, Workflow)]
start ws i = map (i,) $ maybeToList $ Map.lookup "in" ws

process :: Map String Workflow -> Item -> (Item, Workflow) -> [(Item, Workflow)]
process _ _ (_,(_,[])) = []
process ws i (_,(l,r:rs)) = if mkPred r i
    then map (i,) $ maybeToList $ Map.lookup (destination r) ws
    else process ws i (i,(l,rs))

mkPred :: Rule -> Item -> Bool
mkPred (Rule 'o' _ _ _) _ = True
mkPred (Rule attr comp v _) i =
    getComparison comp (getAttribute attr i) v

getComparison :: Char -> Int -> Int -> Bool
getComparison '>' = (>)
getComparison '<' = (<)

getAttribute :: Char -> Item -> Int
getAttribute 'x' = x
getAttribute 'm' = m
getAttribute 'a' = a
getAttribute 's' = s

data Rule = Rule {
    attribute :: Char,
    comparison :: Char,
    val :: Int,
    destination :: String
}  deriving (Eq, Ord, Show)

data Item = Item {
    x :: Int,
    m :: Int,
    a :: Int,
    s :: Int
} deriving (Show, Eq, Ord)

parser :: Parser (Map String Workflow,[Item])
parser = do
    ws <- workflow `sepEndBy1` newline
    _ <- newline
    is <- item `sepEndBy1` newline
    return (fromList ws, is)

workflow :: Parser (String, Workflow)
workflow = do
    lbl <- many1 letter
    _ <- char '{'
    rs <- rule `sepBy1` char ','
    _ <- char '}'
    return (lbl, (lbl, rs))

rule :: Parser Rule
rule = try (do
    attr <- char 'x' <|> char 'm' <|> char 'a' <|> char 's'
    comp <- char '<' <|> char '>'
    value <- int
    _ <- char ':'
    lbl <- many1 alphaNum
    return $ Rule attr comp value lbl)
    <|> (do lbl <- many1 letter ; return (Rule 'o' '=' 0 lbl))

item :: Parser Item
item = do
    _ <- string "{x="
    xval <- int
    _ <- string ",m="
    mval <- int
    _ <- string ",a="
    aval <- int
    _ <- string ",s="
    sval <- int
    _ <- char '}'
    return $ Item xval mval aval sval



{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Challenges.Y2016.Day10 (solutionA, solutionB) where
import Common.Prelude
import Common.Parsing (int)
import Text.ParserCombinators.Parsec
import Data.Map (Map)
import qualified Data.Map as Map

solutionA :: String -> String
solutionA = solve parser (fst . findBotForNumbers 61 17 . foldl mkBotSet (Map.empty, Map.empty))
solutionB :: String -> String
solutionB = solve parser (product . concatMap (getNumbers . snd ) . take 3 . Map.toAscList . snd . foldl mkBotSet (Map.empty, Map.empty))

data Output = Bot {
    low :: Maybe Id,
    high :: Maybe Id,
    numbers :: [Int]
} | Bin [Int] deriving (Show, Eq, Ord)

type BotSet = (Map Int Output, Map Int Output)

findBotForNumbers :: Int -> Int -> BotSet -> (Int, Output)
findBotForNumbers i j = head . Map.toList . Map.filter (\o -> case o of
    Bin _ -> False
    Bot _ _ ns -> i `elem` ns && j `elem` ns) . fst

getNumbers :: Output -> [Int]
getNumbers (Bot _ _ n) = n
getNumbers (Bin n) = n

mkBotSet :: BotSet -> Instruction -> BotSet
mkBotSet (m, os) (GoesTo n b) = updateMap b $ case Map.lookup b m of
    Nothing -> (Map.insert b (Bot Nothing Nothing [n]) m, os)
    Just b' -> (Map.insert b (b' { numbers = n : numbers b'} ) m, os)
mkBotSet (m,os) (LowHigh b l h) = updateMap b $ case Map.lookup b m of
    Nothing -> (Map.insert b (Bot (Just l) (Just h) []) m, os)
    Just b' -> (Map.insert b (Bot (Just l) (Just h) (numbers b')) m, os)

updateMap :: Int -> BotSet -> BotSet
updateMap b (m, os) = case Map.lookup b m of
    Nothing -> (m, os)
    Just b' -> if length (numbers b') == 2
        then assignNumber (low b') (minimum $ numbers b')
           $ assignNumber (high b') (maximum $ numbers b') (m, os)
        else (m, os)

assignNumber :: Maybe Id -> Int -> BotSet -> BotSet
assignNumber Nothing _ m = m
assignNumber (Just (OutputId b)) n (m,os) = case Map.lookup b os of
    Nothing -> (m, Map.insert b (Bin [n]) os)
    Just (Bin ns) -> (m, Map.insert b (Bin (n:ns)) os)
    Just (Bot {}) -> error "list of bins contains a bot"
assignNumber (Just (BotId b)) n (m,os) = case Map.lookup b m of
    Nothing -> (Map.insert b (Bot Nothing Nothing [n]) m, os)
    Just b' -> if n `elem` numbers b'
        then updateMap b (m,os)
        else updateMap b (Map.insert b (b'{ numbers = n : numbers b'}) m, os)

data Id = BotId Int | OutputId Int deriving (Show, Eq, Ord)

data Instruction = GoesTo Int Int | LowHigh Int Id Id
    deriving (Show, Eq, Ord)

parser :: Parser [Instruction]
parser = (lowhigh <|> goesto) `sepEndBy` newline where
    goesto = do
        _ <- string "value "
        n <- int
        _ <- string " goes to bot "
        GoesTo n <$> int
    lowhigh = do
        _ <- string "bot "
        b <- int
        _ <- string " gives low to "
        l <- output <|> bot
        _ <- string " and high to "
        h <- output <|> bot
        return $ LowHigh b l h
    output = do
        _ <- string "output "
        OutputId <$> int
    bot = do
        _ <- string "bot "
        BotId <$> int

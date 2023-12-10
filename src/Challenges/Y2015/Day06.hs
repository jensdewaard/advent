{-# LANGUAGE TupleSections #-}
module Challenges.Y2015.Day06 (parseInput, solutionA, solutionB) where
import Data.Map (Map)
import qualified Data.Map as Map
import Common.Coord (Coord)
import Shared (solve)
import Text.ParserCombinators.Parsec

-- 14989458 too low
-- 15343601
-- 21186044 too high

type MappedToInt a = Map a Int

solutionA :: String -> String
solutionA = solve parseInput (show . sum . foldSequence . map sndExpand)
solutionB :: String -> String
solutionB = solve parseInput (show . sum . foldSequence' . map sndExpand)

foldSequence :: [(LightAction, MappedToInt Coord)] -> MappedToInt Coord
foldSequence = foldl doAction Map.empty

foldSequence' :: [(LightAction, MappedToInt Coord)] -> MappedToInt Coord
foldSequence' = foldl doAction' Map.empty

sndExpand :: (LightAction, Coord, Coord) -> (LightAction, MappedToInt Coord)
sndExpand (a, c1, c2) = (a, Map.fromList $ map pairOne (rectFromTo c1 c2)) where
    pairOne x = (x, 1)

rectFromTo :: Coord -> Coord -> [Coord]
rectFromTo (x1,y1) (x2, y2) = concatMap (\x -> map (x,) (enumFromTo y1 y2)) (enumFromTo x1 x2)

data LightAction = Toggle | On | Off deriving Eq

doAction :: MappedToInt Coord -> (LightAction, MappedToInt Coord) -> MappedToInt Coord
doAction cs (Toggle, cs') = Map.unionWith (\ a -> const $ 1 - a) cs cs'
doAction cs (On, cs') = Map.unionWith (const $ const 1) cs cs'
doAction cs (Off, cs') = Map.unionWith (const $ const 0) cs cs'

doAction' :: MappedToInt Coord -> (LightAction, MappedToInt Coord) -> MappedToInt Coord
doAction' cs (Toggle, cs') = Map.unionWith (\ a -> const $ a + 2) cs cs'
doAction' cs (On, cs') = Map.unionWith (\ a -> const $ a + 1) cs cs'
doAction' cs (Off, cs') = Map.unionWith (\ a -> const $ max (a - 1) 0) cs cs'

-- Parsers
parseInput :: Parser [(LightAction, Coord, Coord)]
parseInput = endBy1 parseLine newline

parseToggle :: Parser LightAction
parseToggle = string "toggle " >> return Toggle

parseOn :: Parser LightAction
parseOn = string "turn on " >> return On

parseOff :: Parser LightAction
parseOff = string "turn off " >> return Off

parseLine :: Parser (LightAction, Coord, Coord)
parseLine = do
    a <- try parseToggle <|> try parseOn <|> try parseOff
    x1 <- many1 digit
    _ <- char ','
    y1 <- many1 digit
    _ <- string " through "
    x2 <- many1 digit
    _ <- char ','
    y2 <- many1 digit
    return (a, (read x1, read y1), (read x2, read y2))


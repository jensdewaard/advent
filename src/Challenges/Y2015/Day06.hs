module Challenges.Y2015.Day06 (parseInput, solutionA, solutionB) where
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as Map
import Shared (Coord, solve, rectFromTo)
import Text.ParserCombinators.Parsec

-- 14989458 too low
-- 15343601
-- 21186044 too high

type MappedToInt a = Map a Int

solutionA :: String -> String
solutionA input = solve input parseInput (show . sum . foldSequence . (map sndExpand))
solutionB :: String -> String
solutionB input = solve input parseInput (show . sum . foldSequence' . map sndExpand)

foldSequence :: [(LightAction, MappedToInt Coord)] -> MappedToInt Coord
foldSequence as = foldl doAction Map.empty as

foldSequence' :: [(LightAction, MappedToInt Coord)] -> MappedToInt Coord
foldSequence' as = foldl doAction' Map.empty as

sndExpand :: (LightAction, Coord, Coord) -> (LightAction, MappedToInt Coord)
sndExpand (a, c1, c2) = (a, Map.fromList $ map pairOne (rectFromTo c1 c2)) where
    pairOne x = (x, 1)

data LightAction = Toggle | On | Off deriving Eq

doAction :: MappedToInt Coord -> (LightAction, MappedToInt Coord) -> MappedToInt Coord
doAction cs (Toggle, cs') = Map.unionWith (\a -> \b -> 1 - a) cs cs'
doAction cs (On, cs') = Map.unionWith (\a -> \b -> 1) cs cs'
doAction cs (Off, cs') = Map.unionWith (\a -> \b -> 0) cs cs'

doAction' :: MappedToInt Coord -> (LightAction, MappedToInt Coord) -> MappedToInt Coord
doAction' cs (Toggle, cs') = Map.unionWith (\a -> \b -> a + 2) cs cs'
doAction' cs (On, cs') = Map.unionWith (\a -> \b -> a + 1) cs cs'
doAction' cs (Off, cs') = Map.unionWith (\a -> \b -> max (a - 1) 0) cs cs'

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
    a <- (try parseToggle <|> try parseOn <|> try parseOff)
    x1 <- many1 digit
    _ <- char ','
    y1 <- many1 digit
    _ <- string " through "
    x2 <- many1 digit
    _ <- char ','
    y2 <- many1 digit
    return (a, (read x1, read y1), (read x2, read y2))


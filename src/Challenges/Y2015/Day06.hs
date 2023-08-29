module Challenges.Y2015.Day06 (parseInput, solutionA, solutionB) where
import qualified Data.Set as S
import Shared (Coord, solve, rectFromTo)
import Text.ParserCombinators.Parsec

-- 598216 too high

solutionA :: String -> String
solutionA input = solve input parseInput (show . length . foldSequence . runSequence)
solutionB :: String -> String
solutionB _ = "0"

foldSequence :: [(LightAction, S.Set Coord)] -> S.Set Coord
foldSequence as = foldl doAction S.empty as

runSequence :: [(LightAction, Coord, Coord)] -> [(LightAction, S.Set Coord)]
runSequence as = map sndExpand as

sndExpand :: (LightAction, Coord, Coord) -> (LightAction, S.Set Coord)
sndExpand (a, c1, c2) = (a, S.fromList $ rectFromTo c1 c2)

data LightAction = Toggle | On | Off

doAction :: S.Set Coord -> (LightAction, S.Set Coord) -> S.Set Coord
doAction cs (Toggle, cs') = foldl toggle cs cs' 
doAction cs (On, cs') = foldl on cs cs' 
doAction cs (Off, cs') = foldl off cs cs' 

toggle :: S.Set Coord -> Coord -> S.Set Coord
toggle cs c = if S.member c cs then off cs c else on cs c
on :: S.Set Coord -> Coord -> S.Set Coord
on cs c = S.insert c cs
off :: S.Set Coord -> Coord -> S.Set Coord
off cs c = S.delete c cs

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


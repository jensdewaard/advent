module Challenges.Y2016.Day08 (solutionA, solutionB) where
import Common.Prelude
import Common.Parsing (int)
import Common.List (rotateR, count, sumWith)
import Data.List (transpose)
import Text.ParserCombinators.Parsec hiding (count)

solutionA :: String -> String
solutionA = solve parser (sumWith (count (=='#')) . foldl go defaultScreen)
solutionB :: String -> String
solutionB = solve parser (foldl go defaultScreen)

defaultScreen :: [String]
defaultScreen = replicate 6 (replicate 50 '.')

go :: [String] -> Instruction -> [String]
go screen (TurnOn width height) =
    map (\l -> replicate width '#' ++ drop width l) (take height screen)
    ++ drop height screen
go screen (RotateRow row places) = let
    (a,l:b) = splitAt row screen
    in
    a ++ [rotateR places l] ++ b

go screen (RotateCol col places) = let
    (a,l:b) = splitAt col (transpose screen)
    in
    transpose $ a ++ [rotateR places l] ++ b

data Instruction = TurnOn Int Int
    | RotateRow Int Int
    | RotateCol Int Int

parser :: Parser [Instruction]
parser = instruction `sepEndBy` newline where
    instruction = do
        try (do
            _ <- string "rect "
            w <- int
            _ <- char 'x'
            TurnOn w <$> int
            )
        <|> try (do
            _ <- string "rotate row y="
            r <- int
            _ <- string " by "
            RotateRow r <$> int
        )
        <|> try (do
            _ <- string "rotate column x="
            c <- int
            _ <- string " by "
            RotateCol c <$> int
            )
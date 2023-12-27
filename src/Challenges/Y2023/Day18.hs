module Challenges.Y2023.Day18 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.Prelude
import Common.Coord (Dir(..), moveN, origin)
import qualified Common.Coord as Coord
import Parsing (dir, int)
import Numeric (readHex)

solutionA :: String -> String
solutionA = solve parser area
solutionB :: String -> String
solutionB = solve parser' area

type Dig = (Dir, Int, String)

area :: [Dig] -> Int
area input = abs (Coord.area path) + perimeter `quot` 2 + 1
  where
    path      = scanl (\c (d,n,_) -> moveN n d c) origin input
    perimeter = sum [ n | (_,n,_) <- input]

parser :: Parser [Dig]
parser = do dig `sepEndBy1` newline where
    dig = do
        d <- dir
        _ <- space
        n <- int
        _ <- string " (#"
        color <- many1 alphaNum
        _ <- char ')'
        return (d,n,"#"++color)

parser' :: Parser [Dig]
parser' = do dig `sepEndBy1` newline where
    dig = do
        _ <- dir
        _ <- space
        _ <- int
        _ <- string " (#"
        color <- readHex <$> count 5 alphaNum
        d <- (char '0' >> return R) <|> (char '1' >> return D) <|> (char '2' >> return L) <|> (char '3' >> return U)
        _ <- char ')'
        return (d,fst $ head color,"")
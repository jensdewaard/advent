module Challenges.Y2015.Day02 (input, solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (number, fromRight)
import Data.List (sort)

input :: Bool -> IO String
input False = readFile "data/2015/02.txt"
input True = return "3x11x24\n\
\13x5x19\n\
\1x9x27"
solutionA :: String -> String
solutionA = show . sum . map requiredPaper . Shared.fromRight . parseInput
solutionB :: String -> String
solutionB = show . sum . map requiredRibbon . Shared.fromRight . parseInput

data Present = Present
    { w :: Int
    , h :: Int
    , l :: Int
    } deriving Show

requiredPaper :: Present -> Int
requiredPaper p = 2*(w p * h p 
    + w p * l p 
    + l p * h p) + minimum [w p * h p, w p * l p, l p * h p]

requiredRibbon :: Present -> Int
requiredRibbon p = (w p * h p * l p) + 
    2 * (sum $ take 2 $ sort [w p, l p , h p])

parseInput :: String -> Either ParseError [Present]
parseInput = parse parser "could not parse file"

parser :: GenParser Char st [Present]
parser = sepBy present newline

present :: GenParser Char st Present
present = do
    l <- number
    _ <- string "x"
    w <- number
    _ <- string "x"
    h <- number
    return $ Present w h l
module Challenges.Y2016.Day04 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Common.Char (shift)
import Common.List (occur, sumWith)
import Data.Char (isDigit)
import Data.Ord (comparing)
import Data.List (sortBy, isInfixOf)

solutionA :: String -> String
solutionA = solve parser (sumWith sector . filter isReal)
solutionB :: String -> String
solutionB = solve parser (head . map sector . filter northPoleRoom . map decrypt)

data Room = Room {
    name :: String,
    sector :: Int,
    checksum :: String
} deriving (Eq, Show, Ord)

northPoleRoom :: Room -> Bool
northPoleRoom (Room n _ _) = "pole" `isInfixOf` n

decrypt :: Room -> Room
decrypt (Room n s c) = Room (map (shift' s) n) s c

shift' :: Int -> Char -> Char
shift' _ '-' = ' '
shift' n c = shift n c

isReal :: Room -> Bool
isReal r = computeChecksum r == checksum r

computeChecksum :: Room -> String
computeChecksum (Room n _ _) = let os = sortBy (flip (comparing snd) <> comparing fst) (occur $ filter ('-' /=) n) in
    map fst $ take 5 os

parser :: Parser [Room]
parser = room `sepEndBy` newline where
    room = do
        n <- many (alphaNum <|> char '-')
        let stor = reverse $ takeWhile isDigit $ reverse n
        let nam = reverse $ drop (length stor + 1) $ reverse n
        _ <- char '['
        csum <- many letter
        _ <- char ']'
        return $ Room nam (read stor) csum
module Challenges.Y2016.Day09 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Data.List (singleton)
import Common.Parsing (int)
import Common.List (splitOn)

solutionA :: String -> String
solutionA = solve parser length
solutionB :: String -> String
solutionB = solve parser decompressedLength

decompressedLength :: [Char] -> Int
decompressedLength ('(':xs) =
  let marker = takeWhile (/=')') xs
      ys = drop (length marker + 1) xs
      [n, m] = map read $ splitOn "x" marker
      rep = take n ys
  in (m * decompressedLength rep) + decompressedLength (drop n ys)
decompressedLength (_:xs) = 1 + decompressedLength xs
decompressedLength [] = 0

parser :: Parser [Char]
parser = concat <$> many (marker <|> (singleton <$> letter)) where
    marker :: Parser [Char]
    marker = do
        (l,n) <- between (char '(') (char ')') (do
            a <- int
            _ <- char 'x'
            b <- int
            return (a,b)
            )
        cs <- count l anyToken
        return $ concat $ replicate n cs

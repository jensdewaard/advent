module Parsing where
import Text.ParserCombinators.Parsec
import Common.Coord (Coord)

int :: Parser Int
int = do
  sign <- option 1 (do { _ <- char '-'; return (-1) })
  num <- read <$> many1 digit
  return (sign * num)

coords :: Parser Coord
coords = do
    l <- int
    _ <- string ","
    r <- int
    return (l, r)
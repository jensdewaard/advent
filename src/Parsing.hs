module Parsing where
import Text.ParserCombinators.Parsec

int :: Parser Int
int = do
  sign <- option 1 (do { _ <- char '-'; return (-1) })
  num <- read <$> many1 digit
  return (sign * num)
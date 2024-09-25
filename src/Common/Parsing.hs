module Common.Parsing (int, coords, symbol, grid, dir) where

import Common.Coord (Coord, Dir (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

int :: Parser Int
int = do
  sign <- option 1 (do _ <- char '-'; return (-1))
  num <- read <$> many1 digit
  return (sign * num)

coords :: Parser Coord
coords = do
  l <- int
  _ <- string ","
  r <- int
  return (l, r)

symbol :: Parser Char
symbol = char '-' <|> char '+' <|> char '=' <|> char '*' <|> char '#' <|> char '.'

grid :: Parser a -> Parser (Map Coord a)
grid f = do
  ps <- concat <$> many1 position `sepEndBy1` newline
  return $ Map.fromList ps
  where
    position = do
      pos <- getPosition
      let p = (sourceColumn pos, sourceLine pos)
      c <- f
      return (p, c)

dir :: Parser Dir
dir =
  do (char 'U' >> return U) <|> (char 'L' >> return L)
    <|> (char 'R' >> return R)
    <|> (char 'D' >> return D)

module Challenges.Y2015.Day08 (solutionA, solutionB) where

import Text.ParserCombinators.Parsec
import Common.Prelude hiding (length)

solutionA :: String -> String
solutionA = solve parser (sum . map (uncurry (-) . (\s -> (length s, length $ unescape $ unquote s))))

solutionB :: String -> String
solutionB = solve parser (sum .map ((\(x,y) -> y - x) . (\s -> (length s, (+2) . length $ escape s))))

parser :: Parser [String]
parser = endBy1 (many1 (noneOf "\n")) newline

unescape :: String -> String
unescape "" = ""
unescape [x] = [x]
unescape ('\\':'\\':ss) = "\\" ++ unescape ss
unescape ('\\':'"':ss) = "\"" ++ unescape ss
unescape ('\\':'x':_:_:ss) = "x" ++ unescape ss
unescape (c : cs) = c : unescape cs

escape :: String -> String
escape "" = ""
escape ('\"':ss) = "\\\"" ++ escape ss
escape ('\\':ss) = "\\\\" ++ escape ss
escape (s:ss) = s : escape ss

unquote :: String -> String
unquote [] = []
unquote "\"" = "\""
unquote s = if head s == '"' && last s == '"' then tail $ init s else s

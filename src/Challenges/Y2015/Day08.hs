module Challenges.Y2015.Day08 (solutionA, solutionB) where

import Text.ParserCombinators.Parsec
import Shared (solve)

solutionA :: String -> String
solutionA input = solve input parser (show . sum . map (\(x,y) -> x - y) . map (\s -> (length s, length $ unescape $ unquote s)))

solutionB :: String -> String
solutionB input = solve input parser (show . sum .map (\(x,y) -> y - x) . map (\s -> (length s, (+2) . length $ escape s)))

parser :: Parser [String]
parser = endBy1 (many1 (noneOf "\n")) newline

unescape :: String -> String
unescape "" = ""
unescape [x] = [x]
unescape ('\\':'\\':ss) = "\\" ++ unescape ss
unescape ('\\':'"':ss) = "\"" ++ unescape ss
unescape ('\\':'x':x:y:ss) = "x" ++ unescape ss
unescape (c : cs) = c : unescape cs

escape :: String -> String
escape "" = ""
escape ('\"':ss) = "\\\"" ++ escape ss
escape ('\\':ss) = "\\\\" ++ escape ss
escape (s:ss) = [s] ++ escape ss

unquote :: String -> String
unquote [] = []
unquote "\"" = "\""
unquote s = if head s == '"' && last s == '"' then tail $ init s else s

module Challenges.Y2022.Day01 where

import Data.List
import Data.Maybe
import Parsing (int)
import Text.ParserCombinators.Parsec

partA :: [Elf] -> Int
partA elves = maximum $ sortBy (flip compare) (map sum elves)

partB :: [Elf] -> Int
partB elves = sum . take 3 $ sortBy (flip compare) (map sum elves)

parseFile :: String -> Either ParseError [Elf]
parseFile = parse file "could not parse file" where
    file = elf `endBy1` newline
    elf = int `endBy1` newline

type Elf = [Int]

maybeFromEither :: Either a b -> Maybe b
maybeFromEither (Right x) = Just x
maybeFromEither (Left _) = Nothing

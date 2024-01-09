module Challenges.Y2015.Day04 (solutionA, solutionB, hash, fiveZeroes) where

import Data.List (findIndex)
import Data.Maybe (fromJust)
import Common.Prelude
import Text.ParserCombinators.Parsec
import Data.Digest.Pure.MD5 (md5)
import Data.String (fromString)

solutionA :: String -> String
-- solutionA _ = hash 609043
solutionA = solve parser (\i -> show $ (+1) $ fromJust . findIndex fiveZeroes $ map (hash i) [1..])

solutionB :: String -> String
solutionB = solve parser (\i -> show $ (+1) $ fromJust . findIndex sixZeroes $ map (hash i) [1..])

hash :: String -> Int -> String
hash s i = show $ md5 $ fromString $ s ++ show i

fiveZeroes :: String -> Bool
fiveZeroes ('0':'0':'0':'0':'0':_) = True
fiveZeroes _ = False

sixZeroes :: String -> Bool
sixZeroes ('0':'0':'0':'0':'0':'0':_) = True
sixZeroes _ = False

parser :: Parser String
parser = many1 letter
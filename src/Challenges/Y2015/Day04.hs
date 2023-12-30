module Challenges.Y2015.Day04 (solutionA, solutionB, hash, fiveZeroes) where

import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Digest.Pure.MD5 (md5)
import qualified Data.String as LB

solutionA :: String -> String
-- solutionA _ = hash 609043
solutionA i = show $ (+1) $ fromJust . findIndex fiveZeroes $ map (hash i) [1..]

solutionB :: String -> String
solutionB i = show $ (+1) $ fromJust . findIndex sixZeroes $ map (hash i) [1..]

hash :: String -> Int -> String
hash s i = show $ md5 $ LB.fromString $ s ++ show i

fiveZeroes :: String -> Bool
fiveZeroes ('0':'0':'0':'0':'0':_) = True
fiveZeroes _ = False

sixZeroes :: String -> Bool
sixZeroes ('0':'0':'0':'0':'0':'0':_) = True
sixZeroes _ = False
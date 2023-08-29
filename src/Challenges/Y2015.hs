{-# LANGUAGE RankNTypes #-}
module Challenges.Y2015 (getDay) where

import qualified Challenges.Y2015.Day01 as Day01
import qualified Challenges.Y2015.Day02 as Day02
import qualified Challenges.Y2015.Day03 as Day03
import qualified Challenges.Y2015.Day04 as Day04
import qualified Challenges.Y2015.Day05 as Day05
import qualified Challenges.Y2015.Day06 as Day06

getDay :: Int -> (String -> String, String -> String)
--getDay test 1 = (Day01.input test, Day01.solutionA , Day01.solutionB)
--getDay test 2 = (Day02.input test, Day02.solutionA , Day02.solutionB)
--getDay test 3 = (Day03.input test, Day03.solutionA , Day03.solutionB)
--getDay test 4 = (Day04.input test, Day04.solutionA , Day04.solutionB)
--getDay test 5 = (Day05.input test, Day05.solutionA , Day05.solutionB)
getDay 6 = (Day06.solutionA , Day06.solutionB)
getDay _ = error "invalid day"

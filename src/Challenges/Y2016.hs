{-# LANGUAGE RankNTypes #-}
module Challenges.Y2016 (getDay) where

import qualified Challenges.Y2016.Day01 as Day01
import qualified Challenges.Y2016.Day02 as Day02
import qualified Challenges.Y2016.Day03 as Day03

getDay :: Integer -> (String -> String, String -> String)
getDay 1 = (Day01.solutionA , Day01.solutionB)
getDay 2 = (Day02.solutionA , Day02.solutionB)
getDay 3 = (Day03.solutionA , Day03.solutionB)
getDay _ = error "invalid day"

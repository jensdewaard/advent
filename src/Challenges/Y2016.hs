{-# LANGUAGE RankNTypes #-}
module Challenges.Y2016 (getDay) where

import qualified Challenges.Y2016.Day01 as Day01

getDay :: Integer -> (String -> String, String -> String)
getDay 1 = (Day01.solutionA , Day01.solutionB)
getDay _ = error "invalid day"

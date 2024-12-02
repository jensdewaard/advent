module Challenges.Y2024 (getDay) where

import qualified Challenges.Y2024.Day01 as Day01
import qualified Challenges.Y2024.Day02 as Day02

getDay :: Integer -> (String-> String, String -> String)
getDay 1 = (Day01.solutionA, Day01.solutionB)
getDay 2 = (Day02.solutionA, Day02.solutionB)
getDay _ = error "unsupported day"

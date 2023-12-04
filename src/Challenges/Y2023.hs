module Challenges.Y2023 (getDay) where

import qualified Challenges.Y2023.Day01 as Day01
import qualified Challenges.Y2023.Day02 as Day02

getDay :: Int -> (String-> String, String -> String)
getDay 1 = (Day01.solutionA, Day01.solutionB)
getDay 2 = (Day02.solutionA, Day02.solutionB)
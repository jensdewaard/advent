module Challenges.Y2023 (getDay) where

import qualified Challenges.Y2023.Day01 as Day01
import qualified Challenges.Y2023.Day02 as Day02
import qualified Challenges.Y2023.Day03 as Day03
import qualified Challenges.Y2023.Day04 as Day04

getDay :: Int -> (String-> String, String -> String)
getDay 1 = (Day01.solutionA, Day01.solutionB)
getDay 2 = (Day02.solutionA, Day02.solutionB)
getDay 3 = (Day03.solutionA, Day03.solutionB)
getDay 4 = (Day04.solutionA, Day04.solutionB)
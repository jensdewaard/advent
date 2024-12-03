module Challenges.Y2022 (getDay) where

import qualified Challenges.Y2022.Day01 as Day01
import qualified Challenges.Y2022.Day02 as Day02
import qualified Challenges.Y2022.Day03 as Day03
import qualified Challenges.Y2022.Day04 as Day04
import qualified Challenges.Y2022.Day10 as Day10
import qualified Challenges.Y2022.Day12 as Day12
import qualified Challenges.Y2022.Day15 as Day15

getDay :: Integer -> (String -> String, String -> String)
getDay 1 = (Day01.solutionA, Day01.solutionB)
getDay 2 = (Day02.solutionA, Day02.solutionB)
getDay 3 = (Day03.solutionA, Day03.solutionB)
getDay 4 = (Day04.solutionA, Day04.solutionB)
getDay 10 = (Day10.solutionA, Day10.solutionB)
getDay 12 = (Day12.solutionA, Day12.solutionB)
getDay 15 = (Day15.solutionA, Day15.solutionB)
getDay _ = error "invalid day"

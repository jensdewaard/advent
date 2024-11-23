module Challenges.Y2019 (getDay) where

import qualified Challenges.Y2019.Day01 as Day01
import qualified Challenges.Y2019.Day02 as Day02
import qualified Challenges.Y2019.Day03 as Day03
import qualified Challenges.Y2019.Day04 as Day04
import qualified Challenges.Y2019.Day05 as Day05
import qualified Challenges.Y2019.Day07 as Day07

getDay :: Integer -> (String -> String, String -> String)
getDay 1 = (Day01.solutionA, Day01.solutionB)
getDay 2 = (Day02.solutionA, Day02.solutionB)
getDay 3 = (Day03.solutionA, Day03.solutionB)
getDay 4 = (Day04.solutionA, Day04.solutionB)
getDay 5 = (Day05.solutionA, Day05.solutionB)
getDay 7 = (Day07.solutionA, Day07.solutionB)
getDay _ = error "unsupported day"
module Challenges.Y2019 where

import qualified Challenges.Y2019.Day01 as Y19D01
import qualified Challenges.Y2019.Day02 as Day02
import qualified Challenges.Y2019.Day03 as Day03
import qualified Challenges.Y2019.Day04 as Y19D04

getDay :: Integer -> (String -> String, String -> String)
-- getDay 1 = (Y19D01.input test, Y19D01.solutionA, Y19D01.solutionB)
getDay 2 = (Day02.solutionA, Day02.solutionB)
getDay 3 = (Day03.solutionA, Day03.solutionB)
-- getDay 4 = (Y19D04.input test, Y19D04.solutionA, Y19D04.solutionB)
getDay _ = error "unsupported day"
module Challenges.Y2022 (getDay) where

import qualified Challenges.Y2022.Day10 as Day10
import qualified Challenges.Y2022.Day12 as Day12
import qualified Challenges.Y2022.Day15 as Day15

getDay :: Integer -> (String -> String, String -> String)
getDay 10 = (Day10.solutionA, Day10.solutionB)
getDay 12 = (Day12.solutionA, Day12.solutionB)
getDay 15 = (Day15.solutionA, Day15.solutionB)
getDay _ = error "invalid day"

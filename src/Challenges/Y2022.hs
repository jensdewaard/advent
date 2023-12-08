module Challenges.Y2022 (getDay) where

import qualified Challenges.Y2022.Day15 as Day15

getDay :: Integer -> (String -> String, String -> String)
getDay 15 = (Day15.solutionA, Day15.solutionB)
getDay _ = error "invalid day"

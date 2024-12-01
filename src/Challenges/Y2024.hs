module Challenges.Y2024 (getDay) where

import qualified Challenges.Y2024.Day01 as Day01

getDay :: Integer -> (String-> String, String -> String)
getDay 1 = (Day01.solutionA, Day01.solutionB)
getDay _ = error "unsupported day"

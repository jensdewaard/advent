module Challenges.Y2023 (getDay) where

import qualified Challenges.Y2023.Day01 as Day01
import qualified Challenges.Y2023.Day02 as Day02
import qualified Challenges.Y2023.Day03 as Day03
import qualified Challenges.Y2023.Day04 as Day04
import qualified Challenges.Y2023.Day05 as Day05
import qualified Challenges.Y2023.Day06 as Day06
import qualified Challenges.Y2023.Day07 as Day07
import qualified Challenges.Y2023.Day08 as Day08
import qualified Challenges.Y2023.Day09 as Day09
import qualified Challenges.Y2023.Day10 as Day10
import qualified Challenges.Y2023.Day11 as Day11
import qualified Challenges.Y2023.Day12 as Day12
import qualified Challenges.Y2023.Day13 as Day13

getDay :: Integer -> (String-> String, String -> String)
getDay 1 = (Day01.solutionA, Day01.solutionB)
getDay 2 = (Day02.solutionA, Day02.solutionB)
getDay 3 = (Day03.solutionA, Day03.solutionB)
getDay 4 = (Day04.solutionA, Day04.solutionB)
getDay 5 = (Day05.solutionA, Day05.solutionB)
getDay 6 = (Day06.solutionA, Day06.solutionB)
getDay 7 = (Day07.solutionA, Day07.solutionB)
getDay 8 = (Day08.solutionA, Day08.solutionB)
getDay 9 = (Day09.solutionA, Day09.solutionB)
getDay 10 = (Day10.solutionA, Day10.solutionB)
getDay 11 = (Day11.solutionA, Day11.solutionB)
getDay 12 = (Day12.solutionA, Day12.solutionB)
getDay 13 = (Day13.solutionA, Day13.solutionB)
getDay _ = error "unsupported day"
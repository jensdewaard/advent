module Challenges.Y2024 (getDay) where

import qualified Challenges.Y2024.Day01 as Day01
import qualified Challenges.Y2024.Day02 as Day02
import qualified Challenges.Y2024.Day03 as Day03
import qualified Challenges.Y2024.Day04 as Day04
import qualified Challenges.Y2024.Day05 as Day05
import qualified Challenges.Y2024.Day06 as Day06
import qualified Challenges.Y2024.Day07 as Day07
import qualified Challenges.Y2024.Day08 as Day08
import qualified Challenges.Y2024.Day09 as Day09
import qualified Challenges.Y2024.Day10 as Day10
import qualified Challenges.Y2024.Day11 as Day11
import qualified Challenges.Y2024.Day12 as Day12
import qualified Challenges.Y2024.Day13 as Day13
import qualified Challenges.Y2024.Day14 as Day14
import qualified Challenges.Y2024.Day15 as Day15
import qualified Challenges.Y2024.Day16 as Day16
import qualified Challenges.Y2024.Day17 as Day17
import qualified Challenges.Y2024.Day18 as Day18
import qualified Challenges.Y2024.Day19 as Day19
import qualified Challenges.Y2024.Day20 as Day20
import qualified Challenges.Y2024.Day21 as Day21
import qualified Challenges.Y2024.Day22 as Day22
import qualified Challenges.Y2024.Day23 as Day23
import qualified Challenges.Y2024.Day24 as Day24
import qualified Challenges.Y2024.Day25 as Day25

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
getDay 14 = (Day14.solutionA, Day14.solutionB)
getDay 15 = (Day15.solutionA, Day15.solutionB)
getDay 16 = (Day16.solutionA, Day16.solutionB)
getDay 17 = (Day17.solutionA, Day17.solutionB)
getDay 18 = (Day18.solutionA, Day18.solutionB)
getDay 19 = (Day19.solutionA, Day19.solutionB)
getDay 20 = (Day20.solutionA, Day20.solutionB)
getDay 21 = (Day21.solutionA, Day21.solutionB)
getDay 22 = (Day22.solutionA, Day22.solutionB)
getDay 23 = (Day23.solutionA, Day23.solutionB)
getDay 24 = (Day24.solutionA, Day24.solutionB)
getDay 25 = (Day25.solutionA, Day25.solutionB)
getDay _ = error "unsupported day"

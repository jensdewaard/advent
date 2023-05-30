module Challenges.Y2019 where

import qualified Challenges.Y2019.Day01 as Y19D01
import qualified Challenges.Y2019.Day02 as Y19D02
import qualified Challenges.Y2019.Day03 as Y19D03
import qualified Challenges.Y2019.Day04 as Y19D04

getDay :: Bool -> Int -> (IO String, String -> String, String -> String)
getDay test 1 = (Y19D01.input test, Y19D01.solutionA, Y19D01.solutionB)
getDay test 2 = (Y19D02.input test, Y19D02.solutionA, Y19D02.solutionB)
getDay test 3 = (Y19D03.input test, Y19D03.solutionA, Y19D03.solutionB)
getDay test 4 = (Y19D04.input test, Y19D04.solutionA, Y19D04.solutionB)

module Challenges.Y2023 (getDay) where

    import qualified Challenges.Y2023.Day01 as Day01

    getDay :: Int -> (String-> String, String -> String)
    getDay 1 = (Day01.solutionA, Day01.solutionB)
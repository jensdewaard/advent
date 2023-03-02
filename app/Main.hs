module Main (main) where

import qualified Challenges.Y2015.Day01 as Y15D01 (input, solutionA, solutionB)
import qualified Challenges.Y2015.Day02 as Y15D02 (input, solutionA, solutionB)
import qualified Challenges.Y2015.Day03 as Y15D03 (input, solutionA, solutionB)
import qualified Challenges.Y2015.Day04 as Y15D04 (input, solutionA, solutionB)
import qualified Challenges.Y2015.Day05 as Y15D05 (input, solutionA, solutionB)
import qualified Challenges.Y2022.Day14 as D14 (input, solutionA, solutionB) 
import Parser (Sample (Sample), sample)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure))

main :: IO ()
main = runProg =<< execParser opts
    where
        opts = info (sample <**> helper)
            ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative" )

runProg :: Sample -> IO ()
runProg (Sample test year day) = (getSol (test, year, day)) >>= runSol >>= putStr

type Sol = (IO String, String -> String, String -> String)

getSol :: (Bool, Int, Int) -> IO Sol
getSol (test, y, d) = return $ ((getYear y) test) d

runSol :: Sol -> IO String
runSol (inputSource, solveA, solveB) = do
    input <- inputSource
    return $ concat ["Solution A: ", solveA input, "\nSolution B: ", solveB input, "\n"] 

getYear :: Int -> Bool -> Int -> Sol
getYear 2015 = getDay2015
getYear 2022 = getDay2022
getYear _ = error "unsupported year"

getDay2022 :: Bool -> Int -> Sol
getDay2022 test 14 = (D14.input test, D14.solutionA, D14.solutionB)
getDay2022 _ _ = error "unsupported day"

getDay2015 :: Bool -> Int -> Sol
getDay2015 test 1 = (Y15D01.input test, Y15D01.solutionA, Y15D01.solutionB)
getDay2015 test 2 = (Y15D02.input test, Y15D02.solutionA, Y15D02.solutionB)
getDay2015 test 3 = (Y15D03.input test, Y15D03.solutionA, Y15D03.solutionB)
getDay2015 test 4 = (Y15D04.input test, Y15D04.solutionA, Y15D04.solutionB)
getDay2015 test 5 = (Y15D05.input test, Y15D05.solutionA, Y15D05.solutionB)
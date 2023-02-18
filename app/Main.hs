module Main (main) where

import qualified Challenges.Y2015.Day01 as Y15D01 (input, solutionA, solutionB)
import qualified Challenges.Y2015.Day02 as Y15D02 (input, solutionA, solutionB)
import qualified Challenges.Y2015.Day03 as Y15D03 (input, solutionA, solutionB)
import qualified Challenges.Y2015.Day04 as Y15D04 (input, solutionA, solutionB)
import qualified Challenges.Y2015.Day05 as Y15D05 (input, solutionA, solutionB)
import qualified Challenges.Y2022.Day14 as D14 (input, solutionA, solutionB) 
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure))

main :: IO ()
main = getArgs 
    >>= parse 
    >>= getSol
    >>= runSol
    >>= putStr 

parse :: [String] -> IO (Bool, String, String)
parse ["-h"] = usage >> exit
parse ["-t", y, d] = return (True, y, d)
parse ["--test", y, d] = return (True, y, d)
parse [y, d] = return (False, y, d)
parse _ = usage >> die

type Sol = (IO String, String -> String, String -> String)

getSol :: (Bool, String, String) -> IO Sol
getSol (test, y, d) = return $ ((getYear y) test) d

runSol :: Sol -> IO String
runSol (inputSource, solveA, solveB) = do
    input <- inputSource
    return $ concat ["Solution A: ", solveA input, "\nSolution B: ", solveB input, "\n"] 

getYear :: String -> Bool -> String -> Sol
getYear "2015" = getDay2015
getYear "2022" = getDay2022
getYear _ = error "unsupported year"

getDay2022 :: Bool -> String -> Sol
getDay2022 test "14" = (D14.input test, D14.solutionA, D14.solutionB)
getDay2022 _ _ = error "unsupported day"

getDay2015 :: Bool -> String -> Sol
getDay2015 test "1" = (Y15D01.input test, Y15D01.solutionA, Y15D01.solutionB)
getDay2015 test "2" = (Y15D02.input test, Y15D02.solutionA, Y15D02.solutionB)
getDay2015 test "3" = (Y15D03.input test, Y15D03.solutionA, Y15D03.solutionB)
getDay2015 test "4" = (Y15D04.input test, Y15D04.solutionA, Y15D04.solutionB)
getDay2015 test "5" = (Y15D05.input test, Y15D05.solutionA, Y15D05.solutionB)

usage :: IO ()
usage = putStrLn "Usage: stack run [-h] YEAR DAY"
exit :: IO a
exit = exitWith ExitSuccess
die :: IO a
die = exitWith (ExitFailure 1)
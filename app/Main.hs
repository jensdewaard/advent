module Main (main) where

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
    return $ concat ["Solution A: ", solveA input, "\nSolution B:", solveB input] 

getYear :: String -> Bool -> String -> Sol
getYear "2022" = getDay2022
getYear _ = error "unsupported year"

getDay2022 :: Bool -> String -> Sol
getDay2022 test "14" = (D14.input test, D14.solutionA, D14.solutionB)
getDay2022 _ _ = error "unsupported day"

usage :: IO ()
usage = putStrLn "Usage: stack run [-h] YEAR DAY"
exit :: IO a
exit = exitWith ExitSuccess
die :: IO a
die = exitWith (ExitFailure 1)
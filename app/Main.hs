module Main (main) where

import Parser (ProgramArgs (ProgramArgs), args)
import Options.Applicative
import System.IO
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure))
import qualified Challenges.Y2015 as Y2015
import qualified Challenges.Y2019 as Y2019
import qualified Challenges.Y2022 as Y2022

main :: IO ()
main = runProg =<< execParser opts
    where
        opts = info (args <**> helper)
            ( fullDesc
            <> progDesc "Run an advent of code solution"
            <> header "advent - running advent of code solutions" )

getSol :: (Int, Int) -> IO (String -> String, String -> String)
getSol (y, d) = return $ getYear y d

getYear :: Int -> Int -> (String -> String, String -> String)
getYear 2015 = Y2015.getDay
--getYear 2019 = Y2019.getDay
--getYear 2022 = Y2022.getDay
getYear _ = error "unsupported year"


runProg :: ProgramArgs -> IO ()
--runProg (Sample test year day) = getSol (test, year, day) >>= runSol >>= putStr
runProg (ProgramArgs test year day) = do
    (a, b) <- getSol (year, day)
    handle <- openFile (mkDataPath year day) ReadMode
    contents <- hGetContents handle
    let a' = a contents
    let b' = b contents
    let out = "A: " ++ a' ++ "\n" ++ "B: " ++ b' ++ "\n"
    putStr out

mkDataPath :: Int -> Int -> String
mkDataPath y d
    | d <= 9 = "data/" ++ show y ++ "/0" ++ show d ++ ".txt"
    | otherwise = "data/" ++ show y ++ "/" ++ show d ++ ".txt"

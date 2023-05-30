module Main (main) where

import Parser (Sample (Sample), sample)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure))
import Solution

main :: IO ()
main = runProg =<< execParser opts
    where
        opts = info (sample <**> helper)
            ( fullDesc
            <> progDesc "Run an advent of code solution"
            <> header "advent - running advent of code solutions" )

runProg :: Sample -> IO ()
runProg (Sample test year day) = getSol (test, year, day) >>= runSol >>= putStr

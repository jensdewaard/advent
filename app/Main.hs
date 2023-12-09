{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Options.Applicative
import Parser (ProgramArgs (ProgramArgs), args)

import Advent
import System.CPUTime
import System.IO
import Text.Printf
import qualified Challenges.Y2015 as Y2015
import qualified Challenges.Y2019 as Y2019
import qualified Challenges.Y2022 as Y2022
import qualified Challenges.Y2023 as Y2023
import Data.Text (unpack)

main :: IO ()
main = runProg =<< execParser opts
    where
        opts = info (args <**> helper)
            ( fullDesc
            <> progDesc "Run an advent of code solution"
            <> header "advent - running advent of code solutions" )

getSol :: (Integer, Integer) -> IO (String -> String, String -> String)
getSol (y, d) = return $ getYear y d

getYear :: Integer -> Integer -> (String -> String, String -> String)
getYear 2015 = Y2015.getDay
getYear 2022 = Y2022.getDay
getYear 2023 = Y2023.getDay
--getYear 2019 = Y2019.getDay
--getYear 2022 = Y2022.getDay
getYear _ = error "unsupported year"


runProg :: ProgramArgs -> IO ()
--runProg (Sample test year day) = getSol (test, year, day) >>= runSol >>= putStr
runProg (ProgramArgs cmd year day) = do
    case cmd of
        "solve" -> solve year day
        "test" -> test year day
        "get" -> get year day
        _ -> error "unknown command"

solveAndTime :: String -> (String -> String) -> String -> IO ()
solveAndTime lbl f x = do
    time <- getCPUTime
    let !r = f x
    now <- getCPUTime
    let diff = showDiff $ fromIntegral (now - time) / (10 ^(9 :: Integer))
    putStr (lbl ++ ": " ++ r ++ "\t Computation time: " ++ diff ++ ")" ++ "\n")

showDiff :: Double -> String
showDiff diff
    | diff <    1000 = printf "%0.3f ms" diff
    | diff <   60000 = printf "%0.3f  s" (diff/1000)
    | diff < 3600000 = printf "%0.2f m" (diff/36000)
    | otherwise = let 
        ms = round diff :: Int
        h = round ( diff / 3600000 ) :: Int
        m = (ms - (h*3600000)) `div` 60000
        in printf "%dh%dm" h m

mkDataPath :: Integer -> Integer -> String
mkDataPath y d
    | d <= 9 = "cache/example/" ++ show y ++ "/0" ++ show d ++ ".txt"
    | otherwise = "cache/example" ++ show y ++ "/" ++ show d ++ ".txt"

solve :: Integer -> Integer -> IO ()
solve year day = do
    (a, b) <- getSol (year, day)
    cs <- runAoC_ (aocOpts year) $ AoCInput $ mkDay_ day
    solveAndTime "A" a $ unpack cs
    solveAndTime "B" b $ unpack cs

test :: Integer -> Integer -> IO ()
test year day = do
    (a,b) <- getSol (year,day)
    handle <- openFile (mkDataPath year day) ReadMode
    contents <- hGetContents handle
    solveAndTime "A" a contents
    solveAndTime "B" b contents

aocOpts :: Integer -> AoCOpts
aocOpts y = (defaultAoCOpts y "") { _aCache = Just "./cache"}

get :: Integer -> Integer -> IO ()
get year day = do
    ps <- runAoC_ (aocOpts year) $ AoCPrompt $ mkDay_ day
    putStr $ show ps

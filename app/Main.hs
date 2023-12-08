{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Options.Applicative
import Parser (ProgramArgs (ProgramArgs), args)
import System.CPUTime
import System.IO
import qualified Challenges.Y2015 as Y2015
import qualified Challenges.Y2019 as Y2019
import qualified Challenges.Y2022 as Y2022
import qualified Challenges.Y2023 as Y2023
import Text.Printf

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
getYear 2022 = Y2022.getDay
getYear 2023 = Y2023.getDay
--getYear 2019 = Y2019.getDay
--getYear 2022 = Y2022.getDay
getYear _ = error "unsupported year"


runProg :: ProgramArgs -> IO ()
--runProg (Sample test year day) = getSol (test, year, day) >>= runSol >>= putStr
runProg (ProgramArgs year day) = do
    (a, b) <- getSol (year, day)
    handle <- openFile (mkDataPath year day) ReadMode
    contents <- hGetContents handle
    solveAndTime "A" a contents
    solveAndTime "B" b contents

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

mkDataPath :: Int -> Int -> String
mkDataPath y d
    | d <= 9 = "data/" ++ show y ++ "/0" ++ show d ++ ".txt"
    | otherwise = "data/" ++ show y ++ "/" ++ show d ++ ".txt"

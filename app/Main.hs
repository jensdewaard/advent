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
getYear 2023 = Y2023.getDay
--getYear 2019 = Y2019.getDay
--getYear 2022 = Y2022.getDay
getYear _ = error "unsupported year"


runProg :: ProgramArgs -> IO ()
--runProg (Sample test year day) = getSol (test, year, day) >>= runSol >>= putStr
runProg (ProgramArgs test year day) = do
    (a, b) <- getSol (year, day)
    handle <- openFile (mkDataPath year day) ReadMode
    contents <- hGetContents handle
    time <- getCPUTime
    let !a' = a contents
    time_a <- getCPUTime
    let !b' = b contents
    time_b <- getCPUTime
    let diff_a = showDiff $ fromIntegral (time_a - time) / (10^9)
    let diff_b = showDiff $ fromIntegral (time_b - time_a) / (10^9)
    let out = "A: " ++ a' ++ "\t(" ++ diff_a ++ ")" ++ "\n" ++ "B: " ++ b' ++ "\t(" ++ diff_b ++ ")\n"
    putStr out

showDiff :: Double -> String
showDiff diff = printf "Computation time: %0.5f ms" (diff :: Double)

mkDataPath :: Int -> Int -> String
mkDataPath y d
    | d <= 9 = "data/" ++ show y ++ "/0" ++ show d ++ ".txt"
    | otherwise = "data/" ++ show y ++ "/" ++ show d ++ ".txt"

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Advent
    ( defaultAoCOpts,
      runAoC_,
      mkDay_,
      AoC(AoCPrompt, AoCInput, AoCSubmit),
      AoCOpts(_aCache),
      AoCUserAgent(AoCUserAgent, _auaEmail, _auaRepo), SubmitRes (SubUnknown), Part (Part1, Part2) )
import qualified Challenges.Y2015 as Y2015
import qualified Challenges.Y2016 as Y2016
import qualified Challenges.Y2017 as Y2017
import qualified Challenges.Y2018 as Y2018
import qualified Challenges.Y2019 as Y2019
import qualified Challenges.Y2022 as Y2022
import qualified Challenges.Y2023 as Y2023
import qualified Challenges.Y2024 as Y2024
import Data.Text (unpack, Text)
import Options.Applicative
import Parser (ProgramArgs (ProgramArgs), args)
import System.CPUTime
import System.IO
import Text.Printf
import Control.Monad (void)
import Data.Map (Map, member)

main :: IO ()
main = runProg =<< execParser opts
  where
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> progDesc "Run an advent of code solution"
            <> header "advent - running advent of code solutions"
        )

getSol :: (Integer, Integer) -> IO (String -> String, String -> String)
getSol (y, d) = return $ getYear y d

getYear :: Integer -> Integer -> (String -> String, String -> String)
getYear 2015 = Y2015.getDay
getYear 2016 = Y2016.getDay
getYear 2017 = Y2017.getDay
getYear 2018 = Y2018.getDay
getYear 2019 = Y2019.getDay
getYear 2022 = Y2022.getDay
getYear 2023 = Y2023.getDay
getYear 2024 = Y2024.getDay
-- getYear 2022 = Y2022.getDay
getYear _ = error "unsupported year"

runProg :: ProgramArgs -> IO ()
-- runProg (Sample test year day) = getSol (test, year, day) >>= runSol >>= putStr
runProg (ProgramArgs cmd year day) = do
  case cmd of
    "solve" -> void (solve year day)
    "test" -> void (test year day)
    "get" -> void (get year day)
    "start" -> start year day
    "submit" -> submit year day
    _ -> error "unknown command"

solveAndTime :: String -> (String -> String) -> String -> IO Solution
solveAndTime lbl f x = do
  time <- getCPUTime
  let !r = f x
  now <- getCPUTime
  let diff = fromIntegral (now - time) / (10 ^ (9 :: Integer))
  putStr (lbl ++ ": " ++ r ++ "\t (Computation time: " ++ showDiff diff ++ ")" ++ "\n")
  return $ Solution r diff (SubUnknown "not yet submitted")

showDiff :: Double -> String
showDiff diff
  | diff < 1 = printf "%0.3f μs" (diff * 1000)
  | diff < 1000 = printf "%0.3f ms" diff
  | diff < 60000 = printf "%0.3f  s" (diff / 1000)
  | diff < 3600000 = printf "%0.2f m" (diff / 36000)
  | otherwise =
      let ms = round diff :: Int
          h = round (diff / 3600000) :: Int
          m = (ms - (h * 3600000)) `div` 60000
       in printf "%dh%dm" h m

mkDataPath :: Integer -> Integer -> String
mkDataPath y d
  | d <= 9 = "cache/example/" ++ show y ++ "/0" ++ show d ++ ".txt"
  | otherwise = "cache/example/" ++ show y ++ "/" ++ show d ++ ".txt"

data Solution = Solution
  {   answer :: String
  ,   time   :: Double
  ,   correct :: SubmitRes
  } deriving (Show, Read, Eq)

solve :: Integer -> Integer -> IO (Solution, Solution)
solve year day = do
  (a, b) <- getSol (year, day)
  opts <- aocOpts year
  cs <- runAoC_ opts $ AoCInput $ mkDay_ day
  solA <- solveAndTime "A" a $ unpack cs
  solB <- solveAndTime "B" b $ unpack cs
  return (solA, solB)

test :: Integer -> Integer -> IO (Solution, Solution)
test year day = do
  (a, b) <- getSol (year, day)
  handle <- openFile (mkDataPath year day) ReadMode
  contents <- hGetContents handle
  solA <- solveAndTime "A" a contents
  solB <- solveAndTime "B" b contents
  return (solA, solB)

aocOpts :: Integer -> IO AoCOpts
aocOpts y = do
  handle <- openFile "./aoc-key" ReadMode
  key <- hGetContents handle
  let uAgent = AoCUserAgent {_auaRepo = "github.com/jensdewaard/advent", _auaEmail = "adventofcode.com.diffused666@passmail.net"}
  let opts = defaultAoCOpts uAgent y key
  return opts {_aCache = Just "./cache"}

get :: Integer -> Integer -> IO (Map Part Text)
get year day = do
  opts <- aocOpts year
  runAoC_ opts $ AoCPrompt $ mkDay_ day

submit :: Integer -> Integer -> IO ()
submit year day = do
  opts <- aocOpts year
  parts <- get year day
  (solA, solB) <- solve year day
  (txt, response) <- if Part2 `member` parts
    then runAoC_ opts $ AoCSubmit (mkDay_ day) Part2 (answer solB)
    else runAoC_ opts $ AoCSubmit (mkDay_ day) Part1 (answer solA)

  putStr (show response)


getAnswer :: Integer -> Integer -> IO (Maybe (Solution, Solution))
getAnswer year day = do
  handle <- openFile ("cache/answers/" ++ show year ++ "/" ++ show day) ReadMode
  fileContents <- hGetContents handle
  return $ read fileContents



start :: Integer -> Integer -> IO ()
start year day = do
  -- TODO add a check to see if the file already exists
  let d = (if day <= 9 then "0" else "") ++ show day
  template <- openFile "./src/day_template.hs" ReadMode
  contents <- hGetContents template
  let content = "module Challenges.Y" ++ show year ++ ".Day" ++ d ++ " (solutionA, solutionB) where\n" ++ contents
  handle <- openFile ("./src/Challenges/Y" ++ show year ++ "/Day" ++ d ++ ".hs") WriteMode
  hPutStr handle content
  hClose template
  hClose handle
  return ()

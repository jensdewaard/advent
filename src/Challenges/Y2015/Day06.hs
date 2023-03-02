module Challenges.Y2015.Day06 () where
import Data.Set (Set)
import Shared (Coord)

solutionA :: String -> String
solutionA = undefined
solutionB :: String -> String
solutionB = undefined
input :: Bool -> IO String
input = undefined

toggle :: Set Coord -> Coord -> Set Coord
toggle ls c = undefined

data Command = Toggle Coord | On Coord | Off Coord

runCommand :: Set Coord -> Command -> Set Coord
runCommand w (Toggle c) = toggle w c
runCommand w (On c) = on w c
runCommand w (Off c) = off w c

on = undefined
off = undefined

parseLine :: String -> [Command]
parseLine = undefined

#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

module Challenges.Y2022.Day02 where

import qualified Data.Text as T

main :: IO ()
main = do
    contents <- readFile "inputs/2.txt"
    let lines = T.splitOn (T.pack "\n") (T.pack contents)
    let rounds = map (readRound . T.unpack) lines
    let rounds' = map (readRound' . T.unpack) lines
    print ("Strategy score: " ++ show (sum (map score rounds)))
    print ("Stategy score with results: " ++ show (sum (map score' rounds')))
    
data Move = Rock | Paper | Scissors
data Result = Win | Lose | Draw

readMove :: Char -> Move
readMove 'A' = Rock
readMove 'X' = Rock
readMove 'B' = Paper
readMove 'Y' = Paper
readMove 'C' = Scissors
readMove 'Z' = Scissors

readResult :: Char -> Result
readResult 'X' = Lose
readResult 'Y' = Draw
readResult 'Z' = Win

type Round = (Move, Move)
type Round' = (Move, Result)

readRound :: String -> Maybe Round
readRound [l, ' ', r] = Just (readMove l, readMove r)
readRound "" = Nothing

readRound' :: String -> Maybe Round'
readRound' [l, ' ', r] = Just(readMove l, readResult r)
readRound' "" = Nothing

score :: Maybe Round -> Int
score (Just (Rock, Rock)) = 1 + 3
score (Just (Rock, Paper)) = 2 + 6
score (Just (Rock, Scissors)) = 3 + 0
score (Just (Paper, Rock)) = 1 + 0
score (Just (Paper, Paper)) = 2 + 3
score (Just (Paper, Scissors)) = 3 + 6
score (Just (Scissors, Rock)) = 1 + 6
score (Just (Scissors, Paper)) = 2 + 0
score (Just (Scissors, Scissors)) = 3 + 3
score Nothing = 0

score' :: Maybe Round' -> Int
score' (Just (Rock, Draw)) = 1 + 3
score' (Just (Rock, Win)) = 2 + 6
score' (Just (Rock, Lose)) = 3 + 0
score' (Just (Paper, Lose)) = 1 + 0
score' (Just (Paper, Draw)) = 2 + 3
score' (Just (Paper, Win)) = 3 + 6
score' (Just (Scissors, Win)) = 1 + 6
score' (Just (Scissors, Lose)) = 2 + 0
score' (Just (Scissors, Draw)) = 3 + 3
score' Nothing = 0

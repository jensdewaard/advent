#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

module Challenges.Y2022.Day07 where

import qualified Data.Text as T
import Data.List

main :: IO ()
main = do
    contents <- readFile "inputs/7.txt"
    files <- readFile "files.txt"
    let ls = filter (/= "") $ map T.unpack (T.splitOn (T.pack "\n") (T.pack contents))
    let t = head $ fst $ readDirectoryContents ls
    let smallDirs = dirSizeWhere (<=100000) t
    print $ "Sum of dirs below 100000: " ++ show (sum $ map size smallDirs)
    let neededSpace = 30000000
    let totalSpace  = 70000000
    let usedSpace = size t
    let makeFree = neededSpace - (totalSpace - usedSpace)
    let ds = dirSizeWhere (>= makeFree) t -- a list of dirs large enough to free the required space
    let ss = map size ds -- the spaces used by those dirs
    print $ "Should delete dir with size: " ++ show (minimum ss)


data Tree = Dir String Int [Tree] | File String Int deriving (Show)

readDirectoryContents :: [String] -> ([Tree], [String])
readDirectoryContents [] = ([], [])
readDirectoryContents ("$ cd .." : ls) = ([], ls) -- leave directory
readDirectoryContents ("$ ls" : ls) = readDirectoryContents ls
readDirectoryContents (l : ls)
    | "$ cd " `isPrefixOf` l = (Dir dirName (sum $ map size subdirContents) subdirContents : rest , remainingAfterCurrent) 
    | "dir" `isPrefixOf` l = readDirectoryContents ls
    | otherwise = (readFileNode l : rest', parsed')
    where
        dirName = drop 5 l
        (subdirContents, remainingAfterSubdir) = readDirectoryContents ls
        (rest, remainingAfterCurrent) = readDirectoryContents remainingAfterSubdir
        (rest', parsed') = readDirectoryContents ls

readFileNode :: String -> Tree
readFileNode s = File name size where
    parsedFile = T.splitOn (T.pack " ") (T.pack s)
    size = readInt $ head parsedFile
    name = T.unpack $ last parsedFile

readInt :: T.Text -> Int
readInt t
  | s == "" = 0
  | otherwise = read s
  where 
    s = T.unpack t

size :: Tree -> Int
size (Dir _ s ts) = s
size (File _ s) = s

dirSizeWhere :: (Int -> Bool) -> Tree -> [Tree]
dirSizeWhere pred (File _ _) = []
dirSizeWhere pred (Dir n s ts) = 
    if pred s 
       then Dir n s ts : rest
       else rest
    where
        rest = concatMap (dirSizeWhere pred) ts

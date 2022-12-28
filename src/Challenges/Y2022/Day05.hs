#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

module Challenges.Y2022.Day05 where

import qualified Data.Text as T
import Data.List as L
import Data.Sequence as S

main :: IO ()
main = do
    contents <- readFile "inputs/5.txt"
    let ps = T.splitOn (T.pack "\n\n") (T.pack contents)
    let a = map T.unpack (T.splitOn (T.pack "\n") (head ps))
    let a' = arrClearEmpty $ readArrangement a
    let ms = map (readMove . T.unpack) (T.splitOn (T.pack "\n") (last ps))
    print ("9000: " ++ show (arrTop (foldl (doMove L.reverse) a' ms)))
    print ("9001: " ++ show (arrTop (foldl (doMove id) a' ms)))

data Box = EmptyBox | Contains Char deriving (Eq, Show)

type Assignment = [Int]
type Stack = [Box]
type Arrangement = S.Seq Stack

data Move = Move Int Int Int deriving (Show)

readArrangement :: [String] -> Arrangement
readArrangement ss = 
    foldr (\l -> arrAppend (readArrangementLine (l ++ ""))) S.empty (init ss)

readArrangementLine :: String -> Arrangement
readArrangementLine "" = S.empty
readArrangementLine s = [s'] :<| bs where
    s' = readBox (L.take 4 s)
    bs = readArrangementLine (L.drop 4 s)

readBox :: String -> Box
readBox s = if b == "" then EmptyBox else Contains (head b) where
    b = unpackBox s

readMove :: String -> Move
readMove "" = Move 0 0 0
readMove s = readMove' (T.splitOn (T.pack " ") (T.pack s))

readMove' :: [T.Text] -> Move
readMove' t = Move (readInt (t !! 1)) (readInt (t !! 3)) (readInt (t !! 5))

arrAppend :: Arrangement -> Arrangement -> Arrangement
arrAppend s t
    | S.null s, S.null t = S.empty
    | S.null s = t
    | S.null t = s
    | otherwise = (s' ++ t') :<| arrAppend ss tt where
        (s' :<| ss) = s
        (t' :<| tt) = t

arrTop :: Arrangement -> String
arrTop s
  | S.null s = "" 
  | otherwise = if L.null s' then " " ++ arrTop ss else c : arrTop ss
  where 
      Contains c = head s'
      (s' :<| ss) = s

doMove :: ([Box] -> [Box]) -> Arrangement -> Move -> Arrangement
doMove f a (Move 0 _ _) = a
doMove f a (Move n s e) = a' where
    taken = L.take n (index a (s-1))
    s' = L.drop n (index a (s-1))
    e' = f taken ++ index a (e-1)
    a' = update (e-1) e' (update (s-1) s' a)

readInt :: T.Text -> Int
readInt s = read (T.unpack s)

unpackBox :: String -> String
unpackBox cs = [ c | c <- cs, c `notElem` "[] " ]

arrClearEmpty :: Arrangement -> Arrangement
arrClearEmpty = fmap stackClearEmpty

stackClearEmpty :: Stack -> Stack
stackClearEmpty [] = []
stackClearEmpty (EmptyBox:bs) = stackClearEmpty bs
stackClearEmpty (b:bs) = b:bs

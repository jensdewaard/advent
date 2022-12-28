#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

module Challenges.Y2022.Day11 where

import Data.List

main :: IO ()
main = do
    let ms = inputMonkeys
    let mbs = iterate (doRound (`div` 3)) ms
    let ds = product $ map test ms
    let mbs' = iterate (doRound (`mod` ds)) ms
    print $ mBusiness $ map inspections (last $ take 21 mbs)
    print $ mBusiness $ map inspections (last $ take 10001 mbs')

divTest :: Int -> Int -> Bool
divTest x y = mod x y == 0

exampleMonkeys :: [Monkey]
exampleMonkeys = let
    m0 = Monkey { idx = 0, items = [79, 98], op = (*19), test = 23, ifTrue = 2, ifFalse = 3, inspections = 0 }
    m1 = Monkey { idx = 1, items = [54, 65, 75, 74], op = (+6), test = 19, ifTrue = 2, ifFalse = 0, inspections = 0 }
    m2 = Monkey { idx = 2, items = [79, 60, 97], op = \x -> x*x, test = 13, ifTrue = 1, ifFalse = 3, inspections = 0 }
    m3 = Monkey { idx = 3, items = [74], op = (+3), test = 17, ifTrue = 0, ifFalse = 1, inspections = 0 }
    in [m0, m1, m2, m3]

inputMonkeys :: [Monkey]
inputMonkeys = let
    m0 = Monkey { idx = 0, items = [72, 64, 51, 57, 93, 97, 68], op = (*19), test = 17, ifTrue = 4, ifFalse = 7, inspections = 0 }
    m1 = Monkey { idx = 1, items = [62], op = (*11), test = 3, ifTrue = 3, ifFalse = 2, inspections = 0 }
    m2 = Monkey { idx = 2, items = [57, 94, 69, 79, 72], op = (+6), test = 19, ifTrue = 0, ifFalse = 4, inspections = 0 }
    m3 = Monkey { idx = 3, items = [80, 64, 92, 93, 64, 56], op = (+5), test = 7, ifTrue = 2, ifFalse = 0, inspections = 0 }
    m4 = Monkey { idx = 4, items = [70, 88, 95, 99, 78, 72, 65, 94], op = (+7), test = 2, ifTrue = 7, ifFalse = 5, inspections = 0 }
    m5 = Monkey { idx = 5, items = [57, 95, 81, 61], op = \x -> x*x, test = 5, ifTrue = 1, ifFalse = 6, inspections = 0 }
    m6 = Monkey { idx = 6, items = [79, 99], op = (+2), test = 11, ifTrue = 3, ifFalse = 1, inspections = 0 }
    m7 = Monkey { idx = 7, items = [68, 98, 62], op = (+3), test = 13, ifTrue = 5, ifFalse = 6, inspections = 0 }
    in [m0, m1, m2, m3, m4, m5, m6, m7]


data Monkey = Monkey 
    { idx :: Int
    , items :: [Int]
    , op :: Int -> Int
    , test :: Int
    , ifTrue :: Int
    , ifFalse :: Int
    , inspections :: Int
    }

doRound :: (Int -> Int) -> [Monkey] -> [Monkey]
doRound _ [] = []
doRound f ms = round' f 0 ms
    where 
        round' f i ms' = if i >= length ms' then ms' else
            let m' = ms' !! i in
            let ms'' = turn f ms' m' in
            round' f (i+1) ms'' 

--round :: [Monkey] -> [Monkey]
--round ms = foldl turn ms ms

throwItem :: (Int -> Int) -- worry mitigation
    -> [Monkey]  -- all monkeys
    -> Monkey -- the throwing monkey
    -> Monkey -- the receiving monkey
    -> (Monkey, [Monkey])
throwItem f ms m n = let 
    (i:is) = items m 
    m' = m {items=is, inspections= inspections m + 1 }
    in (m' , addItem (updateMonkey ms m') n (f $ op m i))

turn' :: (Int -> Int) -> [Monkey] -> Monkey -> (Monkey, [Monkey])
turn' f ms m = let 
    i = f $ op m (head $ items m)
    f' ms m = throwItem f ms m (if divTest i (test m) then getMonkey ms (ifTrue m) else getMonkey ms (ifFalse m)) 
    in f' ms m


turn :: (Int -> Int) -> [Monkey] -> Monkey -> [Monkey]
turn f ms m@(Monkey { items = [] }) = ms
turn f ms m@(Monkey { items = (i:is) }) =
    let (m', ms') = turn' f ms m in turn f ms' m'

hasItems :: Monkey -> Bool
hasItems m = not (null (items m))

getMonkey :: [Monkey] -> Int -> Monkey
getMonkey [] _ = error "no monkey with index"
getMonkey (m:ms) i = if idx m == i then m else getMonkey ms i

addItem :: [Monkey] -> Monkey -> Int -> [Monkey]
addItem ms m i = let m' = m { items = items m ++ [i] } in
    updateMonkey ms m'

updateMonkey :: [Monkey] -> Monkey -> [Monkey]
updateMonkey ms m = map (\x -> if idx m == idx x then m else x) ms

mBusiness :: [Int] -> Int
mBusiness =  product . take 2 . reverse . sort

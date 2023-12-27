module Challenges.Y2015.Day21 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Data.List (minimumBy, maximumBy)
import Common.Prelude
import Parsing (int)

solutionA :: String -> String
solutionA = solve parser (\b -> minCost $ filter (wouldWin 100 b) $ map (foldl equip (0,0,0)) loadouts)
solutionB :: String -> String
solutionB = solve parser (\b -> maxCost $ filter (not . wouldWin 100 b) $ map (foldl equip (0,0,0)) loadouts)

--          Cost  DMG  Armor
type Item = (Int, Int, Int)
--           HP   DMG   Armor
type Boss = (Int, Int, Int)

minCost :: [Item] -> Int
minCost = fst3 . minimumBy (\a b -> fst3 a `compare` fst3 b)

maxCost :: [Item] -> Int
maxCost = fst3 . maximumBy (\a b -> fst3 a `compare` fst3 b)

equip :: Item -> Item -> Item
equip (a,b,c) (x,y,z) = (a+x,b+y,c+z)

wouldWin :: Int -> Boss -> Item -> Bool
wouldWin myHp (bossHp, bossDmg, bossArmor) eq@(_, myDmg, myArmor) =
    let
        bossHp' = bossHp - max 1 (myDmg - bossArmor)
        myHp' = myHp - max 1 (bossDmg - myArmor)
    in ((bossHp' <= 0) || ((myHp' > 0) && wouldWin myHp' (bossHp', bossDmg, bossArmor) eq))

loadouts :: [[Item]]
loadouts = [ [w , a , r1 , r2] |
    w <- weapons,
    a <- armor,
    r1 <- rings,
    r2 <- rings,
    r1 /= r2 || r1 == (0,0,0)
    ]

weapons :: [Item]
weapons = [(8,4,0), (10,5,0), (25, 6, 0),
    (40, 7, 0), (74,8,0)]

armor :: [Item]
armor = [(0,0,0), (13,0,1), (31,0,2),
    (53,0,3),(75,0,4), (102, 0, 5)]

rings :: [Item]
rings = [(0,0,0), (25,1,0), (50,2,0),
    (100,3,0),(20,0,1),(40,0,2),
    (80,0,3)]

parser :: Parser Boss
parser = do
    _ <- string "Hit Points: "
    hp <- int
    _ <- newline >> string "Damage: "
    dmg <- int
    _ <- newline >> string "Armor: "
    a <- int
    return (hp, dmg, a)
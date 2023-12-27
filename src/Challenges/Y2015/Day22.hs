module Challenges.Y2015.Day22 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.Prelude
import Common.Search (bfs)
import Data.List (singleton, minimumBy)
import Parsing (int)
import Prelude hiding (round)

solutionA :: String -> String
solutionA = solve parser (minCost . filter ((==Player) . (\(m,_,_,_) -> m)) . bfs (round Easy) . singleton . gameStart)
solutionB :: String -> String
solutionB = solve parser (minCost . filter ((==Player) . (\(m,_,_,_) -> m)) . bfs (round Hard) . singleton . gameStart)
-- solutionB = solve parser (const "")

minCost :: [GameState] -> Int
minCost = snd4 . minimumBy (\a b -> snd4 a `compare` snd4 b)

snd4:: (a,b,c,d) -> b
snd4 (_,b,_,_) = b

gameStart :: BossState -> GameState
gameStart b = (Undecided, 0, (50, 500, []), b)

data Difficulty = Easy | Hard deriving (Eq, Ord, Show)
data Spell = Missile | Drain | Shield | Poison | Recharge
data Effect = Shielded Int | Poisoned Int | Recharging Int deriving (Show, Eq, Ord)
data Winner = Player | Boss | Undecided deriving (Show, Eq, Ord)

round :: Difficulty -> GameState -> [GameState]
round d s = [determineWinner $ bossTurn myTurn | myTurn <- playerTurn d s]

determineWinner :: GameState -> GameState
determineWinner s@(Player,_,_,_)= s
determineWinner s@(Boss, _, _, _) = s
determineWinner s@(_, m, p@(playerHp, _, _), b@(bossHp, _, _))
  | bossHp <= 0 = (Player,m,p,b)
  | playerHp <= 0 = (Boss,m,p,b)
  | otherwise = s

bossTurn :: GameState -> GameState
bossTurn s@(Player, _, _, _) = s
bossTurn s@(Boss, _, _, _) = s
bossTurn (w,m,(playerHp, playerMp, playerFx),(bossHp, bossDmg, bossFx)) =
    (w,m,(playerHp', playerMp', playerFx'),(bossHp', bossDmg, bossFx')) where
        playerHp' = playerHp - max 1 (bossDmg - if shielded playerFx then 7 else 0)
        playerMp' = if recharging playerFx then playerMp + 101 else playerMp
        bossHp' = if poisoned bossFx then bossHp - 3 else bossHp
        playerFx' = endEffect playerFx
        bossFx' = endEffect bossFx

playerTurn :: Difficulty -> GameState -> [GameState]
playerTurn _ (Player,_,_,_) = []
playerTurn _ (Boss,_,_,_) = []
playerTurn d (w,m,p,b) = [ determineWinner g' | 
        let p' = if d == Hard then hurt p else p,
        let w' = if dead p' then Boss else w,
        let b' = poison b,
        let p'' = recharge p',
        s <- spellChoices p'' b',
        let g' = cast s (w',m,p'',b')
        ]

dead :: PlayerState -> Bool
dead (hp,_,_) = hp <= 0

hurt :: PlayerState -> PlayerState
hurt (hp,mp,fx) = (hp-1,mp,fx)

recharge :: PlayerState -> PlayerState
recharge (hp,mp,fx) = if recharging fx then (hp,mp+101,fx) else (hp,mp,fx)

poison :: BossState -> BossState
poison (hp,dmg,fx) = if poisoned fx then (hp - 3, dmg, fx) else (hp,dmg,fx)

cast :: Spell -> GameState -> GameState
cast Missile (w,m,(hp,mp,fx),(hp',dmg,fx')) = 
    (w,m+53,(hp, mp - 53, endEffect fx),(hp'-4,dmg, endEffect fx'))
cast Drain (w, m, (hp,mp,fx),(hp',dmg,fx')) = 
    (w,m+73,(hp+2, mp - 73, endEffect fx),(hp'-2,dmg,endEffect fx'))
cast Shield (w, m,(hp,mp,fx),(hp',dmg,fx')) = 
    (w,m+113, (hp, mp - 113, Shielded 6 : endEffect fx), (hp', dmg, endEffect fx'))
cast Poison (w,m,(hp,mp,fx),(hp',dmg,fx')) = 
    (w,m+173,(hp, mp - 173, endEffect fx),(hp',dmg, Poisoned 6 : endEffect fx'))
cast Recharge (w,m,(hp,mp,fx),(hp',dmg,fx')) = 
    (w,m+229,(hp, mp - 229, Recharging 5 : endEffect fx),(hp',dmg, endEffect fx'))

endEffect :: [Effect] -> [Effect]
endEffect [] = []
endEffect (Shielded 1:fx) = endEffect fx
endEffect (Poisoned 1:fx) = endEffect fx
endEffect (Recharging 1:fx) = endEffect fx
endEffect (Shielded n:fx) = Shielded (n-1) : endEffect fx
endEffect (Poisoned n:fx) = Poisoned (n-1) : endEffect fx
endEffect (Recharging n:fx) = Recharging (n-1) : endEffect fx

spellChoices :: PlayerState -> BossState -> [Spell]
spellChoices (_, mp, fx) (_,_,fx')= concat [
    [Missile | mp >= 53],
    [Drain | mp >= 73],
    [Shield | mp >= 113, canCast Shield fx],
    [Poison | mp >= 173, canCast Poison fx'],
    [Recharge | mp >= 229, canCast Recharge fx]
    ]

canCast :: Spell -> [Effect] -> Bool
canCast _ [] = True
canCast Shield (Shielded x:_) = x <= 1
canCast Poison (Poisoned x:_) = x <= 1 
canCast Recharge (Recharging x:_) = x <= 1
canCast x (_:fx) = canCast x fx

shielded :: [Effect] -> Bool
shielded [] = False
shielded (Shielded _:_) = True
shielded (_:fx) = shielded fx

poisoned :: [Effect] -> Bool
poisoned [] = False
poisoned (Poisoned _:_) = True
poisoned (_:fx) = poisoned fx

recharging :: [Effect] -> Bool
recharging [] = False
recharging (Recharging _:_) = True
recharging (_:fx) = recharging fx

--                     mana spend
type GameState = (Winner, Int, PlayerState, BossState)
--                  HP   MP
type PlayerState = (Int, Int, [Effect])
--                 HP Damage                  
type BossState = (Int, Int, [Effect])

parser :: Parser BossState
parser = do
    _ <- string "Hit Points: "
    hp <- int
    _ <- newline >> string "Damage: "
    dmg <- int
    return (hp,dmg,[])
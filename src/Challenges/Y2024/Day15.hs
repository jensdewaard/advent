module Challenges.Y2024.Day15 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, noneOf, newline, many, char, sepBy)
import Control.Arrow ((>>>))
import Common.Coord (Coord, Dir(..), move)
import Common.Parsing (grid)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Applicative ((<|>))
import Data.Bifunctor (first)

solutionA :: String -> String
solutionA = solve parser (runMoves >>> boxes >>> S.map gps >>> sum)
-- solutionA = solve parserWide (\s -> (boxesLHS s, boxesRHS s))
solutionB :: String -> String
-- solutionB = solve parserWide (runMovesW >>> \s -> (boxesLHS s, boxesRHS s))
solutionB = solve parser (widen >>> runMovesW >>> boxesLHS >>> S.map gps >>> sum)

type World = Map Coord Char

data RobotState = RobotState
    {   robot :: Coord
    ,   moves :: [Dir]
    ,   boxes :: Set Coord
    ,   walls :: Set Coord
    } deriving (Eq, Show)

data RobotStateWide = RobotStateWide
    {   robotW :: Coord
    ,   movesW :: [Dir]
    ,   boxesLHS :: Set Coord
    ,   boxesRHS :: Set Coord
    ,   wallsW :: Set Coord
    } deriving (Eq, Show)

-- (1,_) -> (1,_),(2,_)
-- (2,_) -> (3,)  (4,)
-- (3,_) -> (5,_) (6,_)
widen :: RobotState -> RobotStateWide
widen rs = let
    widenX x = (2 * (x-1)) + 1
    (rx, ry) = robot rs
    ws = concatMap (\(x,y) -> [(widenX x,y), (widenX x + 1, y)]) (S.toList $ walls rs)
    lbs = S.map (first widenX) (boxes rs)
    rbs = S.map (\(x,y) -> (widenX x + 1, y)) (boxes rs)
    in RobotStateWide (widenX rx, ry) (moves rs) lbs rbs (S.fromList ws)

gps :: Coord -> Int
gps (x,y) = x - 1 + 100 * (y - 1)

runMovesW :: RobotStateWide -> RobotStateWide
runMovesW rs
    | null (movesW rs) = rs
runMovesW rs = let
   mvs = movesW rs
   mrs = pushWide (robotW rs) (head mvs) rs
   in case mrs of
       Just rs' -> runMovesW rs' { movesW = tail mvs }
       Nothing -> runMovesW rs { movesW = tail mvs }

runMoves :: RobotState -> RobotState
runMoves rs
    | null (moves rs) = rs
runMoves rs = let
   mvs = moves rs
   mrs = push (robot rs) (head mvs) rs
   in case mrs of
       Just rs' -> runMoves rs' { moves = tail mvs }
       Nothing -> runMoves rs { moves = tail mvs }

-- | Move an object located at coordinate in a direction.
-- Returns the new world state in case of a succesful push.
push :: Coord -> Dir -> RobotState -> Maybe RobotState
push p d rs
    | p == robot rs = do
        rs' <- push (move d p) d rs
        pure rs' { robot = move d p }
    | p `elem` boxes rs = do
        rs' <- push (move d p) d rs
        pure rs' { boxes = S.insert (move d p) $ S.delete p $ boxes rs'}
    | p `elem` walls rs = Nothing
    | otherwise = Just rs

-- | Move an object located at coordinate in a direction.
-- Returns the new world state in case of a succesful push.
pushWide :: Coord -> Dir -> RobotStateWide -> Maybe RobotStateWide
pushWide p d rs
    | p == robotW rs = do
        rs' <- pushWide (move d p) d rs
        pure rs' { robotW = move d p }
    | p `elem` wallsW rs = Nothing
pushWide p L rs
    | p `elem` boxesLHS rs = do
        rs' <- pushWide (move L p) L rs
        pure rs' { boxesLHS = S.insert (move L p) $ S.delete p $ boxesLHS rs'}
    | p `elem` boxesRHS rs = do
        rs' <- pushWide (move L p) L rs
        pure rs' { boxesRHS = S.insert (move L p) $ S.delete p $ boxesRHS rs'}
pushWide p R rs
    | p `elem` boxesLHS rs = do
        rs' <- pushWide (move R p) R rs
        pure rs' { boxesLHS = S.insert (move R p) $ S.delete p $ boxesLHS rs'}
    | p `elem` boxesRHS rs = do
        rs' <- pushWide (move R p) R rs
        pure rs' { boxesRHS = S.insert (move R p) $ S.delete p $ boxesRHS rs'}
pushWide p d rs
    | p `elem` boxesLHS rs = do
        rs' <- pushWide (move d p) d rs
        rs'' <- pushWide (move R $ move d p) d rs'
        pure rs'' {
            boxesLHS = S.insert (move d p) $ S.delete p $ boxesLHS rs'',
            boxesRHS = S.insert (move R $ move d p) $ S.delete (move R p) $ boxesRHS rs''
        }
    | p `elem` boxesRHS rs = do
        rs' <- pushWide (move d p) d rs
        rs'' <- pushWide (move L $ move d p) d rs'
        pure rs'' {
            boxesLHS = S.insert (move L $ move d p) $ S.delete (move L p) $ boxesLHS rs'',
            boxesRHS = S.insert (move d p) $ S.delete p $ boxesRHS rs''
        }
pushWide _ _ rs = Just rs

splitWorld :: World -> (Coord, Set Coord, Set Coord)
splitWorld w = let
    rp = fst $ head $ M.toList $ M.filter (== '@') w
    bs = S.fromList $ map fst $ M.toList $ M.filter (== 'O') w
    ws = S.fromList $ map fst $ M.toList $ M.filter (== '#') w
    in (rp, bs, ws)


parser :: Parser RobotState
parser = do
    g <- grid (noneOf ['\n', ' '])
    _ <- newline
    ds <- many dir `sepBy` newline
    let (r,bs,ws) = splitWorld g
    return $ RobotState r (mconcat ds) bs ws

dir :: Parser Dir
dir = (char '<' >> return L)
    <|> (char '>' >> return R)
    <|> (char '^' >> return U)
    <|> (char 'v' >> return D)
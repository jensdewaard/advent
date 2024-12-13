{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# Language BlockArguments, ImportQualifiedPost, MonadComprehensions #-}
module Challenges.Y2024.Day12 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, letter, alphaNum)
import Control.Arrow ((>>>))
import Data.List (sort, nub, unfoldr, partition, delete, groupBy)
import Common.Coord (Coord, Dir(..), cardinal, dist, move)
import qualified Common.Coord as C
import Common.Parsing (grid)
import Data.Map (toList, Map)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Common.List (sumWith, count, none)
import Common.Search (bfs,dfs)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.List.Extra (sortOn, groupOn)
import Data.List (group)

-- 761341 is too low
solutionA :: String -> String
solutionA = solve parser (floodRegion >>> sumWith (\(Region r _) -> length r * length (perimeter r)))

solutionB :: String -> String
solutionB = solve parser (floodRegion >>> map (\r -> (flowerR r, area r , length $ sides r)) >>> sumWith (\(_,a,s) -> a*s))

parser :: Parser (Set Garden)
parser = do
    g <- grid letter
    return $ S.fromList $ map (\(c,f) -> Garden c f g) $ toList g


floodRegion :: Set Garden -> [Region]
floodRegion (length -> 0) = []
floodRegion plots = let
        currentGarden = S.take 1 plots
        gardens = bfs adjacent (S.toList currentGarden)
    in mkRegion gardens : floodRegion (plots `S.difference` S.fromList gardens)

adjacent :: Garden -> [Garden]
adjacent g = [ Garden c (fromJust h) (world g) |
        c <- cardinal (coord g),
        let h = M.lookup c (world g),
        h == Just (flower g)
    ]

area :: Region -> Int
area (Region gs _) = length gs

perimeter :: [Garden] -> [(Coord, Dir)]
perimeter gs = let
    gcs = map coord (sort gs)
    allNeighbours = concatMap cardinal' gcs
    outerN = filter (\(c, _) -> c `notElem` gcs) allNeighbours
    in outerN

cardinal' :: Coord -> [(Coord, Dir)]
cardinal' c = [(move U c, U), (move R c, R), (move D c, D), (move L c, L)]

mkRegion :: [Garden] -> Region
mkRegion gs = Region gs (perimeter gs)

sides :: Region -> [[(Coord, Dir)]]
-- sides :: Region -> Int
-- sides (Region gs)
--     | length gs <= 2 = 4
sides (Region _ p) = let
        sds = groupOn snd $ sortOn snd p
    in mconcat $ map (mergeSides . groupsBy (\ (a,_) (b,_) -> dist a b == 1)) sds

mergeSides :: [[(Coord, Dir)]] -> [[(Coord, Dir)]]
mergeSides [] = []
mergeSides (a:as) = let
    x = last a
    d (n,_) (m,_) = dist n m
    b = filter (\l -> d x (head l) == 1) as
    in if null b
        then a : mergeSides as
        else let b' = head b in mergeSides $ (a <> b') : delete b' as

groupsBy :: Eq a => (a -> a -> Bool) -> [a] -> [[a]]
groupsBy _ [] = []
groupsBy predicate (a:as) = let
    (ns,ms) = partition (predicate a) as
    in (a:ns) : groupsBy predicate (delete a ms)


--  ... ... ... ... .#. ... ##. .#.
--  .## .## ##. ##. .#. .#. ##. .##
--  .## ... ... ##. ... .#. ... ...
--
corners :: [Coord] -> Int
corners cs
  = count (corner U L) cs + count (corner U R) cs
  + count (corner D L) cs + count (corner D R) cs
  + 2 * count (uturn L) cs + 2 * count (uturn R) cs
  + 2 * count (uturn U) cs + 2 * count (uturn D) cs
  + 4 * count single cs
  where
    fence dir x = move dir x `elem` cs
    single x = none (`fence` x) [U,D,L,R]
    corner dir1 dir2 x =
        fence dir1 x && (fence dir2 x || fence dir2 (move dir1 x))
    uturn d1 x = fence d1 x && none (`fence` x) (delete d1 [U,D,L,R])


data Garden = Garden
    {   coord  :: Coord
    ,   flower :: Char
    ,   world :: Map Coord Char
    }

flowerR :: Region -> Char
flowerR (Region r _) = flower $ head r

instance Show Garden where
    show g = show (coord g) ++ "," ++ show (flower g)

instance Eq Garden where
  (==) :: Garden -> Garden -> Bool
  (==) g h = coord g == coord h && flower g == flower h

instance Ord Garden where
    compare = comparing coord <> comparing flower

data Region = Region [Garden] [(Coord, Dir)] deriving Show
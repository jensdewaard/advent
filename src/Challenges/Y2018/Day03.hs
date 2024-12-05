module Challenges.Y2018.Day03 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, newline, sepEndBy1, char, string)
import Control.Arrow ((>>>))
import Common.Coord (Coord)
import Common.Parsing (int)
import Data.Map (Map)
import Data.List (delete)
import qualified Data.Map as M

solutionA :: String -> String
solutionA = solve parser (map claimRect >>> mconcat >>> points >>> M.filter (>=2) >>> M.size)
solutionB :: String -> String
solutionB = solve parser (soleSurvivor >>> number)

parser :: Parser [Claim]
parser = claim `sepEndBy1` newline where
    claim = do
        _ <- char '#'
        i <- int
        _ <- string " @ "
        x <- int
        _ <- char ','
        y <- int
        _ <- string ": "
        w <- int
        _ <- char 'x'
        Claim i (x,y) w <$> int

data Claim = Claim
    {   number :: Int
    ,   topLeft :: Coord
    ,   width :: Int
    ,   height :: Int
    } deriving (Eq, Show)

claimRect :: Claim -> Rect 
claimRect (Claim _ (rx,ry) w h) = Rect 
    $ M.fromList [((x,y),1) | x <- [rx..(rx+w - 1)], y <- [ry..(ry+h - 1)]]

soleSurvivor :: [Claim] -> Claim
soleSurvivor cs = head $ [ c |
    c <- cs,
    let ds = delete c cs,
    let (Rect cc) = claimRect c,
    let dcs =  map claimRect ds,
    let overlaps = map (\(Rect dc) -> M.intersection cc dc) dcs,
    all null overlaps
    ]

newtype Rect = Rect (Map Coord Int) deriving (Eq, Show)
points :: Rect -> Map Coord Int
points (Rect m) = m

instance Semigroup Rect where  
    (<>) :: Rect -> Rect -> Rect
    (<>) (Rect m1) (Rect m2) = Rect m where
        m = M.unionWith (+) m1 m2

instance Monoid Rect where
  mempty :: Rect
  mempty = Rect M.empty

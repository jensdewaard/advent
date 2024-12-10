module Challenges.Y2024.Day10 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, digit, char)
import Control.Arrow ((>>>), (&&&))
import Data.Map (Map)
import qualified Data.Map as M
import Common.Coord (Coord, cardinal)
import Common.Parsing (grid)
import Data.Char (digitToInt)
import Data.Maybe (fromJust, isJust)
import Common.Search (bfs)
import Data.Ord (comparing)
import Data.List (nub, nubBy)
import Control.Applicative ((<|>))

solutionA :: String -> String
solutionA = solve parser (findAllTrails >>> filter walkedTrail >>> scores (\p1 p2 -> head (path p1) == head (path p2)))
solutionB :: String -> String
solutionB = solve parser (findAllTrails >>> filter walkedTrail >>> scores (\p1 p2 -> path p1 == path p2))

data SearchState = SearchState
    {   path :: [Coord]
    ,   pos  :: Coord
    ,   world :: Map Coord Char
    }

instance Eq SearchState where
    (==) s t = path s == path t && pos s == pos t

instance Ord SearchState where
    compare = comparing path <> comparing pos

instance Show SearchState where
    show ss = show (pos ss) ++ ": " ++ show (path ss) ++ "\n"

findAllTrails :: Map Coord Char -> [SearchState]
findAllTrails w = let
        starts = map ((\c -> SearchState [c] c w) . fst) $ M.toList $  M.filter (=='0') w in
    bfs nextSteps starts

scores :: (SearchState -> SearchState -> Bool) -> [SearchState] -> Int
scores eq ss = let
        starts = map fst $ M.toList $  M.filter (=='0') (world $ head ss)
    in sum $ map (\c -> uniqueFrom eq c ss) starts

uniqueFrom :: (SearchState -> SearchState -> Bool) -> Coord -> [SearchState] -> Int
uniqueFrom eqOp trailhead ss = length $ nubBy eqOp $ filter ((==trailhead) . last . path) ss

walkedTrail :: SearchState -> Bool
walkedTrail ss = length (path ss) == 10 && M.lookup (head (path ss)) (world ss) == Just '9'

nextSteps :: SearchState -> [SearchState]
nextSteps ss = [SearchState (p' : path ss) p' (world ss) |
                  let oldHeight = digitToInt $ fromJust $ M.lookup (pos ss) (world ss),
                  p' <- cardinal (pos ss),
                  let mnH = M.lookup p' (world ss),
                  isJust mnH && mnH /= Just '.',
                  let newHeight = digitToInt $ fromJust mnH,
                  newHeight == oldHeight + 1
                  ]

parser :: Parser (Map Coord Char)
parser = grid (digit <|> char '.')
module Challenges.Y2024.Day13 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, newline, string, sepEndBy1, optional)
import Control.Arrow ((>>>))
import Common.Parsing (int)
import Data.Bifunctor (second)
import GHC.Float (int2Double)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import Data.Either

-- 158022280630272 is too high
-- 1085856661492 is too low
solutionA :: String -> String
solutionA = solve parser (concatMap (uncurry costM) >>> sum)
solutionB :: String -> String
solutionB = solve parser (map (second farPrize) >>> concatMap (uncurry costM) >>> sum)

farPrize :: Vector Int -> Vector Int
farPrize = V.map (+ 10000000000000)

costM :: Matrix Int -> Vector Int -> [Int]
costM m v = [3 * a + b |
               let inv = M.inverse (int2Double <$> m),
               not (isLeft inv) || error (fromLeft "" inv),
               let mi = fromRight (M.zero 2 2) inv,
               let ans = M.getCol 1 $ M.multStd mi (M.colVector (int2Double <$> v)),
               let a = round $ (V.!) ans 0,
               let b = round $ (V.!) ans 1,
               let mab = M.multStd m (M.colVector (round <$> ans)),
                M.getCol 1 mab == v
            ]

parser :: Parser [(Matrix Int, Vector Int)]
parser = machine `sepEndBy1` newline where
    machine = do
        _ <- string "Button A: X+"
        x1 <- int
        _ <- string ", Y+"
        y1 <- int <* newline
        _ <- string "Button B: X+"
        x2 <- int
        _ <- string ", Y+"
        y2 <- int <* newline
        _ <- string "Prize: X="
        c1 <- int
        _ <- string ", Y="
        c2 <- int <* optional newline
        return (M.fromList 2 2 [x1,x2,y1,y2], V.fromList [c1,c2])
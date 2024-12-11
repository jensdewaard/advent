{-# LANGUAGE ViewPatterns #-}
module Challenges.Y2024.Day11 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, spaces, sepEndBy1)
import Control.Arrow ((>>>))
import Common.Parsing (int)
import qualified Common.FreqMap as F
import Common.FreqMap (FreqMap, applyF)

solutionA :: String -> String
solutionA = solve parser (iterate (applyF blink) >>> (!!25) >>> F.length )
solutionB :: String -> String
solutionB = solve parser (iterate (applyF blink) >>> (!!75) >>> F.length)

blink :: (Semigroup (m Int), Applicative m) => Int -> m Int
blink 0 = pure 1
blink (show -> s)
    | even $ length s  = let
        h = length s `div` 2
        fh = read $ take h s
        sh = read $ drop h s
        in pure fh <> pure sh
blink x = pure (2024 * x)

type Stone = FreqMap Int

parser :: Parser Stone
parser = do
    ns <- int `sepEndBy1` spaces
    return $ F.fromList ns
{-# LANGUAGE ViewPatterns #-}
module Challenges.Y2024.Day11 (solutionA, solutionB) where
import Common.Prelude (solve, HasLength (len), (>==))
import Text.ParserCombinators.Parsec (Parser, spaces, sepEndBy1)
import Control.Arrow ((>>>))
import Common.Parsing (int)
import Common.FreqMap (FreqMap)
-- import Common.LinkedList (LinkedList)

solutionA :: String -> String
solutionA = solve parser (iterate (>== blink) >>> (!!25) >>> len )
solutionB :: String -> String
solutionB = solve parser (iterate (>== blink) >>> (!!75) >>> len )

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
    return $ fromList ns

fromList :: Ord a => [a] -> FreqMap a
fromList = foldr ((<>) . pure) mempty

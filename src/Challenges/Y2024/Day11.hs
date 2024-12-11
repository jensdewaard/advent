module Challenges.Y2024.Day11 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, spaces, sepEndBy1)
import Control.Arrow ((>>>))
import Common.Parsing (int)
import qualified Common.FreqMap as F
import Common.FreqMap (FreqMap (..), singleton)

solutionA :: String -> String
solutionA = solve parser (iterate blink >>> (!!25) >>> F.length )
solutionB :: String -> String
solutionB = solve parser (iterate blink >>> (!!75) >>> F.length)

blink :: Stone -> Stone
blink (FM []) = mempty
blink (FM ((txt,n):ns))
    | txt == 0 = singleton 1 n <> blink (FM ns)
    | even (length $ show txt)  = let
        s = show txt
        h = length s `div` 2
        e1 :: Int
        e1 = read $ take h s
        e2 :: Int
        e2 = read $ drop h s
        in singleton e1 n <> singleton e2 n <> blink (FM ns)
    | otherwise = singleton (2024 * txt) n <> blink (FM ns)

type Stone = FreqMap Int

parser :: Parser Stone
parser = do
    ns <- int `sepEndBy1` spaces
    return $ F.fromList ns 
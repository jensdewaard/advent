module Challenges.Y2024.Day11 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, many1, spaces, sepEndBy1)
import Control.Arrow ((>>>), (&&&))
import Data.Maybe (isJust, fromJust)
import Common.Parsing (int)
import qualified Common.LinkedList as LL
import Common.LinkedList (LinkedList(..))

solutionA :: String -> String
solutionA = solve parser (iterate blink >>> (!!25) >>> LL.length )
solutionB :: String -> String
solutionB = solve parser (iterate blink >>> (!!0) >>> LL.length )

blink :: Stone -> Stone
blink ListEnd = ListEnd
blink (Link txt ns)
    | txt == "0" = Link "1" (blink ns)
    | even (length txt)  = let
        h = length txt `div` 2
        e1 :: Int
        e1 = read $ take h txt
        e2 :: Int
        e2 = read $ drop h txt
        in Link (show e1) (Link (show e2) (blink ns))
    | otherwise = Link (show (2024 * read txt)) (blink ns)

type Stone = LinkedList String

parser :: Parser Stone
parser = do
    ns <- int `sepEndBy1` spaces
    return $ LL.fromList $ map show ns
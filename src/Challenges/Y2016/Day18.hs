module Challenges.Y2016.Day18 (solutionA, solutionB) where
import Common.Prelude
import Common.List (count)
import Text.ParserCombinators.Parsec hiding (count)
import Data.List (tails, inits)

solutionA :: String -> String
solutionA = solve parser (sum . map (count not) . take 40 . iterate nextRow)
solutionB :: String -> String
solutionB = solve parser (sum . map (count not) . take 400000 . iterate nextRow)

type Trapped = Bool

nextRow :: [Trapped] -> [Trapped]
nextRow s = take (length s) . drop 2 . map trapped . window 3 . (++ [False]) . ([False] ++) $ s

trapped :: [Trapped] -> Trapped
trapped [True , True , False] = True
trapped [False, True, True ] = True
trapped [True, False, False] = True
trapped [False, False, True] = True
trapped [True, False] = True
trapped _ = False

parser :: Parser [Trapped]
parser = many1 trap where trap = (char '.' >> return False) <|> (char '^' >> return True)

window :: Int -> [a] -> [[a]]
window k xs | k > 0
   = a
     ++ replicate (k - length a) xs
     ++ (init . map (take k) . tails
              . drop 1 $ xs)
   where
   a = take k . tail $ inits xs
window _ _ = error "wrong arguments"

module Challenges.Y2023.Day12 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec 
import Shared (solve)
import Parsing (int)
import Data.Function (fix)
import Data.List (intercalate)
import Data.MemoCombinators (memo2, list, )
import qualified Data.MemoCombinators as M

solutionA :: String -> String
solutionA = solve parser (sum . map ( uncurry (fix assignSpring)))
solutionB :: String -> String
solutionB = solve parser (sum . map (uncurry solveB . unfold))

type Row = ([Int], [Character])

data Character = Hash | Dot | Unknown deriving (Eq, Enum)

solveB :: [Int] -> [Character] -> Int
solveB = memo2 (list M.integral) (list M.enum) assign'
    where assign' = assignSpring solveB


unfold :: Row -> Row
unfold (g,r) = (concat $ replicate 5 g, intercalate [Unknown] $ replicate 5 r)

assignSpring :: ([Int] -> [Character] -> Int) -> [Int] -> [Character] -> Int
-- if there are still unmatched groups, this is not a valid solution branch
assignSpring _ b [] = if null b then 1 else 0
-- if there are still # but no more groups, this is not a valid solution branch
assignSpring  _ [] a  = if Hash `elem` a then 0 else 1
-- check the first character
assignSpring f gs (Unknown:cs) = f gs (Hash:cs) + f gs (Dot:cs) 
assignSpring f gs (Dot:cs)  = f gs cs
assignSpring f (g:gs) s@(Hash:_)
    | Dot `elem` take g s = 0 -- illegal string, because there are dots part of the group
    | length s < g = 0        -- illegal string, because it's shorter than the size of the next group 
    | length s == g = f gs [] -- end of string
    | Hash == head (drop g s) = 0 -- check the next character after the group of g springs
    | otherwise = let
        springs = replicate g Hash ++ [Dot]
        in f gs (drop (length springs) s)

parser :: Parser [Row]
parser = row `sepEndBy1` newline where
    row :: Parser Row
    row = do
        ss <- many1 ((char '#' >> return Hash) <|> (char '.' >> return Dot) <|> (char '?' >> return Unknown))
        _ <- space
        gs <- int `sepBy1` char ','
        return (gs, ss)
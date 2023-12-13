module Challenges.Y2023.Day12 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve, prepend)
import Parsing (int)
import Data.Function (fix)
import Data.List (intercalate)

solutionA :: String -> String
solutionA = solve parser (sum . map ( uncurry (fix assignSpring)))
solutionB :: String -> String
-- solutionB = solve parser (sum . map (length . (fix assignSpring) . unfold))
solutionB = const ""

type Row = ([Char], [Int])

unfold :: Row -> Row
unfold (r,g) = (intercalate "?" $ replicate 5 (r ++ ['?']), concat $ replicate 5 g)

assignSpring :: ([Char] -> [Int] -> Int) -> [Char] -> [Int] -> Int
-- if there are still unmatched groups, this is not a valid solution branch
assignSpring _ [] b = if null b then 1 else 0
-- if there are still # but no more groups, this is not a valid solution branch
assignSpring _  a []  = if '#' `elem` a then 0 else 1
-- check the first character
assignSpring f ('?':cs) gs =  f ('#':cs) gs + f ('.':cs) gs
assignSpring f ('.':cs)  gs = f cs  gs
assignSpring f s@('#':_)  (g:gs)
    | '.' `elem` take g s = 0 -- illegal string, because there are dots part of the group
    | length s < g = 0        -- illegal string, because it's shorter than the size of the next group 
    | length s == g = f "" gs -- end of string
    | '#' == head (drop g s) = 0 -- check the next character after the group of g springs
    | otherwise = let
        springs = replicate g '#' ++ ['.']
        in f (drop (length springs) s) gs
assignSpring _  s _ = error ("illegal character in string: " ++ s)

parser :: Parser [Row]
parser = row `sepEndBy1` newline where
    row :: Parser Row
    row = do
        ss <- many1 (char '#' <|> char '.' <|> char '?')
        _ <- space
        gs <- int `sepBy1` char ','
        return (ss, gs)
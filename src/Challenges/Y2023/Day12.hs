module Challenges.Y2023.Day12 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve, prepend)
import Parsing (int)

solutionA :: String -> String
solutionA = solve parser (sum . map (length . assignSpring))
solutionB :: String -> String
solutionB = solve parser (sum . map (length . assignSpring . unfold))

type Row = ([Char], [Int])

unfold :: Row -> Row
unfold (r,g) = (init $ concat $ replicate 5 (r ++ ['?']), concat $ replicate 5 g)

assignSpring :: Row -> [Row]
-- if there are still unmatched groups, this is not a valid solution branch
assignSpring ([],b) = [([],[]) | null b]
-- if there are still # but no more groups, this is not a valid solution branch
assignSpring (a,[]) = [(replicate (length a) '.',[]) | '#' `notElem` a]
-- check the first character
assignSpring ('?':cs, g:gs) =  assignSpring ('#':cs, g:gs) ++ assignSpring ('.':cs,g:gs)
assignSpring ('.':cs, gs) = map (prepend (['.'],[])) $ assignSpring (cs, gs)
assignSpring (s@('#':_), g:gs)
    | '.' `elem` take g s = [] -- illegal string, because there are dots part of the group
    | length s < g = []        -- illegal string, because it's shorter than the size of the next group 
    | length s == g = map (prepend (replicate g '#', [g])) $ assignSpring ("", gs) -- end of string
    | '#' == head (drop g s) = [] -- check the next character after the group of g springs
    | otherwise = let
        springs = replicate g '#' ++ ['.']
        in map (prepend (springs, [g])) $ assignSpring (drop (length springs) s, gs)
assignSpring _ = error "illegal character in string"

parser :: Parser [Row]
parser = row `sepEndBy1` newline where
    row :: Parser Row
    row = do
        ss <- many1 (char '#' <|> char '.' <|> char '?')
        _ <- space
        gs <- int `sepBy1` char ','
        return (ss, gs)
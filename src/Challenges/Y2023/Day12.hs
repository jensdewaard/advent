module Challenges.Y2023.Day12 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve, prepend)
import Parsing (int)
import Data.List (elemIndex, nub)
import Data.Maybe (isNothing)

solutionA :: String -> String
solutionA = solve parser (map (assignSpring))
solutionB :: String -> String
solutionB = solve parser (id)

type Row = ([Char], [Int])

verifyAgainst :: Row -> Row -> Bool
verifyAgainst (os,og) (rs,rg)
    | length os /= length rs = False
    | otherwise = all (uncurry verifyChar) (zip os rs) && og == rg
        where
            verifyChar :: Char -> Char -> Bool
            verifyChar '#' '#' = True
            verifyChar '.' '.' = True
            verifyChar '.' '#' = False
            verifyChar '#' '.' = False
            verifyChar '?' '.' = True
            verifyChar '?' '#' = True
            verifyChar _ _ = False


assignSpring :: Row -> [Row]
-- if there are still unmatched groups, this is not a valid solution branch
assignSpring ([],b) = [([],[]) | null b]
-- if there are still unassigned ?, this is not a valid solution branch
assignSpring (a,[]) = [([],[]) | isNothing (elemIndex '?' a)]
-- check the first character
assignSpring ('.':cs, g:gs) = map (prepend (['.'],[])) $ assignSpring (cs, g:gs)
assignSpring ('?':cs, g:gs) = assignSpring ('.':cs,g:gs) ++ assignSpring ('#':cs, g:gs)
assignSpring (s@('#':cs), g:gs)
    | '.' `elem` take g s = [] -- illegal string, because there are dots part of the group
    | length s < g = []        -- illegal string, because it's shorter than the size of the next group 
    | length s == g = map (prepend (replicate g '#', [g])) $ assignSpring (drop g s, gs) -- end of string
    | '#' == head (drop g s) = [] -- check the next character after the group of g springs
    | otherwise = let
        springs = replicate g '#' ++ ['.']
        in map (prepend (springs, [g])) $ assignSpring (drop (length springs) s, gs)
    -- rest = drop g (c:cs) -- the 
    -- in  if length rest > 0 && head rest == '#' then [] else
        -- if length rest > 0 && head rest == '?' then map (prepend (springs, [g])) $ assignSpring ('.' : tail rest, gs)
        -- else map (prepend (springs, [g])) $ assignSpring (rest, gs)
    -- | c == '?' = let
    --     next = take (g-1) cs
    --     springs = replicate g '#'
    --     rest = drop g cs
    --     assignedDot = map (id) $ assignSpring (cs, g:gs)
    --     in assignedDot ++ if any (=='.') next -- some of the next g characters are ., so we cannot assign #
    --         then []
    --         -- assigning '#' is possible, so we try both '.' and '#'
    --         else map (prepend (springs ++ if length rest == 0 then [] else ['.'], [g])) 
    --             $ assignSpring (rest, gs)
assignSpring _ = error "illegal character in string"

parser :: Parser [Row]
parser = row `sepEndBy1` newline where
    row :: Parser Row
    row = do
        ss <- many1 (char '#' <|> char '.' <|> char '?')
        _ <- space
        gs <- int `sepBy1` char ','
        return (ss, gs)
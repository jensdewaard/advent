module Challenges.Y2023.Day12 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve, prepend)
import Parsing (int)
import Data.List (elemIndex, nub)

solutionA :: String -> String
solutionA = solve parser (map (assignSpring'))
solutionB :: String -> String
solutionB = solve parser (id)

type Row = ([Char], [Int])

assignSpring' :: Row -> [Row]
assignSpring' r@(s,g) = let rs = assignSpring r in nub $ filter (verifyAgainst r) rs

verifyAgainst :: Row -> Row -> Bool
verifyAgainst (os,og) (rs,rg)
    | length os /= length rs = False
    | otherwise = (all (uncurry verifyChar) $ zip os rs) && og == rg
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
assignSpring ([],b) = if length b > 0 then [] else [([],[])]
assignSpring (a,[]) = if elemIndex '?' a == Nothing then [([],[])] else []
assignSpring ((c:cs), (g:gs))
    | c == '.' = map (prepend (['.'],[])) $ assignSpring (cs, g:gs)
    | c == '#' = let 
        next = take g (c:cs)
        rest = drop g (c:cs)
        springs = replicate g '#' ++ if length rest == 0 then [] else ['.']
        in  if length rest > 0 && head rest = '#' then [] else
            if length rest > 0 && head rest = '?' 
                then []
                else map (prepend (springs, [g])) $ assignSpring (rest, gs)
    | c == '?' = assignSpring (('.':cs) ,(g:gs)) ++ assignSpring (('#':cs),(g:gs))
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

parser :: Parser [Row]
parser = row `sepEndBy1` newline where
    row :: Parser Row
    row = do
        ss <- many1 (char '#' <|> char '.' <|> char '?')
        _ <- space
        gs <- int `sepBy1` char ','
        return (ss, gs)
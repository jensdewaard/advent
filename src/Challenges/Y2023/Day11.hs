module Challenges.Y2023.Day11 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve, mapIf)
import Common.Coord (Coord, belowN, rightN, dist)
import Data.Bifunctor (first)

solutionA :: String -> String
solutionA  = solve parser (sum . map (uncurry dist) . pairs . expandUniverse 1)
solutionB :: String -> String
solutionB = solve parser (sum . map (uncurry dist) . pairs . expandUniverse 999999)

pairs :: [(Coord, Char)] -> [(Coord, Coord)]
pairs u = [(p, q) | let stars = map fst $ filter (\x -> snd x == '#') u,
                    p <- stars, q <- stars,
                    p < q]

expandUniverse :: Int -> [(Coord,Char)] -> [(Coord,Char)]
expandUniverse n u = expandX n xs $ expandY n ys u
    where 
        xs = emptyCols u
        ys = emptyRows u


emptyCols :: [(Coord, Char)] -> [Int]
emptyCols u = let maxX = maximum $ map (fst . fst) u in [x | 
                    x <- [1..maxX],
                    let cs = filter (\cc -> fst (fst cc) == x && snd cc == '.') u,
                    length cs == maxX
                  ]

emptyRows :: [(Coord, Char)] -> [Int]
emptyRows u = let maxY = maximum $ map (snd .fst) u in [y | y <- [1..maxY],
                  let cs = filter (\cc -> snd (fst cc) == y && snd cc == '.') u,
                  length cs == maxY]

expandY :: Int -> [Int] -> [(Coord, Char)] -> [(Coord, Char)]
expandY _ [] u = u
expandY n (a:as) u = expandY n (map (+ n) as) (mapIf ((>=a) . snd . fst) (first (belowN n)) u)

expandX  :: Int -> [Int] -> [(Coord, Char)] -> [(Coord, Char)]
expandX _ [] u = u
expandX n (a:as) u = expandX n (map (+ n) as) (mapIf ((>=a) . fst . fst) (first (rightN n)) u)

parser :: Parser [(Coord,Char)]
parser = do
    concat <$> sepEndBy1 (many1 star) newline

star :: Parser  (Coord, Char)
star = do
    pos <- getPosition
    let c = (sourceColumn pos, sourceLine pos)
    s <- char '.' <|> char '#'
    return (c, s)
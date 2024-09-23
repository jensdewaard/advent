{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE GADTs #-}
module Challenges.Y2016.Day21 (solutionA, solutionB, Perm (..), runPerm, invert) where
import Common.Prelude
import Common.List (swapElems, rotate, moveElem)
import Common.Parsing (int)
import Data.List (elemIndex, find, permutations)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec

solutionA :: String -> String
-- solutionA = solve parser (foldl (flip runOp) "abcdefgh")
solutionA = solve parser (flip runPerm "abcdefgh" . mconcat . map parsePerm )
solutionB :: String -> String
solutionB = solve parser (flip runPerm "fbgdceah" . invert . mconcat . map parsePerm)

parser :: Parser [Op Char]
parser = op `sepEndBy` newline

data Op a = SwapL a a -- ^ the letters X and Y should be swapped (regardless of where they appear in the string)
        | SwapP Int Int -- ^ the letters at indexes X and Y (counting from 0) should be swapped.
        | Rot Int -- ^ the whole string should be rotated; for example, one right rotation would turn abcd into dabc.
        | RotL a -- ^ the whole string should be rotated to the right based on the index of letter X
        | Rev Int Int -- ^ the span of letters at indexes X through Y (including the letters at X and Y) should be reversed in order.
        | Move Int Int -- ^ the letter which is at index X should be removed from the string, then inserted such that it ends up at index Y.
        deriving Show

op :: Parser (Op Char)
op = try swapP <|> try swapL <|> try rot <|> try rotL <|> try rev <|> try move

swapP :: Parser (Op Char)
swapP = do
    _ <- string "swap position "
    x <- int
    _ <- string " with position "
    SwapP x <$> int

swapL :: Parser (Op Char)
swapL = do
    _ <- string "swap letter "
    x <- letter
    _ <- string " with letter "
    SwapL x <$> letter

rot :: Parser (Op Char)
rot = do
    _ <- string "rotate "
    d <- parseDir
    _ <- string " "
    x <- int
    _ <- string " step"
    _ <- optional $ char 's'
    return $ Rot (d * x) where
        parseDir :: Parser Int
        parseDir = do
            try (string "left" >> return (-1)) <|> (string "right" >> return 1)

rotL :: Parser (Op Char)
rotL = do
    _ <- string "rotate based on position of letter "
    RotL <$> letter

rev :: Parser (Op Char)
rev = do
    _ <- string "reverse positions "
    x <- int
    _ <- string " through "
    Rev x <$> int

move :: Parser (Op Char)
move = do
    _ <- string "move position "
    x <- int
    _ <- string " to position "
    Move x <$> int

----

data Perm a where
  Perm :: [Int] -> Perm a
  PermF :: ([a] -> [a]) -> Perm a

runPerm :: Perm a -> [a] -> [a]
runPerm (Perm n) s = map (s !!) n
runPerm (PermF f) s = f s

parsePerm :: Eq a => Op a -> Perm a
parsePerm (SwapP x y)
    | x > y = parsePerm (SwapP y x)
parsePerm (SwapP x y) = Perm $ [0..x - 1] ++ [y] ++ [x+1..y-1] ++ [x] ++ [y+1 .. 7]
parsePerm (Rot n) = Perm $ rotate (negate n) [0..7]
parsePerm (RotL x) = PermF $ \s -> let
                        n = fromJust $ elemIndex x s
                        n' = if n >= 4 then n + 2 else n + 1
                                   in rotate (negate n') s
parsePerm (SwapL x y) = PermF $ \s -> let
        x' = fromJust $ elemIndex x s
        y' = fromJust $ elemIndex y s
                      in swapElems x' y' s
parsePerm (Rev x y) = Perm $ let
            s = [0..7]
            initial = take x s
            middle = take (y - x + 1) (drop x s)
            final = drop (y+1) s
            in initial ++ reverse middle ++ final
parsePerm (Move x y) = Perm $ moveElem x y [0..7]

instance Show (Perm a) where
  show (Perm n) = show n
  show (PermF _) = "<function>"

instance Semigroup (Perm a) where
  (<>) (Perm m) (Perm n) = Perm $ map (n !!) m
  (<>) (PermF f) (Perm n) = PermF $ \s -> map (f s !!) n
  (<>) (Perm n) (PermF f) = PermF $ \s -> f $ map (s !!) n
  (<>) (PermF f) (PermF g) = PermF $ \s -> (g . f) s

instance Monoid (Perm a) where
  mempty = Perm [0..7]

invert :: Eq a => Perm a -> Perm a
invert (Perm n) = Perm $ map (fromJust . (`elemIndex` n)) $ take (length n) [0..]
invert f@(PermF _) = PermF $ \s ->
    let n = length s in
--         fromJust $ find (==s) [runPerm (Perm g <> f) s | g <- permutations [0..n-1]]
        head [runPerm (Perm g) s | g <- permutations [0..n-1],
                                   runPerm (Perm g <> f) s == s]

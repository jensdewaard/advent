module Challenges.Y2024.Day09 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, digit, many, getPosition, sourceColumn, option)
import Control.Arrow ((>>>))
import Data.List (singleton)
import Control.Monad (join)
import Data.Char (intToDigit)
import Data.Maybe (isJust, fromJust)

solutionA :: String -> String
solutionA = solve parser (assignIDs >>> compactBlock >>> updatePos >>> checksum)
solutionB :: String -> String
solutionB = solve parser (assignIDs >>> compactFile >>> updatePos >>> checksum)

data Block = File { ident :: Int, size :: Int, position :: Int } | Free Int Int deriving (Eq)

instance Show Block where
    show (Free n _) = replicate n '.'
    show (File i s p) = "(" ++ show p ++ ":"++ replicate s (intToDigit i) ++ ")"

compactFile :: [Block] -> [Block]
compactFile [] = []
compactFile (Free 0 _:bls) = compactFile bls
compactFile (Free f pos:bls)
    | isJust (lastFileOfSize f bls) = let
        nb = fromJust $ lastFileOfSize f bls
        bls' = Free (f - size nb) pos : removeLastFileOfSize f bls
        in nb : compactFile bls'
    | otherwise = Free f pos : compactFile bls
compactFile (f:bls) = f : compactFile bls

compactBlock :: [Block] -> [Block]
compactBlock [] = []
compactBlock (Free 0 _:bls) = compactBlock bls
compactBlock (Free f pos:bls) = nb : compactBlock (Free (f-1) (pos+1):bls') where
    lb = lastFile bls
    nb = File (ident lb) 1 pos
    bls' = removeLastFile bls ++ [File (ident lb) (size lb - 1) (position lb) | size lb > 1]
compactBlock (f:bls) = f : compactBlock bls

updatePos :: [Block] -> [Block]
updatePos = go 0 where
    go _ []  = []
    go p (Free n _ : bs) = Free n p : go (p + n) bs
    go p (File i s _ : bs) = File i s p : go (p + s) bs

lastFile :: [Block] -> Block
lastFile = go . reverse where
    go [] = error "no more files"
    go (Free _ _:bs) = go bs
    go (b:_) = b

lastFileOfSize :: Int -> [Block] -> Maybe Block
lastFileOfSize n = go . reverse where
    go [] = Nothing
    go (Free _ _ : bs) = go bs
    go (f : bs ) = if size f <= n then Just f else go bs


removeLastFile :: [Block] -> [Block]
removeLastFile = reverse . go . reverse where
    go [] = error "no more files"
    go (Free _ _:bs) = go bs
    go (_:bs) = bs

removeLastFileOfSize :: Int -> [Block] -> [Block]
removeLastFileOfSize n = reverse . go . reverse where
    go [] = error "no more files"
    go (f@(File _ s p):bs) = if s <= n then (Free s p ) : bs else f : go bs
    go (f:bs) = f : go bs

checksum :: [Block] -> Int
checksum [] = 0
checksum (Free _ _: bs) = checksum bs
checksum (File i s p : bs) = sum (map (i *) [p..(p+s - 1)]) + checksum bs

assignIDs :: [Block] -> [Block]
assignIDs = go 0 where
    go _ [] = []
    go n (File _ s p:bs) = File n s p : go (n+1) bs
    go n (Free f p:bs) = Free f p : go n bs

parser :: Parser [Block]
parser = join <$> many (do
    fi <- file
    fr <- do
        p <- getPosition
        let s = sourceColumn p
        option (Free 0 s) free
    return [fi, fr]
    ) where
    file :: Parser Block
    file = do
        p <- getPosition
        s <- read . singleton <$> digit
        return $ File 0 s (sourceColumn p - 1)
    free :: Parser Block
    free = do
        p <- getPosition
        let s = sourceColumn p
        f <- read . singleton <$> digit
        return $ Free f s
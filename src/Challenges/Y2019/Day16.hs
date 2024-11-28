module Challenges.Y2019.Day16 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Control.Arrow ((>>>), Arrow ((&&&), first, second))
import Data.Char (digitToInt, intToDigit)

solutionA :: String -> String
solutionA = solve parser (iterate fft >>> (!!100) >>> take 8 >>> digitsToInt)
solutionB :: String -> String
solutionB = solve parser (take 7 &&& id
    >>> first digitsToInt
    >>> second (repeatN 10000 >>> iterate fastFft >>> (!!100))
    >>> uncurry drop
    >>> take 8
    >>> digitsToInt
    )

digitsToInt :: [Int] -> Int
digitsToInt = read  . map intToDigit



repeatN :: Int -> [a] -> [a]
repeatN 0 _ = []
repeatN n as = as ++ repeatN (n-1) as

parser :: Parser [Int]
parser = many1 (digitToInt <$> digit)

type FFT = [Int] -> [Int]
type Pattern = [Int]

base :: Pattern
base = cycle [0, 1, 0, -1]

elongate :: Int -> Pattern -> Pattern
elongate _ [] = []
elongate n (x:xs) = replicate n x ++ elongate n xs

applyPattern :: Int -> [Int] -> Int
applyPattern n = abs . sum . zipWith (*) (drop 1 $ elongate n base)

fastFft :: FFT
fastFft = scanr (\ n m -> (n + m) `mod` 10) 0

fft :: FFT
fft = zip [1..] >>> (\xs -> map (\(k,_) -> applyPattern k (map snd xs)) xs) >>> map (`mod` 10)
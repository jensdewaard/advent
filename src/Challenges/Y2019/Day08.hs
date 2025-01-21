module Challenges.Y2019.Day08 (solutionA, solutionB) where

import Common.Prelude
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = solve parser (\(Image ls) -> (\l -> occur 1 l * occur 2 l) $ minimumBy (compare `on` occur 0) ls)

solutionB :: String -> String
solutionB = solve parser (\(Image ls) -> showLayer 6 25 $ mconcat ls)

newtype Image = Image [Layer] deriving (Eq, Show)

newtype Layer = Layer [Pixel] deriving (Eq, Show)

newtype Pixel = Pixel Int deriving (Eq, Show)

instance Semigroup Pixel where
  (<>) :: Pixel -> Pixel -> Pixel
  (<>) (Pixel 2) (Pixel a) = Pixel a
  (<>) (Pixel n) (Pixel _) = Pixel n

instance Semigroup Layer where
  (<>) :: Layer -> Layer -> Layer
  (<>) (Layer p1) (Layer p2) = Layer $ zipWith (<>) p1 p2

instance Monoid Layer where
  mempty :: Layer
  mempty = Layer $ repeat $ Pixel 2
  mappend :: Layer -> Layer -> Layer
  mappend = (<>)
  mconcat :: [Layer] -> Layer
  mconcat [] = mempty
  mconcat (l : ls) = l <> mconcat ls

occur :: Int -> Layer -> Int
occur n (Layer px) = length $ filter (== Pixel n) px

showPixel :: Pixel -> Char
showPixel (Pixel 0) = '.'
showPixel (Pixel 1) = '#'
showPixel (Pixel d) = intToDigit d

showLayer :: Int -> Int -> Layer -> String
showLayer _ _ (Layer []) = ""
showLayer height width (Layer ps) =
  let line = map showPixel (take width ps) ++ ['\n']
   in line ++ showLayer height width (Layer $ drop width ps)

parser :: Parser Image
parser = Image <$> many1 layer
  where
    l = 6
    w = 25
    layer = Layer <$> count (l * w) pixel
    pixel = Pixel . digitToInt <$> digit

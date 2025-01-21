module Challenges.Y2023.Day15 (solutionA, solutionB) where

import Common.Parsing (symbol)
import Common.Prelude
import Data.Char (digitToInt, ord)
import Data.List (delete)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = solve parser (sum . map h)
  where
    h (Dash l) = hash' 0 (l ++ "-")
    h (Equals l p) = hash' 0 (l ++ "=" ++ show p)

solutionB :: String -> String
solutionB = solve parser (power . Map.toAscList . foldl store Map.empty)

power :: [(Int, [Lens])] -> Int
power [] = 0
power ((i, ls) : ts) = power ts + (i + 1) * sum (zipWith multLens [1 ..] ls)

multLens :: Int -> Lens -> Int
multLens i l@(Equals _ x) = i * x

store :: Map Int [Lens] -> Lens -> Map Int [Lens]
store m l@(Equals s _) =
  let h = hash' 0 s
      box = Map.findWithDefault [] h m
   in if l `elem` box
        then Map.insert h (replace l box) m
        else Map.insert h (box ++ [l]) m
store m l@(Dash s) =
  let h = hash' 0 s
      box = Map.findWithDefault [] h m
   in Map.insert (hash' 0 s) (delete l box) m

replace :: (Eq t) => t -> [t] -> [t]
replace _ [] = []
replace l (l' : ls) = if l == l' then l : ls else l' : replace l ls

data Lens = Dash String | Equals String Int deriving (Show)

instance Eq Lens where
  (Dash lbl) == (Dash lbl') = lbl == lbl'
  (Dash lbl) == (Equals lbl' _) = lbl == lbl'
  (Equals lbl _) == (Equals lbl' _) = lbl == lbl'
  (Equals lbl _) == (Dash lbl') = lbl == lbl'

hash' :: Int -> String -> Int
hash' current "" = current
hash' current (c : cs) =
  let c' = (current + ord c) * 17 `mod` 256
   in hash' c' cs

parser :: Parser [Lens]
parser = lens `sepBy1` char ','
  where
    lens :: Parser Lens
    lens = do
      lbl <- many1 letter
      c <- symbol
      strength <- optionMaybe digit
      case c of
        '-' -> return $ Dash lbl
        '=' -> return $ Equals lbl (digitToInt $ fromJust strength)
        _ -> error "parsing lens"

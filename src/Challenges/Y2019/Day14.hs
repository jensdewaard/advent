module Challenges.Y2019.Day14 (solutionA, solutionB) where
import Common.Prelude
import Common.Parsing (int)
import Common.List (aggregateOn, firstWhere)
import Text.ParserCombinators.Parsec
import Control.Arrow ((>>>))
import Data.List (find, sortBy)
import Data.Maybe (fromJust, isNothing)
import Data.Function (on)

solutionA :: String -> String
solutionA = solve parser (run [(1,IngredientName "FUEL")] )
-- 1091397 too low
solutionB :: String -> String
solutionB = solve parser (\rs -> sortBy (topoSort rs) $ ((1, IngredientName "ORE") :) $ map rhs rs)

newtype IngredientName = IngredientName String deriving (Eq, Show)

ingredientLTE :: [Recipe] -> IngredientName -> IngredientName -> Bool
ingredientLTE _ n1 n2
    | n1 == n2 = True
ingredientLTE recipes n1 n2 =
        let
            rcp1 = find ((==n1) . snd . rhs) recipes
        in case rcp1 of
            Just r -> any ((\i -> ingredientLTE recipes i n2) . snd) (lhs r)
            Nothing -> False


run :: [(Int, IngredientName)] -> [Recipe] -> [(Int, IngredientName)]
run [] _ = []
run needs recipes = let
    nextNeeded = firstWhere (\(amt, IngredientName name) -> name /= "ORE" && amt > 0) needs
    rest = filter (\x -> Just x /= nextNeeded) needs
    in if isNothing nextNeeded
        then needs
        else run (clean recipes $ rest ++ create recipes (fromJust nextNeeded)) recipes

clean :: Num a => [Recipe] -> [(a, IngredientName)] -> [(a, IngredientName)]
clean rcps = aggregateOn snd combine . sortBy (topoSort rcps)
    where combine (a1,n1) (a2,_) = (a1+a2,n1)

topoSort :: [Recipe] -> (a, IngredientName) -> (a, IngredientName) -> Ordering
topoSort rcps (_, n1) (_, n2)
    | n1 == n2 = EQ
    | ingredientLTE rcps n1 n2 = LT
    | otherwise = GT

onlyOres :: (Num a, Ord a) => [(a, IngredientName)] -> Bool
onlyOres = all (\i -> isOre i || isNeeded i) where
    isOre :: (a, IngredientName) -> Bool
    isOre (_, IngredientName s) = s == "ORE"
    isNeeded :: (Num a, Ord a) => (a, IngredientName) -> Bool
    isNeeded (n, _) = n <= 0

createIfNotOre :: [Recipe] -> (Int, IngredientName) -> [(Int, IngredientName)]
createIfNotOre _ (n, IngredientName "ORE") = [(n, IngredientName "ORE")]
createIfNotOre _ i@(n, _)
    | n <  0 = [i]
    | n == 0 = []
createIfNotOre recipes n = create recipes n

create :: [Recipe] -> (Int, IngredientName) -> [(Int, IngredientName)]
create recipes (n, ingredient) = let
    r = fromJust $ find ((==ingredient) . snd . rhs) recipes
    scale = n `div` fst (rhs r)
    remainder = n `mod` fst (rhs r)
    k = if remainder == 0 then scale else scale + 1
    in map (\(i,s) -> (i * k, s)) $ (negate remainder, ingredient) : lhs r


parser :: Parser [Recipe]
parser = recipe `sepEndBy1` newline
    where
        ingredient :: Parser (Int, IngredientName)
        ingredient = do
            amt <- int
            _ <- char ' '
            name <- many1 alphaNum
            return (amt, IngredientName name)
        recipe :: Parser Recipe
        recipe = do
            inputs <- ingredient `sepBy1` string ", "
            _ <- string " => "
            output <- ingredient
            return $ Recipe {
                lhs = inputs,
                rhs = output
            }


data Recipe = Recipe
    { lhs :: [(Int, IngredientName)]
    , rhs :: (Int, IngredientName)
    } deriving (Eq, Show)

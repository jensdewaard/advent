{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Common.Prelude where 
import Text.ParserCombinators.Parsec (Parser, parse)

solve :: Show b => Parser a -> (a -> b) -> String -> String
solve parser f = show . f . parse' parser

(==>) :: Show b => Parser a -> (a -> b) -> String -> String
p ==> f = solve p f

infix 6 ==>

parse' :: Parser a -> String -> a
parse' p i = case parse p "advent" i of
                  Left err -> error ("could not run parser " ++ show err)
                  Right val -> val

trd :: (a, b, c) -> c
trd (_,_,c) = c

fst3 :: (a, b, c) -> a
fst3 (a,_,_) = a

snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b

-- | Applies the function f each member of the list if predicate p holds, otherwise keeps 
--   the original value of a.
mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf _ _ [] = []
mapIf p f l = map (\x -> if p x then f x else x) l

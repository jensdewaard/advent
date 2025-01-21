{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Common.Prelude where

import Text.ParserCombinators.Parsec (Parser, parse)

class Showable s where
  show' :: s -> String

class HasLength a where
  len :: a -> Int

instance {-# OVERLAPS #-} Showable String where
  show' = id

instance (Show s) => Showable s where
  show' = show

solve :: (Showable b) => Parser a -> (a -> b) -> String -> String
solve parser f = show' . f . parse' parser
  where
    parse' :: Parser a -> String -> a
    parse' p i = case parse p "advent" i of
      Left err -> error ("could not run parser " ++ show err)
      Right val -> val

(==>) :: (Showable b) => Parser a -> (a -> b) -> String -> String
p ==> f = solve p f

infix 6 ==>

trd :: (a, b, c) -> c
trd (_, _, c) = c

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- | Applies the function f each member of the list if predicate p holds, otherwise keeps
--   the original value of a.
mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf _ _ [] = []
mapIf p f l = map (\x -> if p x then f x else x) l

type Predicate a = a -> Bool

class MonadOrd m where
  (>==) :: (Ord b) => m a -> (a -> m b) -> m b

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Common.FreqMap (FreqMap(FM, (:<|)), length, fromList, singleton, first, applyF) where

import Prelude hiding (length)

import Data.List (sort)
import Data.Bifunctor (second)
import Data.List.Extra (mconcatMap)

data FreqMap a where
  FM :: [(a, Int)] -> FreqMap a
  deriving (Eq, Show)

instance Functor FreqMap where
  fmap :: (a -> b) -> FreqMap a -> FreqMap b
  fmap f (FM kvps) = let
    ks = map fst kvps
    vs = map snd kvps
    in FM (zip (map f ks) vs)

instance Applicative FreqMap where
  pure :: a -> FreqMap a
  pure x = singleton x 1
  (<*>) :: FreqMap (a -> b) -> FreqMap a -> FreqMap b
  (<*>) = undefined

combine :: Ord a => FreqMap a -> FreqMap a -> FreqMap a
combine (FM s1) (FM s2) = let
    combine' [] b = b
    combine' a [] = a
    combine' ((ax,an):as) ((bx, bn):bs)
      | ax == bx = (ax, an+bn) : combine' as bs
      | ax < bx = (ax, an) : combine' as ((bx,bn):bs)
      | otherwise = (bx,bn) : combine' ((ax,an):as) bs

    in FM $ combine' (sort s1) (sort s2)

instance Ord a => Semigroup (FreqMap a) where
  (<>) :: FreqMap a -> FreqMap a -> FreqMap a
  (<>) = combine


instance Ord a => Monoid (FreqMap a) where
  mempty :: FreqMap a
  mempty = FM []

length :: FreqMap a -> Int
length (FM x) = sum $ map snd x

fromList :: Ord a => [a] -> FreqMap a
fromList = foldr ((<>) . pure) mempty

singleton :: a -> Int -> FreqMap a
singleton a n = FM [(a,n)]

first :: FreqMap a -> Maybe ((a,Int), FreqMap a)
first (FM []) = Nothing
first (FM (a:as)) = Just (a, FM as)

pattern (:<|) :: (a,Int) -> FreqMap a -> FreqMap a
pattern x :<| xs <- (first -> Just (x, xs))
infix 6 :<|

doF :: (a -> FreqMap a) -> a -> Int -> FreqMap a
doF f a n = let (FM as) = f a in FM (map (second (*n)) as)

applyF :: Ord a => (a -> FreqMap a) -> FreqMap a -> FreqMap a
applyF f (FM as) = mconcatMap (uncurry (doF f)) as
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Common.FreqMap (FreqMap(FM), fromList, singleton, first, applyF) where

import Data.List (sort)
import Data.Bifunctor (first, second)
import Data.List.Extra (mconcatMap)
import Common.Prelude (HasLength (..), MonadOrd ((>==)))
import Control.Applicative (asum)
import Control.Monad (join)
import Data.Tuple (swap)

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

fromList :: Ord a => [a] -> FreqMap a
fromList = foldr ((<>) . pure) mempty

singleton :: a -> Int -> FreqMap a
singleton a n = FM [(a,n)]

doF :: (a -> FreqMap b) -> a -> Int -> FreqMap b
doF f a n = let (FM as) = f a in FM (map (second (*n)) as)

applyF :: Ord b => (a -> FreqMap b) -> FreqMap a -> FreqMap b
applyF f (FM as) = let fs = map (uncurry (doF f)) as in
  undefined

instance HasLength (FreqMap a) where
  len (FM x) = sum $ map snd x

instance Foldable FreqMap where

instance Traversable FreqMap where
  traverse :: Applicative f => (a -> f b) -> FreqMap a -> f (FreqMap b)
  traverse f (FM as) = let
    fas = map (first f) as
    in sequenceA (FM fas)

scale :: Int -> FreqMap a -> FreqMap a
scale n (FM as) = FM (map (second (*n)) as)

joinFM :: Ord a => FreqMap (FreqMap a) -> FreqMap a
joinFM (FM fms) = let
  fms' = map (uncurry scale . swap) fms
  in
    mconcat fms'

instance MonadOrd FreqMap where
  (>==) :: Ord b => FreqMap a -> (a -> FreqMap b) -> FreqMap b
  (>==) ma k = joinFM $ fmap k ma
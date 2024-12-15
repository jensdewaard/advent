{-# LANGUAGE TemplateHaskell #-}
module Common.CoordTests (runPropertyTests) where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Data.Proxy (Proxy (..))
import Common.Coord (Coord)

coord :: Proxy Coord
coord = Proxy

laws :: (Ord a, Arbitrary a, Show a, Num a) => Proxy a -> [Laws]
laws p = [eqLaws p, ordLaws p, numLaws p]
--------------------------
return []
runPropertyTests :: IO Bool
runPropertyTests = do
    lawsCheckMany [("Coord", laws coord)]
    $quickCheckAll
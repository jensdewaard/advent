{-# Language ViewPatterns #-}
module Challenges.Y2015.Day12 (solutionA, solutionB) where

import Data.Aeson (Value (Null, Number, Object, Array, String, Bool))
import Data.Aeson.Decoding (decode)
import Data.Aeson.KeyMap (elems)
import Data.Scientific (Scientific, coefficient)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe (fromJust)

solutionA :: String -> String
solutionA = show . foldValue 0 . decodeJson

decodeJson :: String -> Value
decodeJson = fromJust . decode . TLE.encodeUtf8 . TL.pack

toUnboundedInteger :: Scientific -> Int
toUnboundedInteger = fromInteger . coefficient

foldValue :: Int -> Value -> Int
foldValue n (Number s) = n + toUnboundedInteger s
foldValue n (Object km) = foldl foldValue n km
foldValue n (Array es) = foldl foldValue n es
foldValue n (String _) = n
foldValue n (Bool _) = n
foldValue n Null = n

foldValueB :: Int -> Value -> Int
foldValueB n (Object km) = if any isRed $ elems km then n else foldl foldValueB n km
foldValueB n (Array es) = foldl foldValueB n es
foldValueB n j = foldValue n j


isRed :: Value -> Bool
isRed (show -> "red") = True
isRed _ = False

solutionB :: String -> String
solutionB = show . foldValueB 0 . decodeJson



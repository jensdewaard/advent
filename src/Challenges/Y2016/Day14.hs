module Challenges.Y2016.Day14 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import qualified    Data.ByteString.Char8 as Bytes
import Crypto.Hash
import              Data.ByteArray.Encoding

solutionA :: String -> String
solutionA = solve parser (\salt -> iterate (key 0 salt) 0 !! 64)
solutionB :: String -> String
solutionB = solve parser (\salt -> iterate (key 2016 salt) 0 !! 64)

parser :: Parser String
parser = many1 alphaNum

key :: Int -> String -> Int -> Int
key stretch salt n = let k' salt' n' = if isKey stretch salt' n' == Just True then n' else k' salt' (n'+1) in
    k' salt (n+1)

containsThree :: String -> Maybe Char
containsThree [] = Nothing
containsThree [_] = Nothing
containsThree [_,_] = Nothing
containsThree (a:b:c:ds) = if a == b && b == c 
                              then Just a
                              else containsThree (b:c:ds)

md5Hash :: Int -> String -> Int -> String
md5Hash strength door i =
  Bytes.unpack $ iterate erg (Bytes.pack (door <> show i)) !! (strength + 1)
  where erg = convertToBase Base16 . hashWith MD5

containsFiveOf :: Int -> Char -> String -> Int -> Bool
containsFiveOf n c salt index = 
    any (containsFive . md5Hash n salt) [index+1..index+1000] where
    containsFive :: String -> Bool
    containsFive s 
        | length s < 5 = False
        | otherwise = all (==c) (take 5 s) || containsFive (tail s)

isKey :: Int -> String -> Int -> Maybe Bool
isKey n salt index = do
    c <- containsThree (md5Hash n salt index)
    return $ containsFiveOf n c salt index

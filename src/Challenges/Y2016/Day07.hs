module Challenges.Y2016.Day07 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Data.List (isInfixOf)

solutionA :: String -> String
solutionA = solve parser (length . filter supportsTLS)
solutionB :: String -> String
solutionB = solve parser (length . filter supportsSSL)

supportsTLS :: Address -> Bool
supportsTLS (super,hyper) = any hasABBA super && not (any hasABBA hyper)

supportsSSL :: Address -> Bool
supportsSSL (super, hyper) = any (any (\a -> bab a `anySub` hyper) . abas) super

anySub :: String -> [String] -> Bool
anySub s = any (isInfixOf s)

abas :: String -> [String]
abas (a:b:c:as) = if a /= b && a == c && b /= c
    then (a : b : a : "") : abas (b:c:as) else abas (b:c:as)
abas _ = []

hasABBA :: Eq a => [a] -> Bool
hasABBA (a:b:b':a':cs) = (a == a' && a /= b && b == b') || hasABBA (b:b':a':cs)
hasABBA _ = False

type Address = ([String], [String])

bab :: String -> [Char]
bab (a:b:_) = b:a:b:""
bab _ = error "unable to compute bab of string"

parser :: Parser [Address]
parser = address `sepEndBy` newline where
    address :: Parser Address
    address = do
        a <- many1 letter
        as <- many1 (do
            b <- between (char '[') (char ']') (many1 letter)
            c <- many1 letter
            return (b,c)
            )
        return (a : map snd as ,map fst as)
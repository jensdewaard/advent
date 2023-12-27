module Challenges.Y2015.Day07 (solutionA, solutionB) where

import Control.Monad.State
import Data.Bits ((.&.), (.|.), shift, complement)
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word16)
import Text.ParserCombinators.Parsec hiding (State, getInput)
import Common.Prelude

type Wire = String
type Input = Either Wire Word16
type Circuit = Map Wire (Either Gate Word16)

data Gate = And Input Input
    | Or Input Input
    | LShift Input Int
    | Not Input
    | Const Input
    deriving (Eq, Show)

parser :: Parser [(Gate, Wire)]
parser = endBy1 parseLine newline

solutionA :: String -> String
solutionA = solve parser (show . getA .  mkCircuit)

solutionB :: String -> String
solutionB = solve parser (show . solveB . mkCircuit)

solveB :: Circuit -> Word16
solveB c = let
    a' = getA c
    c' = Map.update (\_ -> Just $ Right a') "b" c
    in getA c'

getA :: Circuit -> Word16
getA = evalState (getSignal "a")

mkCircuit :: [(Gate, Wire)] -> Circuit
mkCircuit = mkCircuit' Map.empty where
    mkCircuit' m [] = m
    mkCircuit' m ((g,w):gs) = mkCircuit' (Map.insert w (Left g) m) gs

getSignal :: Wire -> State Circuit Word16
getSignal w = do
    c <- get
    case Map.lookup w c of
      Nothing -> error ("could not find wire with label " ++ show w)
      Just (Right val) -> return val
      Just (Left g) -> case g of
                          And x y -> do
                              x' <- getInput x
                              y' <- getInput y
                              let z = (.&.) x' y'
                              m <- get
                              put $ Map.update (\_ -> Just $ Right z) w m
                              return z
                          Or x y -> do
                              x' <- getInput x
                              y' <- getInput y
                              let z = (.|.) x' y'
                              m <- get
                              put $ Map.update (\_ -> Just $ Right z) w m
                              return z
                          Not x -> do
                              x' <- getInput x
                              let z = complement x'
                              m <- get
                              put $ Map.update (\_ -> Just $ Right z) w m
                              return z
                          LShift x n -> do
                              x' <- getInput x
                              let z = shift x' n
                              m <- get
                              put $ Map.update (\_ -> Just $ Right z) w m
                              return z
                          Const x -> do
                              x' <- getInput x
                              m <- get
                              put $ Map.update (\_ -> Just $ Right x') w m
                              return x'

getInput :: Either Wire Word16 -> State Circuit Word16
getInput (Right val) = return val
getInput (Left w) = do
    x <- getSignal w
    m <- get
    put $ Map.update (\_ -> Just $ Right x) w m
    return x

parseLine :: Parser (Gate, Wire)
parseLine = do
    g <- try parseAndGate
        <|> try parseOrGate
        <|> try parseLShift
        <|> try parseRShift
        <|> try parseNotGate
        <|> try parseConst
    _ <- string " -> "
    w <- many1 alphaNum
    return (g,w)

parseConst :: Parser Gate
parseConst = do
    n <- many1 alphaNum
    return (Const $ mkInput n)

parseAndGate :: Parser Gate
parseAndGate = do
    i1 <- many1 alphaNum
    _ <- string " AND "
    i2 <- many1 alphaNum
    return (And (mkInput i1) (mkInput i2))

parseNotGate :: Parser Gate
parseNotGate = do
    _ <- string "NOT "
    i <- many1 alphaNum
    return (Not (mkInput i))

parseOrGate :: Parser Gate
parseOrGate = do
    i1 <- many1 alphaNum
    _ <- string " OR "
    i2 <- many1 alphaNum
    return (Or (mkInput i1) (mkInput i2))

parseLShift :: Parser Gate
parseLShift = do
    i <- many1 alphaNum
    _ <- string " LSHIFT "
    n <- read <$> many1 digit
    return (LShift (mkInput i) n)

parseRShift :: Parser Gate
parseRShift = do
    i <- many1 alphaNum
    _ <- string " RSHIFT "
    n <- read <$> many1 digit
    return (LShift (mkInput i) (negate n))

mkInput :: String -> Input
mkInput s = if all isDigit s then Right $ read s else Left s

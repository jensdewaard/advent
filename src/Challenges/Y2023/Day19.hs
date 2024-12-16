{-# LANGUAGE FlexibleInstances #-}
module Challenges.Y2023.Day19 (solutionA, solutionB) where

import Common.Prelude (solve)
import Data.Map (Map, fromList)
import qualified Data.Map as M
import Common.Parsing (int)
import Text.ParserCombinators.Parsec
import Control.Applicative (asum)
import Control.Arrow ((>>>))
import Common.List (sumWith)

solutionA :: String -> String
solutionA = solve parser (uncurry accepted >>> sumWith total)
solutionB :: String -> String
solutionB = solve parser (const "")

type Workflow = (String, [Rule])
data Attribute = X | M | A | S deriving (Eq, Show)
type WorkflowName = String
data Rule = Rule Attribute Ordering Int WorkflowName | Unconditional WorkflowName deriving (Eq, Show)

accepted :: Map String Workflow -> [Part Int] -> [Part Int]
accepted wfs = filter (\p -> evaluateWorkflowStart wfs p == "A")

evaluateWorkflowStart :: Map String Workflow -> Part Int -> String
evaluateWorkflowStart ws p = let
    workflowIn = M.lookup "in" ws
    in case workflowIn of
        Just w -> evaluateWorkflow ws w p
        Nothing -> error "no workflow by name of 'in'"

evaluateRule :: Part Int -> Rule -> Maybe WorkflowName
evaluateRule _ (Unconditional wf) = Just wf
evaluateRule p (Rule attr GT v wf) = if get attr p > v then Just wf else Nothing
evaluateRule p (Rule attr LT v wf) = if get attr p < v then Just wf else Nothing
evaluateRule _ (Rule _ EQ _ _) = error "invalid comparison"

evaluateWorkflow :: Map String Workflow -> Workflow -> Part Int -> String
evaluateWorkflow ws (_, rs) p = let
    res = asum $ map (evaluateRule p) rs
    in case res of
        Nothing -> error ("could not evaluate part " ++ show p ++ " on rules " ++ show rs)
        Just "A" -> "A"
        Just "R" -> "R"
        Just ans -> let w = M.lookup ans ws in
            case w of
                Just w' -> evaluateWorkflow ws w' p
                Nothing -> error ("evaluated to non-existing workflow " ++ show w)

get :: Attribute -> Part a -> a
get X = x
get M = m
get A = a
get S = s

data Part a = Part
    {   x :: a
    ,   m :: a
    ,   a :: a
    ,   s :: a
    } deriving (Eq, Show)

total :: Num a => Part a -> a
total p = x p + m p + a p + s p

parser :: Parser (Map String Workflow,[Part Int])
parser = do
    ws <- workflow `sepEndBy1` newline
    _ <- newline
    is <- item `sepEndBy1` newline
    return (fromList ws, is)

workflow :: Parser (String, Workflow)
workflow = do
    lbl <- many1 letter
    _ <- char '{'
    rs <- rule `sepBy1` char ','
    _ <- char '}'
    return (lbl, (lbl, rs))

rule :: Parser Rule
rule = try (do
    attr <- (char 'x' >> return X) <|> (char 'm' >> return M)
        <|> (char 'a' >> return A) <|> (char 's' >> return S)
    comp <- (char '<' >> return LT) <|> (char '>' >> return GT)
    value <- int
    _ <- char ':'
    lbl <- many1 alphaNum
    return $ Rule attr comp value lbl)
    <|> (do lbl <- many1 letter ; return (Unconditional lbl))

item :: Parser (Part Int)
item = do
    _ <- string "{x="
    xval <- int
    _ <- string ",m="
    mval <- int
    _ <- string ",a="
    aval <- int
    _ <- string ",s="
    sval <- int
    _ <- char '}'
    return $ Part xval mval aval sval



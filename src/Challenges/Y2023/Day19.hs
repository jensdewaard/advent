{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Challenges.Y2023.Day19 (solutionA, solutionB) where

import Common.Interval (Interval (..), fromPair, intersection, isIn)
import qualified Common.Interval as I
import Common.List (sumWith)
import Common.Parsing (int)
import Common.Prelude (solve)
import Control.Applicative (asum)
import Control.Arrow ((>>>))
import Data.Map (Map, fromList)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = solve parser (uncurry accepted >>> sumWith total)

solutionB :: String -> String
-- 11716444663469867 is too high
solutionB = solve parser (fst >>> countWorkflowStart)

type Workflow = (String, [Rule])

data Attribute = X | M | A | S deriving (Eq, Show, Ord)

type WorkflowName = String

data Rule
  = Rule Attribute (Interval Int) WorkflowName
  | Unconditional WorkflowName
  deriving (Eq, Show)

accepted :: Map String Workflow -> [Part Int] -> [Part Int]
accepted wfs = filter (\p -> evaluateWorkflowStart wfs p == "A")

evaluateWorkflowStart :: Map String Workflow -> Part Int -> String
evaluateWorkflowStart ws p =
  let workflowIn = M.lookup "in" ws
   in case workflowIn of
        Just w -> evaluateWorkflow ws w p
        Nothing -> error "no workflow by name of 'in'"

countWorkflowStart :: Map String Workflow -> Int
countWorkflowStart ws =
  let ws' = M.insert "A" ("A", []) $ M.insert "R" ("R", []) ws
      workflowIn = M.lookup "hdj" ws'
   in case workflowIn of
        Just w ->
          countPossibilities
            w
            ws'
            ( M.fromList
                [ (X, Interval 1 4000),
                  (M, Interval 1 4000),
                  (A, Interval 1 4000),
                  (S, Interval 1 4000)
                ]
            )
        Nothing -> error "no workflow by name of 'in'"

evaluateRule :: Part Int -> Rule -> Maybe WorkflowName
evaluateRule _ (Unconditional wf) = Just wf
evaluateRule p (Rule attr range wf) = if get attr p `isIn` range then Just wf else Nothing

evaluateWorkflow :: Map String Workflow -> Workflow -> Part Int -> String
evaluateWorkflow ws (_, rs) p =
  let res = asum $ map (evaluateRule p) rs
   in case res of
        Nothing -> error ("could not evaluate part " ++ show p ++ " on rules " ++ show rs)
        Just "A" -> "A"
        Just "R" -> "R"
        Just ans ->
          let w = M.lookup ans ws
           in case w of
                Just w' -> evaluateWorkflow ws w' p
                Nothing -> error ("evaluated to non-existing workflow " ++ show w)

nextWorkflow :: Rule -> WorkflowName
nextWorkflow (Unconditional n) = n
nextWorkflow (Rule _ _ n) = n

countPossibilities :: Workflow -> Map String Workflow -> Map Attribute (Interval Int) -> Int
countPossibilities (fst -> "A") _ rs = product $ map (I.length . snd) $ M.toList rs
countPossibilities (fst -> "R") _ _ = 0
countPossibilities (_, rules) ws ranges =
  let nextWorkflows = map (\r -> (r, fromJust $ (`M.lookup` ws) $ nextWorkflow r)) rules
   in sumWith
        ( \(r, w) -> case r of
            Unconditional _ -> countPossibilities w ws ranges
            Rule attr i _ ->
              let ranges' = M.update (\j -> Just (i `intersection` j)) attr ranges
               in countPossibilities w ws ranges'
        )
        nextWorkflows

get :: Attribute -> Part a -> a
get X = x
get M = m
get A = a
get S = s

data Part a = Part
  { x :: a,
    m :: a,
    a :: a,
    s :: a
  }
  deriving (Eq, Show)

total :: (Num a) => Part a -> a
total p = x p + m p + a p + s p

parser :: Parser (Map String Workflow, [Part Int])
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
rule =
  try
    ( do
        attr <-
          (char 'x' >> return X)
            <|> (char 'm' >> return M)
            <|> (char 'a' >> return A)
            <|> (char 's' >> return S)
        comp <- (char '<' >> return LT) <|> (char '>' >> return GT)
        value <- int
        _ <- char ':'
        lbl <- many1 alphaNum
        return
          ( case comp of
              GT -> Rule attr (fromPair (value + 1, 4000)) lbl
              LT -> Rule attr (fromPair (1, value - 1)) lbl
              EQ -> error "invalid comparison"
          )
    )
    <|> (do lbl <- many1 letter; return (Unconditional lbl))

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

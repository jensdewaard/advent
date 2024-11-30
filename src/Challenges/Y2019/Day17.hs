module Challenges.Y2019.Day17 (solutionA, solutionB) where
import Common.Prelude
import Common.Parsing (grid)
import Common.Coord (Coord, cardinal, showMap)
import Text.ParserCombinators.Parsec (Parser, parse, letter, char)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (chr, ord)
import Intcode (parseProgram, ProgState (outputs), runInterpreter, mkProgram, mkProgramWithInput, opReplace)
import Control.Arrow ((>>>))
import Control.Applicative ((<|>))

solutionA :: String -> String
solutionA = solve parseProgram (mkProgram
    >>> runInterpreter >>> outputs
    >>> runParser
    >>> (\m -> M.filterWithKey (\ k _ -> isIntersection m k ) m)
    >>> M.toList
    >>> (map fst >>> map (\(x,y) -> (x-1) * (y-1)) >>> sum)
    )

solutionB :: String -> String
solutionB = solve parseProgram (opReplace 0 2
    >>> mkProgramWithInput encodeInput
    >>> runInterpreter >>> outputs
    >>> last
    )

data Command = F Int | L | R | A | B | C

instance Show Command where
  show :: Command -> String
  show A = "A"
  show B = "B"
  show C = "C"
  show (F n) = show n
  show L = "L"
  show R = "R"
  showList :: [Command] -> ShowS
  showList [] s = s
  showList [c] s = show c ++ s
  showList (c:cs) s = show c ++ "," ++ showList cs s

encodeInput :: [Int]
encodeInput = map ord $ unlines [show routine, show funcA, show funcB, show funcC, "n", ""]

routine :: [Command]
routine = [A,A,B,C,B,C,B,C,C,A]
funcA :: [Command]
funcA = [L,F 10,R,F 8,R,F 8]
funcB :: [Command]
funcB = [L,F 10,L,F 12,R,F 8,R,F 10]
funcC :: [Command]
funcC = [R,F 10,L,F 12,R,F 10]

runParser :: [Int] -> Map Coord Char
runParser = parse' parser . map chr where
    parse' :: Parser a -> String -> a
    parse' p i = case parse p "advent" i of
                    Left err -> error ("could not run parser " ++ show err)
                    Right val -> val

parser :: Parser (Map Coord Char)
parser = grid (letter <|> char '.' <|> char '#' <|> char '^' <|> char '<' <|> char '>')

isIntersection :: Map Coord Char -> Coord -> Bool
isIntersection m c = let
    ns = cardinal c
    c' = M.findWithDefault '.' c m
    cs = map (\x -> M.findWithDefault '.' x m) ns
    in all (=='#') (c':cs)
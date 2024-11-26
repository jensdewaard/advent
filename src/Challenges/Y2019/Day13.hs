 {-# LANGUAGE UnicodeSyntax #-}
module Challenges.Y2019.Day13 (solutionA, solutionB) where
import Common.Prelude
import Intcode (runInterpreter, ProgState (outputs), parseProgram, mkProgram, opReplace, runInterpreterUntilOutput, clearOutputs, isFinished, setInput)
import Control.Arrow ((>>>))
import Common.Coord (Coord)
import Data.Map (Map, union, singleton, empty)
import qualified Data.Map as M
import Common.List (chunksOf)
import Control.Monad.State (State, get, MonadState (put), execState)

solutionA :: String -> String
solutionA = solve parseProgram (mkProgram
    >>> runInterpreter
    >>> outputs
    >>> chunksOf 3
    >>> map mkDI
    >>> mconcat
    >>> blocks
    >>> length
    )

solutionB :: String -> String
solutionB = solve parseProgram (opReplace 0 2
    >>> mkProgram
    >>> playGame
    >>> score
    -- >>> \st -> st { cpu = setInput [head $ inputs $ cpu st] $ cpu st }
    )

data GameState = GameState
    {   cpu :: ProgState
    ,   score :: Int
    ,   paddle :: Int
    ,   ball   :: Int
    } deriving (Eq, Show)

playGame :: ProgState -> GameState
playGame p = execState runM (GameState p 0 0 0)

runM :: State GameState Int
runM = do
    st <- get
    let cpu1 = runInterpreterUntilOutput $ cpu st
    if isFinished cpu1 
        then return $ score st 
        else do
            let cpu' = runInterpreterUntilOutput $ runInterpreterUntilOutput $ runInterpreterUntilOutput $ cpu st
            let [a,b,c] = take 3 $ outputs cpu'
            let st' = (case c of
                    3 -> st { cpu = clearOutputs cpu', paddle = a }
                    4 -> st { cpu = clearOutputs cpu', ball = a }
                    _ -> if a == -1 && b == 0
                            then st { cpu = clearOutputs cpu', score = c }
                            else st { cpu = clearOutputs cpu' }
                    )
            let joystick
                    | ball st' < paddle st' = -1
                    | ball st' > paddle st' = 1
                    | otherwise = 0
            put $ st' { cpu = setInput (repeat joystick) $ cpu st' }
            runM

newtype DrawInstruction = DrawInstruction (Map Coord Int) deriving (Eq, Show)

blocks :: DrawInstruction -> [Coord]
blocks (DrawInstruction m) = map fst $ M.toList $ M.filter (==2) m

mkDI :: [Int] -> DrawInstruction
mkDI [x, y, d] = DrawInstruction $ singleton (x,y) d
mkDI x = error ("not a chunk of three : " ++ show x)

instance Semigroup DrawInstruction where
  (<>) :: DrawInstruction -> DrawInstruction -> DrawInstruction
  (<>) (DrawInstruction m1) (DrawInstruction m2) =
    DrawInstruction $ union m2 m1

instance Monoid DrawInstruction where
  mempty :: DrawInstruction
  mempty = DrawInstruction empty
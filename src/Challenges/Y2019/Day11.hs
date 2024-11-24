module Challenges.Y2019.Day11 (solutionA, solutionB) where
import Common.Prelude
import Intcode (ProgState (outputs), parseProgram, OpProgram, mkProgram, runInterpreterUntilOutput, isFinished, setInput, clearInputs, clearOutputs)
import Data.Map (Map)
import qualified Data.Map as M
import Common.Coord (Coord, Dir(..), turnLeft, turnRight, move, showMapWith)
import Control.Arrow ((>>>))

solutionA :: String -> String
solutionA = solve parseProgram (initRobot >>> run >>> world >>> length)
-- 1274 is too low
solutionB :: String -> String
solutionB = solve parseProgram (initRobotB >>> run >>> world >>> M.toList >>> showMapWith showColor)

initRobot :: OpProgram -> RobotState
initRobot p = RobotState (mkProgram p) (0,0) U M.empty

initRobotB :: OpProgram -> RobotState
initRobotB p = RobotState (mkProgram p) (0,0) U $ M.singleton (0,0) 1

run :: RobotState -> RobotState
run x = if isFinished (brain x) 
    then x 
    else run $ runRobot x

runRobot :: RobotState -> RobotState
runRobot rs@(RobotState b p f w) = if isFinished b then rs else 
    let
        currentColor = M.findWithDefault 0 p w
        brain' = setInput (repeat currentColor) b
        brain'' = runInterpreterUntilOutput brain'
        newColor = if isFinished brain'' then currentColor else determineColor $ outputs brain''
        brain''' = runInterpreterUntilOutput (clearOutputs brain'')
        newDir = determineTurn $ outputs brain'''
        facing' = turn newDir f
        world' = paint p newColor w
    in RobotState (clearInputs $ clearOutputs brain''') (move facing' p) facing' world'

determineColor :: [Int] -> Int
determineColor [] = error "cannot get color from empty output"
determineColor (0:_) = 0
determineColor (1:_) = 1
determineColor x = error $ "unknown color : " ++ show x

showColor :: Int -> Char
showColor 1 = '#'
showColor _ = '.'

determineTurn :: [Int] -> Maybe Dir
determineTurn [] = Nothing
determineTurn (0:_) = Just L
determineTurn (1:_) = Just R
determineTurn x = error $ "unknown turn : " ++ show x

paint :: Coord -> Int -> Map Coord Int -> Map Coord Int
paint p c = M.alter (const $ Just c) p

turn :: Maybe Dir -> Dir -> Dir
turn (Just L) = turnLeft
turn (Just R) = turnRight
turn Nothing = id
turn _ = error "unreachable"

data RobotState = RobotState
    {   brain :: ProgState
    ,   position :: Coord
    ,   facing :: Dir
    ,   world :: Map Coord Int
    } deriving (Eq, Show)
data Color = Black | White deriving (Eq, Show)
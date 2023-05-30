{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Solution where
import qualified Challenges.Y2015 as Y2015
import qualified Challenges.Y2019 as Y2019
import qualified Challenges.Y2022 as Y2022

type Sol = (IO String, String -> String, String -> String)

getSol :: (Bool, Int, Int) -> IO Sol
getSol (test, y, d) = return $ getYear y test d

runSol :: Sol -> IO String
runSol (inputSource, solveA, solveB) = do
    input <- inputSource
    return $ concat ["Solution A:\t", solveA input, "\nSolution B:\t", solveB input, "\n"]

getYear :: Int -> Bool -> Int -> Sol
getYear 2015 = Y2015.getDay
getYear 2019 = Y2019.getDay
getYear 2022 = Y2022.getDay
getYear _ = error "unsupported year"

import qualified Common.ListTests as List
import qualified Common.IntervalTests as Interval
import System.Exit (exitSuccess, exitFailure)
import qualified Common.CoordTests as Coord

main :: IO ()
main = do
    good <- and <$> sequence [
        Coord.runPropertyTests,
        List.runTests,
        Interval.runTests
        ]
    if good
        then exitSuccess
        else exitFailure
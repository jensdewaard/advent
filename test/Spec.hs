import qualified Common.ListTests as List
import qualified Common.IntervalTests as Interval
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
    good <- and <$> sequence [
        List.runTests,
        Interval.runTests
        ]
    if good
        then exitSuccess
        else exitFailure
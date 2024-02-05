import qualified Common.CharTests as Char
import qualified Common.IntervalTests as Interval
import qualified FloydWarshallTests as FW
import qualified Y2019Tests.Day01 as Y19D1
import qualified Y2019Tests.Day04 as Y19D4
import Test.HUnit (runTestTT, errors, failures, Test (TestList))
import System.Exit (exitSuccess, exitFailure)
main :: IO ()
main = do
    counts <- runTestTT (TestList [
        Char.tests,
        Interval.tests,
        FW.tests,
        Y19D1.tests,
        Y19D4.tests
        ])
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
import qualified Common.CharTests as Char
import qualified Common.MathTests as Math
import qualified Common.IntervalTests as Interval
import qualified Common.ListTests as List
import qualified FloydWarshallTests as FW
import qualified Y2019Tests.Day01 as Y19D1
import qualified Y2019Tests.Day04 as Y19D4
import qualified Y2016Tests.Day21 as Y16D21
import Test.HUnit (runTestTT, errors, failures, Test (TestList))
import System.Exit (exitSuccess, exitFailure)
main :: IO ()
main = do
    counts <- runTestTT (TestList [
        Char.tests,
        Interval.tests,
        Math.tests,
        FW.tests,
        Y19D1.tests,
        Y19D4.tests,
        Y16D21.tests,
        List.tests
        ])
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure

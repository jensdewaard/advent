import qualified FloydWarshallTests as FW
import qualified Y2019Tests.Day01 as Y19D1
import qualified Y2019Tests.Day04 as Y19D4
import qualified SharedTests as Shared
import Test.HUnit (runTestTT, errors, failures, Test (TestList))
import System.Exit (exitSuccess, exitFailure)
main :: IO ()
main = do
    counts <- runTestTT (TestList [
        FW.tests,
        Y19D1.tests,
        Y19D4.tests,
        Shared.tests
        ])
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
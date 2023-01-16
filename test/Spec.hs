import FloydWarshallTests (tests)
import Test.HUnit (runTestTT, errors, failures)
import System.Exit (exitSuccess, exitFailure)
main :: IO ()
main = do
    counts <- runTestTT tests
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure

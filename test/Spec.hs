import qualified Common.ListTests as List
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
    good <- and <$> sequence [List.runTests]
    if good
        then exitSuccess
        else exitFailure
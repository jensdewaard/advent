import qualified Common.CharTests as Char
import qualified Common.MathTests as Math
import qualified Common.IntervalTests as Interval
import qualified Common.ListTests as List
import qualified FloydWarshallTests as FW
import qualified Y2019Tests.Day01 as Y19D1
import qualified Y2019Tests.Day04 as Y19D4
import qualified Y2016Tests.Day21 as Y16D21
import qualified Y2024Tests.Day05 as Y24D05
import Test.HUnit (runTestTT, errors, failures, Test (TestList))
import System.Exit (exitSuccess, exitFailure)
import Test.QuickCheck
import Data.List (intersperse)

-- import Test.Framework (defaultMain, testGroup)
-- import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Common.ListTests 

main = quickCheck prop_swapelems_comm
-- /show
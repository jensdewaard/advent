module FloydWarshallTests (tests) where

import FloydWarshall (Weight (Inf, Weight), floydwarshall)
import Test.HUnit

testPos :: Test
testPos = "signum law 9" ~: ((signum (Weight 9) * abs (Weight 9)) ~=? (Weight 9))

testNeg :: Test
testNeg = "for abs -3" ~: (signum (Weight (-3)) * abs (Weight (-3))) ~=? (Weight (-3))

-- example from wikipedia for testing
wikiInput :: [[Weight]]
wikiInput = [[Weight 0, Inf, Weight (-2), Inf],
        [Weight 4, Weight 0, Weight 3, Inf],
        [Inf, Inf, Weight 0, Weight 2],
        [Inf, Weight (-1), Inf, Weight 0]]

wikiSol :: [[Weight]]
wikiSol = [[Weight 0, Weight (-1), Weight (-2), Weight 0],
        [Weight 4, Weight 0, Weight 2, Weight 4],
        [Weight 5, Weight 1, Weight 0, Weight 2],
        [Weight 3, Weight (-1), Weight 1, Weight 0]]

testWiki :: Test
testWiki = "for wikipedia graph" ~: wikiSol ~=? (floydwarshall wikiInput)

tests :: Test
tests = TestList [ 
    TestLabel "positive" testPos,
    TestLabel "negative" testNeg,
    TestLabel "wikipedia" testWiki
    ]
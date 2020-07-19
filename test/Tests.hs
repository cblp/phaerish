import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

import           Sqeq             (solveSquare)

main :: IO ()
main =
  defaultMain $
    testGroup "solveSquare"
      [ testCase "x² + 2x + 1 = 0 -> [-1, -1]" $
          solveSquare 1 2 1 @?= [-1, -1]
      , testCase "x² - 2x + 1 = 0 -> [1, 1]" $
          solveSquare 1 (-2) 1 @?= [1, 1]
      ]

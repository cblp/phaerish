import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

import           Sqeq             (solveSquare)
import           ListLazy         (replicate', zip', enumerate', zipWith', zipLongest, Longest(..))

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [solveSquareTests, listLazyTests]

solveSquareTests = testGroup "solveSquare"
  [ testCase "x² + 2x + 1 = 0 -> [-1, -1]" $
      solveSquare 1 2 1 @?= [-1, -1]
  , testCase "x² - 2x + 1 = 0 -> [1, 1]" $
      solveSquare 1 (-2) 1 @?= [1, 1]
  ]

listLazyTests = testGroup "ListLazy"
  [ testCase "replicate 5 'a' -> \"aaaaa\"" $
      replicate' 5 'a' @?= "aaaaa"
  , testCase "replicate 0 'a' -> []" $
      replicate 0 'a' @?= ""
  , testCase "replicate (- 1) 'a' -> []" $
      replicate (- 1) 'a' @?= []
  , testCase "zip' [1.. ] \"abc\" -> [(1, 'a'), (2, 'b'), (3, 'c')]" $
      zip' [1 .. ] "abc" @?= [(1, 'a'), (2, 'b'), (3, 'c')]
  , testCase "zip' \"abc\" [1.. ] -> [('a', 1), ('b', 2), ('c', 3)]" $
      zip' "abc" [1 .. ] @?= [('a', 1), ('b', 2), ('c', 3)]
  , testCase "enumerate' \"abc\" -> [(0, 'a'), (1, 'b'), (2, 'c')]" $
      enumerate' "abc" @?= [(0, 'a'), (1, 'b'), (2, 'c')]
  , testCase "zipWith' (+) [4, 5] [1.. ] -> [5, 7]" $
      zipWith' (+) [4, 5] [1 .. ] @?= [5, 7]
  , testCase "zipLongest [1, 2] ['a', 'b'] -> [These 1 'a',These 2 'b']" $
      zipLongest [1, 2] ['a', 'b'] @?= [These 1 'a',These 2 'b']
  , testCase "zipLongest [1, 2] ['a'] -> [These 1 'a',This 2]" $
      zipLongest [1, 2] ['a'] @?= [These 1 'a',This 2]
  , testCase "zipLongest [1] ['a', 'b'] -> [These 1 'a',That 'b']" $
      zipLongest [1] ['a', 'b'] @?= [These 1 'a',That 'b']
  ]

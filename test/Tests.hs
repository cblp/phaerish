import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

import           Sqeq             (solveSquare)
import           Hw               (Idt(..))

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [solveSquareTests, testsIdtFunctor, testsIdtApplicative]

solveSquareTests = testGroup "solveSquare"
  [ testCase "x² + 2x + 1 = 0 -> [-1, -1]" $
      solveSquare 1 2 1 @?= [-1, -1]
  , testCase "x² - 2x + 1 = 0 -> [1, 1]" $
      solveSquare 1 (-2) 1 @?= [1, 1]
  ]

testsIdtFunctor = testGroup "Idt functor"
  [ testCase "Idt functor low 1" $
      fmap id (I 'a') @?= (I 'a')
  , testCase "Idt functor low 2" $
      fmap (+ 1) (fmap (+ 2) (I 1)) @?= fmap ((+ 1) . (+ 2)) (I 1)]

testsIdtApplicative = testGroup "Idt applicative"
  [ testCase "Idt applicative low 1" $
      pure id <*> I 'a' @?= (I 'a')
  , testCase "Idt applicative low 2" $
      pure (.) <*> I (+ 1) <*> I (+ 1) <*> I 1 @?= I (+ 1) <*> (I (+ 1) <*> I (1))
  , testCase "Idt applicative low 3" $
      pure (+ 1) <*> pure 1 @?= (I 2)
  , testCase "Idt applicative low 4" $
      I (+ 1) <*> pure 1 @?= pure ($ 1) <*> I (+ 1)]

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Monad (forM_)

import Types
import Lang

-- Unit-тесты
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Simple addition" $
      executeCmd (Word "+") [2, 3] @?= Ok [5]

  , testCase "Division by zero" $
      executeCmd (Word "/") [0, 3] @?= RuntimeError DivisionByZero

  , testCase "Stack underflow for addition" $
      executeCmd (Word "+") [3] @?= RuntimeError StackUnderflow

  , testCase "MOD with division by zero" $
      executeCmd (Word "MOD") [0, 7] @?= RuntimeError DivisionByZero

  , testCase "SWAP command" $
      executeCmd (Word "SWAP") [1, 2] @?= Ok [2, 1]

  , testCase "OVER command" $
      executeCmd (Word "OVER") [1, 2] @?= Ok [2, 1, 2]
  ]

-- Тесты программы
programTests :: TestTree
programTests = testGroup "Program tests"
  [ testCase "Simple program 5 3 +" $
      executeProgram (Program [Number 5, Number 3, Word "+"]) [] @?= Ok [8]

  , testCase "Complex program with DUP and *" $
      executeProgram (Program [Number 4, Word "DUP", Word "*"]) [] @?= Ok [16]

  , testCase "Division and multiplication program" $
      executeProgram (Program [Number 10, Number 2, Word "/", Number 3, Word "*"]) [] @?= Ok [15]
  ]

-- Рандомизированные тесты (Hedgehog)
randomizedTests :: TestTree
randomizedTests = testGroup "Randomized tests"
  [ testProperty "Addition with two positive integers" $
      property $ do
        x <- forAll $ Gen.int (Range.constant 0 100)
        y <- forAll $ Gen.int (Range.constant 0 100)
        executeCmd (Word "+") [x, y] === Ok [x + y]
  , testProperty "DUP command correctness" $
      property $ do
        x <- forAll $ Gen.int (Range.constant (-100) 100)
        executeCmd (Word "DUP") [x] === Ok [x, x]
  ]

-- Исчерпывающее тестирование
exhaustiveTests :: TestTree
exhaustiveTests = testGroup "Exhaustive tests"
  [ testCase "Exhaustive addition testing" $ do
      let range = [-10..10]
      forM_ range $ \x ->
        forM_ range $ \y ->
          executeCmd (Word "+") [x, y] @?= Ok [x + y]

  , testCase "Exhaustive SWAP testing" $ do
      let range = [-10..10]
      forM_ range $ \x ->
        forM_ range $ \y ->
          executeCmd (Word "SWAP") [x, y] @?= Ok [y, x]

  , testCase "Exhaustive OVER testing" $ do
      let range = [-10..10]
      forM_ range $ \x ->
        forM_ range $ \y ->
          executeCmd (Word "OVER") [x, y] @?= Ok [y, x, y]
  ]

main :: IO ()
main = defaultMain $ testGroup "All tests" [unitTests, programTests, randomizedTests, exhaustiveTests]

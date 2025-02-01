module Tests where

import Test.Tasty
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty.Hedgehog (testProperty, Hedgehog.Property, forAll, property)
import Hedgehog (Gen, Range, MonadGen, MonadTest, assert)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Char (toLower)
import Debug.Trace (trace)
import Lang
import Types
import qualified Data.Map as Map

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Simple addition" $
      assertEqual "1 + 1 should be 2"
        (Right (Ok [2], Dict Map.empty, []))
        (liftIO $ executeProgram (Program [Number 1, Number 1, Word "+"]) (Dict Map.empty) [] [])

  , testCase "String parsing" $
      assertEqual "Parsed string should be correct"
        (Right (Ok [], Dict Map.empty, []))
        (liftIO $ executeProgram (Program [PrintString "Hello, World!"]) (Dict Map.empty) [] [])

  , testCase "Define and use word" $
      assertEqual "Defined word should work correctly"
        (Right (Ok [3], Dict (Map.fromList [("double", Program [Number 2, Word "*"])]), []))
        (liftIO $ executeProgram (Program [Define "double" (Program [Number 2, Word "*"]), Number 1, Word "double"]) (Dict Map.empty) [] [])

  , testCase "If statement true" $
      assertEqual "If statement should handle true condition"
        (Right (Ok [1], Dict Map.empty, []))
        (liftIO $ executeProgram (Program [Number 1, If (Program [Number 1]) (Program [])]) (Dict Map.empty) [] [])

  , testCase "If statement false" $
      assertEqual "If statement should handle false condition"
        (Right (Ok [], Dict Map.empty, []))
        (liftIO $ executeProgram (Program [Number 0, If (Program [Number 1]) (Program [])]) (Dict Map.empty) [] [])

  , testCase "Division by zero" $
      assertEqual "Division by zero should raise error"
        (Right (RuntimeError DivisionByZero, Dict Map.empty, []))
        (liftIO $ executeProgram (Program [Number 1, Number 0, Word "/"]) (Dict Map.empty) [] [])

  , testCase "Stack underflow" $
      assertEqual "Stack underflow should raise error"
        (Right (RuntimeError StackUnderflow, Dict Map.empty, []))
        (liftIO $ executeProgram (Program [Word "+"]) (Dict Map.empty) [] [])

  , testCase "Floating point addition" $
      assertEqual "1.5 + 2.5 should be 4.0"
        (Right (Ok [], Dict Map.empty, [4.0]))
        (liftIO $ executeProgram (Program [Float 1.5, Float 2.5, Word "+"]) (Dict Map.empty) [] [])

  , testCase "Floating point multiplication" $
      assertEqual "1.5 * 2.5 should be 3.75"
        (Right (Ok [], Dict Map.empty, [3.75]))
        (liftIO $ executeProgram (Program [Float 1.5, Float 2.5, Word "*"]) (Dict Map.empty) [] [])

  , testCase "Floating point division" $
      assertEqual "3.75 / 1.5 should be 2.5"
        (Right (Ok [], Dict Map.empty, [2.5]))
        (liftIO $ executeProgram (Program [Float 3.75, Float 1.5, Word "/"]) (Dict Map.empty) [] [])

  , testCase "Floating point mod" $
      assertEqual "3.75 mod 1.5 should be 0.75"
        (Right (Ok [], Dict Map.empty, [0.75]))
        (liftIO $ executeProgram (Program [Float 3.75, Float 1.5, Word "mod"]) (Dict Map.empty) [] [])

  , testCase "Float to stack" $
      assertEqual "f>s should convert float to int"
        (Right (Ok [3], Dict Map.empty, []))
        (liftIO $ executeProgram (Program [Float 3.75, Word "f>s"]) (Dict Map.empty) [] [])

  , testCase "Stack to float" $
      assertEqual "s>f should convert int to float"
        (Right (Ok [], Dict Map.empty, [3.0]))
        (liftIO $ executeProgram (Program [Number 3, Word "s>f"]) (Dict Map.empty) [] [])

  , testCase "Duplication" $
      assertEqual "dup should duplicate top of stack"
        (Right (Ok [2, 2], Dict Map.empty, []))
        (liftIO $ executeProgram (Program [Number 2, Word "dup"]) (Dict Map.empty) [] [])

  , testCase "Drop" $
      assertEqual "drop should remove top of stack"
        (Right (Ok [], Dict Map.empty, []))
        (liftIO $ executeProgram (Program [Number 2, Word "drop"]) (Dict Map.empty) [] [])

  , testCase "Swap" $
      assertEqual "swap should swap top two elements"
        (Right (Ok [2, 1], Dict Map.empty, []))
        (liftIO $ executeProgram (Program [Number 1, Number 2, Word "swap"]) (Dict Map.empty) [] [])

  , testCase "Over" $
      assertEqual "over should copy second element over top"
        (Right (Ok [1, 2, 1], Dict Map.empty, []))
        (liftIO $ executeProgram (Program [Number 1, Number 2, Word "over"]) (Dict Map.empty) [] [])

  , testCase "Print string" $
      assertEqual "print string should output string"
        (Right (Ok [], Dict Map.empty, []))
        (liftIO $ executeProgram (Program [PrintString "Hello, World!"]) (Dict Map.empty) [] [])
  ]

propAddition :: Property
propAddition = property $ do
  x <- forAll $ Gen.int (Range.linear 0 100)
  y <- forAll $ Gen.int (Range.linear 0 100)
  let expected = Ok [x + y]
  actual <- liftIO $ executeProgram (Program [Number x, Number y, Word "+"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [])

propMultiplication :: Property
propMultiplication = property $ do
  x <- forAll $ Gen.int (Range.linear 0 100)
  y <- forAll $ Gen.int (Range.linear 0 100)
  let expected = Ok [x * y]
  actual <- liftIO $ executeProgram (Program [Number x, Number y, Word "*"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [])

propDivision :: Property
propDivision = property $ do
  x <- forAll $ Gen.int (Range.linear 1 100) -- Avoid division by zero
  y <- forAll $ Gen.int (Range.linear 1 100)
  let expected = Ok [x `div` y]
  actual <- liftIO $ executeProgram (Program [Number x, Number y, Word "/"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [])

propDup :: Property
propDup = property $ do
  x <- forAll $ Gen.int (Range.linear 0 100)
  let expected = Ok [x, x]
  actual <- liftIO $ executeProgram (Program [Number x, Word "dup"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [])

propDrop :: Property
propDrop = property $ do
  x <- forAll $ Gen.int (Range.linear 0 100)
  let expected = Ok []
  actual <- liftIO $ executeProgram (Program [Number x, Word "drop"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [])

propSwap :: Property
propSwap = property $ do
  x <- forAll $ Gen.int (Range.linear 0 100)
  y <- forAll $ Gen.int (Range.linear 0 100)
  let expected = Ok [x, y]
  actual <- liftIO $ executeProgram (Program [Number x, Number y, Word "swap"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [])

propOver :: Property
propOver = property $ do
  x <- forAll $ Gen.int (Range.linear 0 100)
  y <- forAll $ Gen.int (Range.linear 0 100)
  let expected = Ok [y, x, y]
  actual <- liftIO $ executeProgram (Program [Number x, Number y, Word "over"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [])

propMod :: Property
propMod = property $ do
  x <- forAll $ Gen.int (Range.linear 1 100)
  y <- forAll $ Gen.int (Range.linear 1 100)
  let expected = Ok [mod x y]
  actual <- liftIO $ executeProgram (Program [Number x, Number y, Word "mod"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [])

propDefineUse :: Property
propDefineUse = property $ do
  word <- forAll $ Gen.text (Range.linear 1 10) Gen.ascii
  x <- forAll $ Gen.int (Range.linear 0 100)
  y <- forAll $ Gen.int (Range.linear 0 100)
  let program = Program [Define (map toLower (unpack word)) (Program [Number x, Word "*"]), Number y, Word (map toLower (unpack word))]
  let expected = Ok [x * y]
  actual <- liftIO $ executeProgram program (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict (Map.fromList [(map toLower (unpack word), Program [Number x, Word "*"])]), [])

propFloatAddition :: Property
propFloatAddition = property $ do
  x <- forAll $ Gen.double (Range.linearFrac (-100.0) 100.0)
  y <- forAll $ Gen.double (Range.linearFrac (-100.0) 100.0)
  let expected = Ok []
  actual <- liftIO $ executeProgram (Program [Float x, Float y, Word "+"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [x + y])

propFloatMultiplication :: Property
propFloatMultiplication = property $ do
  x <- forAll $ Gen.double (Range.linearFrac (-100.0) 100.0)
  y <- forAll $ Gen.double (Range.linearFrac (-100.0) 100.0)
  let expected = Ok []
  actual <- liftIO $ executeProgram (Program [Float x, Float y, Word "*"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [x * y])

propFloatDivision :: Property
propFloatDivision = property $ do
  x <- forAll $ Gen.double (Range.linearFrac 1.0 100.0)
  y <- forAll $ Gen.double (Range.linearFrac 1.0 100.0)
  let expected = Ok []
  actual <- liftIO $ executeProgram (Program [Float x, Float y, Word "/"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [x / y])

propFloatMod :: Property
propFloatMod = property $ do
  x <- forAll $ Gen.double (Range.linearFrac 1.0 100.0) -- Avoid division by zero
  y <- forAll $ Gen.double (Range.linearFrac 1.0 100.0)
  let expected = Ok []
  actual <- liftIO $ executeProgram (Program [Float x, Float y, Word "mod"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [modd x y])

propFloatToStack :: Property
propFloatToStack = property $ do
  x <- forAll $ Gen.double (Range.linearFrac (-100.0) 100.0)
  let expected = Ok [floor x]
  actual <- liftIO $ executeProgram (Program [Float x, Word "f>s"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [])

propStackToFloat :: Property
propStackToFloat = property $ do
  x <- forAll $ Gen.int (Range.linear (-100) 100)
  let expected = Ok []
  actual <- liftIO $ executeProgram (Program [Number x, Word "s>f"]) (Dict Map.empty) [] []
  assert $ actual == Right (expected, Dict Map.empty, [fromIntegral x])

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ unitTests
  , testProperty "Addition" propAddition
  , testProperty "Multiplication" propMultiplication
  , testProperty "Division" propDivision
  , testProperty "Dup" propDup
  , testProperty "Drop" propDrop
  , testProperty "Swap" propSwap
  , testProperty "Over" propOver
  , testProperty "Mod" propMod
  , testProperty "Define and Use" propDefineUse
  , testProperty "Float Addition" propFloatAddition
  , testProperty "Float Multiplication" propFloatMultiplication
  , testProperty "Float Division" propFloatDivision
  , testProperty "Float Mod" propFloatMod
  , testProperty "Float to Stack" propFloatToStack
  , testProperty "Stack to Float" propStackToFloat
  ]

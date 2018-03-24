module CompressionSpec (compressionTests) where

import Test.HUnit
import Data.BitVector(BitVector)
import qualified Data.BitVector as BV
import qualified Compression as CP


compressionTests = TestList [toUnaryTests, fromUnaryTests]

toUnaryTests = TestList [ TestLabel "1 to unary" testToUnary1,
                        TestLabel "5 to unary" testToUnary5,
                        TestLabel "0 to unary" testToUnary0,
                        TestLabel "Negative to unary" testToUnaryNegative]
fromUnaryTests = TestList   [TestLabel "1 from unary" testFromUnary0,
                            TestLabel "5 from unary" testfromUnary5,
                            TestLabel "0 from unary" testFromUnary0,
                            TestLabel "Negative from unary" testfromUnaryNegative]

toUnaryTest :: Maybe String -> Int -> Test
toUnaryTest res x = TestCase ( assertEqual "Coded value"
                        res (fmap BV.showBin . CP.toUnary $ x))
fromUnaryTest :: Maybe Int -> BitVector  -> Test
fromUnaryTest res x = TestCase ( assertEqual "Uncoded value"
                        res (CP.fromUnary x))

testToUnary1 = toUnaryTest (Just "0b1") 1
testToUnary5 = toUnaryTest (Just "0b00001") 5
testToUnary0 = toUnaryTest Nothing 0
testToUnaryNegative = toUnaryTest Nothing (-123456)

testFromUnary1 = fromUnaryTest (Just 1) (BV.bitVec 1 1)
testfromUnary5 = fromUnaryTest (Just 5) (BV.bitVec 5 1)
testFromUnary0 = fromUnaryTest Nothing (BV.bitVec 1 0)
testfromUnaryNegative = fromUnaryTest Nothing (BV.bitVec 2 (-1))
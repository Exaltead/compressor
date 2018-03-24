module CompressionSpec (compressionTests) where

import Test.HUnit
import qualified Data.BitVector as BV
import qualified Compression as Comp


compressionTests = TestList [unaryTests]

unaryTests = TestList [ TestLabel "Unary 1" testUnary1,
                        TestLabel "Unary 5" testUnary5,
                        TestLabel "Unary 0" testUnary0,
                        TestLabel "Unary negative" testUnaryNegative]

unaryTest :: Maybe String -> Int -> Test
unaryTest res x = TestCase ( assertEqual "Bit representation" res (fmap BV.showBin . Comp.unary $ x))
testUnary1 = unaryTest (Just "0b1") 1
testUnary5 = unaryTest (Just "0b00001") 5
testUnary0 = unaryTest Nothing 0
testUnaryNegative = unaryTest Nothing (-123456)
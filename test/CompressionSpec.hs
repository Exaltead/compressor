module CompressionSpec (compressionTests) where

import Test.HUnit
import Data.BitVector(BitVector)
import qualified Data.BitVector as BV
import qualified Compression as CP


compressionTests = TestList [
    toUnaryTests,
    fromUnaryTests,
    toGolompRizeTests,
    gRDecodeTests]

toUnaryTests = TestList [
    TestLabel "1 to unary" testToUnary1,
    TestLabel "5 to unary" testToUnary5,
    TestLabel "0 to unary" testToUnary0,
    TestLabel "Negative to unary" testToUnaryNegative]
fromUnaryTests = TestList [
    TestLabel "1 from unary" testFromUnary1,
    TestLabel "5 from unary" testfromUnary5,
    TestLabel "0 from unary" testFromUnary0,
    TestLabel "Negative from unary" testfromUnaryNegative]
toGolompRizeTests = TestList [
    TestLabel "1 to golompRize, k=0" testGolompRizeK0V1,
    TestLabel "0 to golompRize, k=0" testGolompRizeK0V0,
    TestLabel "5 to golompRize, k=0" testGolompRizeK0V5,
    TestLabel "Negative to golompRize, k=0" testGolompRizeK0VNegative,
    TestLabel "0 to golompRize, k=4" testGolompRizeK4V0,
    TestLabel "10 to golompRize, k=3" testGolompRizeK3V10]
gRDecodeTests = TestList [
    TestLabel "Small int encode and decode, k = 5" testGRDecodeSmallInteger,
    TestLabel "Medium int encode and decode, k = 10" testGRDecodeMediumInteger,
    TestLabel "Large int encode and decode, k = 12" testGRDecodeLargeInteger,
    TestLabel "Zero encode and decode, k = 10" testGRDecodeZero]

toUnaryTest :: Maybe String -> Int -> Test
toUnaryTest res x = TestCase ( assertEqual "Encoded value"
                        res (bitstring . CP.toUnary $ x))
fromUnaryTest :: Maybe Int -> BitVector  -> Test
fromUnaryTest res x = TestCase ( assertEqual "Decoded value"
                        res (CP.fromUnary x))

toGolompRizeTest :: Int -> Maybe String -> Int -> Test
toGolompRizeTest k ex v = TestCase( assertEqual "Encoded value"
                            ex (bitstring . CP.toGolompRize k $ v))

gRDecodeTest :: Int -> Maybe Int -> Test
gRDecodeTest k = decodabilityTest (CP.toGolompRize k) (CP.fromGolompRize k)

decodabilityTest :: (Int -> Maybe a) -> (a -> Maybe Int) -> Maybe Int -> Test
decodabilityTest to from v = TestCase(assertEqual "Encoded and decoded value" v (v >>= to >>= from))

bitstring :: Functor f => f BitVector -> f String
bitstring = fmap (drop 2 . BV.showBin)

-- Unary tests
testToUnary1 = toUnaryTest (Just "1") 1
testToUnary5 = toUnaryTest (Just "00001") 5
testToUnary0 = toUnaryTest Nothing 0
testToUnaryNegative = toUnaryTest Nothing (-123456)

testFromUnary1 = fromUnaryTest (Just 1) (BV.bitVec 1 1)
testfromUnary5 = fromUnaryTest (Just 5) (BV.bitVec 5 1)
testFromUnary0 = fromUnaryTest Nothing (BV.bitVec 1 0)
testfromUnaryNegative = fromUnaryTest Nothing (BV.bitVec 2 (-1))

-- With k = 0 golompRize should be same as unary + 1
testGolompRizeK0V0 = toGolompRizeTest 0 (Just "1") 0
testGolompRizeK0V1 = toGolompRizeTest 0 (Just "01") 1
testGolompRizeK0V5 = toGolompRizeTest 0 (Just "000001") 5
testGolompRizeK0VNegative = toGolompRizeTest 0 Nothing (-1)

-- test with several k and several values
-- TODO: Test more
testGolompRizeK4V0 = toGolompRizeTest 4 (Just "10000") 0
-- h  = 10 / (2^3) -> 01 , tail = 10 mod 2^3 -> 010
testGolompRizeK3V10 = toGolompRizeTest 3 (Just "01010") 10

-- Test that numbers decoded can be reverted
testGRDecodeSmallInteger = gRDecodeTest 5 (Just 12)
testGRDecodeMediumInteger = gRDecodeTest 10 (Just 123456)
testGRDecodeLargeInteger = gRDecodeTest 12 (Just 123548826)
testGRDecodeZero = gRDecodeTest 10 (Just 0)


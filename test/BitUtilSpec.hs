module BitUtilSpec (bitUtilsTests) where

import Test.HUnit
import BitUtils
--import Data.Bits

bitUtilsTests = TestList[
    TestLabel "DivideBits 27 at 3rd bit" testDivideBitsInteger,
    TestLabel "DivideBits 12345 at 0th bit" testDivideBitsZeroIndex,
    TestLabel "JoinBits (6, 3) at 3rd bit" testJoinBitsInteger]

testDivideBitsInteger = TestCase (assertEqual "27 at 3rd bit" (6, 3) (divideBits 2 (27 :: Int)))
testDivideBitsZeroIndex = TestCase(assertEqual "12345 at 0" (12345, 0) (divideBits 0 (12345 :: Int)))

testJoinBitsInteger = TestCase (assertEqual "(6, 3) at 3rd bit" 27 (joinBits 2 ((6, 3):: (Int, Int))))
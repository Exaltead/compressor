module CompressionSpec (compressionTests) where

import Test.HUnit
import qualified Data.BitVector as BV
import qualified Compression as Comp


compressionTests = TestList [TestLabel "0 in unary" testUnary_0]

testUnary_0 = TestCase (assertEqual "Unary 0" "1" (BV.showBin (Comp.unary 0)))
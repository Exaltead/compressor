import Test.HUnit
import CompressionSpec
import BitUtilSpec

testSuites = TestList [compressionTests, bitUtilsTests]

main :: IO ()
main =  runTestTT testSuites >>= wasSuccesful

wasSuccesful :: Counts -> IO()
wasSuccesful (Counts _ _ _ f)
    | f == 0 = return ()
    | otherwise = fail "Tests have failures"
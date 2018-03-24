import Test.HUnit
import CompressionSpec

main :: IO ()
main =  runTestTT compressionTests >>= wasSuccesful

wasSuccesful :: Counts -> IO()
wasSuccesful (Counts _ _ _ f)
    | f == 0 = return ()
    | otherwise = fail "Tests have failures"
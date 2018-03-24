import Test.HUnit
import CompressionSpec
main :: IO ()
main = do
    fmap showCounts (runTestTT compressionTests)
    return ()

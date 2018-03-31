module Compression (toGolompRize,
                    fromGolompRize,
                    toUnary,
                    fromUnary) where
import Control.Monad.Zip(mzip)
import Data.BitVector
import Control.Arrow ((***))
import BitUtils

toGolompRize :: Int -> Int -> Maybe BitVector
-- compute l = x mod (2^k), h = floor(i / (2^k))
-- code h+1 in unary and append binary l in k bits
toGolompRize k x = fmap (`append` l) h
                where   core = 2 ^ k
                        h = toUnary((x `div` core) + 1)
                        l = bitVec k (x `mod` core)

-- decoding expects the bitvector to contain only the decodable string
fromGolompRize :: Int -> BitVector -> Maybe Int
fromGolompRize  = fmap (gRPartsDecode k) . extractUnaryHead

extractUnaryHead :: Monad m => BitVector -> m (BitVector, BitVector)
extractUnaryHead bv | uint bv > 0 = return ( splitBitsAt (msb1 bv)  bv)
                    | otherwise = fail "Has to have nonzero bit"
gRPartsDecode :: Int -> (BitVector,  BitVector) -> Int
gRPartsDecode k (h, t) = ((2^k) * (size h - 1)) + fromInteger (uint t)

splitBitsAt :: Int -> BitVector ->  (BitVector, BitVector)
splitBitsAt i v  = (most (size v - i) v, least i v)


-- Unary = N-1 leading zeros and 1
toUnary :: Int -> Maybe BitVector
toUnary v   | v > 0 = Just (bitVec v 1)
            | otherwise = Nothing

fromUnary :: BitVector -> Maybe Int
fromUnary bv    | uint bv == 1 = Just (size bv)
                | otherwise = Nothing
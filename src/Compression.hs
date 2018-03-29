module Compression (toGolompRize,
                    toUnary,
                    fromUnary) where

import qualified Data.Bits as Bits
import Data.Bits(Bits)
import Data.BitVector(BitVector)
import qualified Data.BitVector as BV
import Control.Arrow ((***))

toGolompRize :: Int -> Int -> Maybe BitVector
-- compute l = x mod (2^k), h = floor(i / (2^k))
-- code h+1 in unary and append binary l in k bits
toGolompRize k x = fmap (`BV.append` l) h
                where   core = 2 ^ k
                        h = toUnary((x `div` core) + 1)
                        l = BV.bitVec k (x `mod` core)

-- decoding expects the bitvector to contain only the decodable string
fromGolompRize :: Int -> BitVector -> Maybe Int
-- find h by finding the most significant 1
-- take the i +1-> end uint value as tail
-- multiply h by 2 ^ k and add the tail
fromGolompRize k = fmap (fromInteger . uncurry(+) . (decodeGRHead k *** decodeGRTail)) . splitWith indexOfLeading1

indexOfLeading1 :: BitVector -> Maybe Int
indexOfLeading1 bv | BV.uint bv /= 0 = Just . BV.msb1 $ bv
                   | otherwise = Nothing

splitWith :: (BitVector -> Maybe Int) -> BitVector ->  Maybe (BitVector, BitVector)
splitWith f bv = splitBitsAt bv <$> f bv

splitBitsAt :: BitVector -> Int ->  (BitVector, BitVector)
splitBitsAt bv i  = (BV.most i bv , BV.least i bv)

decodeGRHead :: Int ->  BitVector -> Maybe Int
decodeGRHead k = fmap (\h -> (h - 1) * (2 ^k)) . fromUnary

decodeGRTail :: BitVector -> Integer
decodeGRTail = BV.uint

-- Unary = N-1 leading zeros and 1
toUnary :: Int -> Maybe BitVector
toUnary v   | v > 0 = Just (BV.bitVec v 1)
            | otherwise = Nothing

fromUnary :: BitVector -> Maybe Int
fromUnary bv    | BV.uint bv == 1 = Just (BV.size bv)
                | otherwise = Nothing
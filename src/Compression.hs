module Compression (toGolompRize,
                    toUnary,
                    fromUnary) where

import qualified Data.Bits as Bits
import Data.Bits(Bits)
import Data.BitVector(BitVector)
import qualified Data.BitVector as BV

toGolompRize :: Int -> Int -> Maybe BitVector
-- compute l = x mod (2^k), h = floor(i / (2^k))
-- code h+1 in unary and append binary l in k bits
toGolompRize k x = fmap (`BV.append` l) h
                where   core = 2 ^ k
                        h = toUnary((x `div` core) + 1)
                        l = BV.bitVec k (x `mod` core)

--fromGolompRize :: Int -> BitVector -> Maybe Int
-- Unary = N-1 leading zeros and 1
toUnary :: Int -> Maybe BitVector
toUnary v   | v > 0 = Just (BV.bitVec v 1)
            | otherwise = Nothing

fromUnary :: BitVector -> Maybe Int
fromUnary bv    | BV.uint bv == 1 = Just (BV.size bv)
                | otherwise = Nothing
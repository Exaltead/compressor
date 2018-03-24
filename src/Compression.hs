module Compression (golompRize,
                    toUnary,
                    fromUnary) where

import qualified Data.Bits as Bits
import Data.Bits(Bits)
import Data.BitVector(BitVector)
import qualified Data.BitVector as BV

golompRize :: Bits b => Int -> b -> BitVector
-- compute l = x mod (2^k), h = floor(i / (2^k))
--
golompRize _ _ = BV.bitVec 1 1

-- Unary = N-1 leading zeros and 1
toUnary :: Int -> Maybe BitVector
toUnary v   | v > 0 = Just (BV.bitVec v 1)
            | otherwise = Nothing

fromUnary :: BitVector -> Maybe Int
fromUnary bv    | BV.uint bv == 1 = Just (BV.size bv)
                | otherwise = Nothing
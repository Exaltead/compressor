module Compression (golompRize, unary) where

import qualified Data.Bits as Bits
import Data.Bits(Bits)
import Data.BitVector(BitVector)
import qualified Data.BitVector as BV

golompRize :: Bits b => b -> BitVector
golompRize _ = BV.bitVec 1 1

-- Unary = N leading zeros and 1
unary :: Int -> BitVector
unary v = BV.bitVec (v + 1) 1
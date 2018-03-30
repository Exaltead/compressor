module BitUtils (divideBits, joinBits) where

import Data.Bits

divideBits :: Bits b => Int -> b -> (b, b)
-- i is the lowest bit of the first value, ie. i=4 -> 111111  = (111, 111)
divideBits i v = (first, second)
                where   first = shiftR  v i
                        second = xor v . shiftL first $ i

joinBits:: Bits b => Int -> (b, b) -> b
joinBits i (f, s) = shiftL f i .|. s

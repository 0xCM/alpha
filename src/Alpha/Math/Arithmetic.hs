module Alpha.Math.Arithmetic
(
    divides,
    modpart,
    lobyte,
    hibyte
)
where

import Data.Bool
import Data.Ix
import Data.Int
import Data.Eq
import Data.Functor
import Data.Word
import qualified Data.List as L
import qualified Data.Ix as Ix
import Alpha.Canonical
import Alpha.Text.Combinators
import Alpha.Data.Numbers
import Alpha.Data.Bits
import Alpha.Canonical
import Alpha.Base

lobyte :: Word16 -> Word8
lobyte x = x .&. 0xFF |> fromIntegral

-- | Extracts the high-order byte 
hibyte :: Word16 -> Word8
hibyte x  = (x .>>.8) .&. 0xFF |> fromIntegral

-- Determines whether m is evenly divisible by n
divides::(Integral a) => a -> a -> Bool
divides m n = (m `mod` n) == 0

-- Calculates the points within the interval that are
-- divisible by n
modpart::(Integral a, Ix.Ix a) => (a, a) -> a -> [a]
modpart (min,max) n 
    = range (min, max) 
      |> fmap (\j -> case divides j n of True -> j; _ -> 0)
      |> L.filter (\j -> j /= 0)


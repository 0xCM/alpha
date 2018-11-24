-----------------------------------------------------------------------------
-- | Bit-level types and operations
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}

module Alpha.Data.Bits 
(
    SizedBits(..),
    toggled, bitcount, bitsplat,
    (.^.), (.~.), (.<<.), (.>>.), (.?.),
    lobyte,hibyte
    
)
where
import qualified Data.Text as Text
import Data.Bits hiding(bit)
import Alpha.Base
import Alpha.Canonical
import Alpha.Text.Combinators
import Alpha.Data.Numbers
import Alpha.Data.Bit
import Alpha.Data.Natural
import GHC.TypeLits

class (Sized n, FiniteBits a) => SizedBits n a where
    
toggled :: Bits a => a -> Int -> Bit
toggled n i = testBit n i |> bit

-- | Delegates to the finiteBitSize function
bitcount :: (FiniteBits a) => a -> Int
bitcount a = finiteBitSize a

bitsplat::(Integral a, Bits a, Integral b, Bits b, Integral c) => c -> a -> a -> b
bitsplat n x y = left .|. right
        where 
            left = (fromIntegral x) .<<. (fromIntegral n)
            right = fromIntegral y 
                    
(.^.) :: Bits a => a -> a -> a
(.^.) m n = xor m n
infixl 6 .^.

(.~.) :: Bits a => a -> a
(.~.) m = complement m
infixl 5 .~.

(.<<.) :: Bits a => a -> Int -> a
(.<<.) n i = shiftL n i
infixl 8 .<<.

(.>>.) :: Bits a => a -> Int -> a
(.>>.) n i = shiftR n i
infixl 8 .>>.

-- | Returns 'True' if the bit at position 'i' of the value 'n' is set, 'False' otherwise
(.?.) :: Bits a => a -> Int -> Bool
(.?.) = testBit
infixl 5 .?.

lobyte :: Word16 -> Word8
lobyte x = x .&. 0xFF |> fromIntegral

-- | Extracts the high-order byte 
hibyte :: Word16 -> Word8
hibyte x  = (x .>>.8) .&. 0xFF |> fromIntegral


instance Concatenable Word8 Word8 where
    type Concatenated Word8 Word8 = Word16
    concat x y = bitsplat 8 x y
            
instance Concatenable Word16 Word16 where
    type Concatenated Word16 Word16 = Word32
    concat x y = bitsplat 16 x y

instance Concatenable Word32 Word32 where
    type Concatenated Word32 Word32 = Word64
    concat x y = bitsplat 32 x y

instance Concatenable Int8 Int8 where
    type Concatenated Int8 Int8 = Int16
    concat x y = bitsplat 8 x y
            
instance Concatenable Int16 Int16 where
    type Concatenated Int16 Int16 = Int32
    concat x y = bitsplat 16 x y

instance Concatenable Int32 Int32 where
    type Concatenated Int32 Int32 = Int64
    concat x y = bitsplat 32 x y
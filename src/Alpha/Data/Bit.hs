-----------------------------------------------------------------------------
-- | A space-efficient represntation of a bit value, adapted from bool8
-- Copyright   :  (c) 0xCM, 2018 + Contributors per license
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Alpha.Data.Bit 
(
    Bit,Toggle(..),ToBit(..),
    on, off,
    fromBool
)
where
    
import Foreign.Storable (Storable, poke, peek, sizeOf, alignment)
import Foreign.Ptr (Ptr, castPtr)
import Data.Bool hiding(bool)
import Alpha.GHC.Base
import Alpha.Canonical
import Data.Bits(Bits(..))
import Prelude(undefined)
import Alpha.Base

data {-# CTYPE "HsBool" #-} Flag = On | Off
    deriving (Eq, Enum, Ord, Generic, Data, Typeable)
    
newtype Bit = Bit Flag
    deriving (Eq, Generic, Data, Typeable)

class ToBit a where
    bit::a -> Bit    
    
type family Toggle t | t -> t where
    Toggle 0 = 0
    Toggle 1 = 1    
            
off::Bit
off = Bit Off

on::Bit
on = Bit On

bitref'::Ptr Bit -> Ptr Word8
bitref' = castPtr

fromBool :: Bool -> Bit
fromBool False = off
fromBool True = on

instance Boolean Bit where
    bool (Bit On) = True
    bool (Bit Off) = False
    
instance ToBit Bool where
    bit True = on
    bit False = off    

instance Show Flag where
    show On = "1"
    show Off = "0"

instance Invertible Bit Bit where
    invert (Bit On) = off
    invert (Bit Off) = on

instance Show Bit where
    show (Bit Off) = "0"
    show _ = "1"

instance Bounded Bit where
    minBound = off
    maxBound = on

instance Enum Bit where
    fromEnum (Bit b) = fromEnum b
    toEnum = Bit . toEnum
    
instance Storable Bit where
    sizeOf _ = 1
    alignment _ = 1
    peek ptr = f <$> peek (bitref' ptr) where 
        nonzero x = ifelse (x /= 0) On Off
        f = Bit . nonzero
    poke ptr (Bit b) = poke (bitref' ptr) (fromIntegral $ fromEnum b)        

instance Bits Bit where
    (.&.) (Bit On) (Bit On) = on
    (.&.) _ _ = off

    (.|.) (Bit Off) (Bit Off) = off
    (.|.) _ _ = on
    
    xor (Bit On) (Bit Off) = on
    xor (Bit Off) (Bit On) = on
    xor _ _ = off

    complement   = invert
    
    shift x i = x
    
    rotate x i = x

    bit 0 = off
    bit 1 = on

    bitSize _     = 1

    bitSizeMaybe _ = Just 1

    isSigned     = const False
    
    testBit (Bit On) 0 = True
    testBit _ _ = False

    popCount (Bit On) = 1
    popCount (Bit Off) = 0
  


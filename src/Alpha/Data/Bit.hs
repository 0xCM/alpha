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
    Bit,ToBit(..),
    on, isOn,
    off, isOff,
    fromBool
)
where
    
import Foreign.Storable (Storable, poke, peek, sizeOf, alignment)
import Foreign.Ptr (Ptr, castPtr)
import Data.Bool hiding(bool)
import Alpha.Base
import Alpha.Canonical
import Data.Bits(Bits(..))

data {-# CTYPE "HsBool" #-} Flag = On | Off
    deriving (Eq, Enum, Ord, Generic, Data, Typeable)
    
newtype Bit = Bit Flag
    deriving (Eq, Ord, Generic, Data, Typeable)

class ToBit a where
    bit::a -> Bit    
        
-- | Constructs a 'Bit' in the 'Off' state    
off::Bit
off = Bit Off

-- | Constructs a 'Bit' in the 'On' state    
on::Bit
on = Bit On

-- | Returns true if off, false otherwise
isOff::Bit -> Bool
isOff (Bit flag) = flag == Off

-- | Returns true if on, false otherwise
isOn::Bit -> Bool
isOn (Bit flag) = flag == On

bitref'::Ptr Bit -> Ptr Word8
bitref' = castPtr

fromBool :: Bool -> Bit
fromBool False = off
fromBool True = on

instance ToInt Bit where
    int (Bit flag) = ifelse (flag == On) 1 0

instance ToWord Bit where
    word (Bit flag) = ifelse (flag == On) 1 0
    
instance FromInt Bit where
    fromInt i = ifelse (i /= 0) on off

instance FromWord Bit where
    fromWord i = ifelse (i /= 0) on off
    
instance Multiplicative Bit where
    mul (Bit On) (Bit On) = on
    mul _ _ = off

instance Additive Bit where
    add (Bit On) (Bit On) = off
    add (Bit On) (Bit Off) = on
    add (Bit Off) (Bit On) = on
    add (Bit Off) (Bit Off) = off

instance Subtractive Bit where
    sub (Bit On) (Bit On) = off
    sub (Bit On) (Bit Off) = on
    sub (Bit Off) (Bit On) = on
    sub (Bit Off) (Bit Off) = off
    
instance Invertible Bit where 
    invert (Bit On) = off
    invert (Bit Off) = on
    
    {-# INLINE invert #-}
    
instance Semigroup Bit where
    (<>) = (+)
    {-# INLINE (<>) #-}    

instance Absolutist Bit where 
    abs = id
    {-# INLINE abs #-}

instance Nullary Bit where
    zero = off
    {-# INLINE zero #-}

instance Unital Bit where
    one = on
    {-# INLINE one #-}

instance Monoid Bit where 
    mempty = off
    {-# INLINE mempty #-}

instance Boolean Bit where
    bool (Bit On) = True
    bool (Bit Off) = False
    
instance ToBit Bool where
    bit True = on
    bit False = off    

instance Show Flag where
    show On = "1"
    show Off = "0"

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
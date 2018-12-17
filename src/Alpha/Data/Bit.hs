-----------------------------------------------------------------------------
-- | A space-efficient represntation of a bit value, adapted from bool8
-- Copyright   :  (c) 0xCM, 2018 + Contributors per license
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Alpha.Data.Bit 
(
    Bit(..),
    ToBit(..), 
    ToBitString(..),
    Flag(..),
    on, isOn,
    off, isOff,
)
where
    
import Alpha.Base
import Alpha.Canonical
import Data.Bits(Bits(..))
import qualified Data.List as List

data {-# CTYPE "HsBool" #-} Flag = On | Off
    deriving (Eq, Enum, Ord, Generic, Data, Typeable, Read)


newtype Bit = Bit Flag
    deriving (Eq, Ord, Generic, Data, Typeable, Read,
        Disjunctive, Conjunctive, Invertive, Logical, 
        JoinSemiLattice,MeetSemiLattice, Lattice
        )




newtype BitString = BitString [Bit]   
    deriving (Eq, Ord, Generic, Data, Typeable, Read)
instance Newtype (BitString)

type instance Unsigned Bit = Bit
instance Unsignable Bit

class ToBit a where
    bit::a -> Bit    

class ToBitString a where
    bits::a -> BitString
        
-- | Constructs a 'Bit' in the 'Off' state    
off::Bit
off = Bit Off
{-# INLINE off #-}

-- | Constructs a 'Bit' in the 'On' state    
on::Bit
on = Bit On
{-# INLINE on #-}

-- | Returns true if off, false otherwise
isOff::Bit -> Bool
isOff (Bit flag) = flag == Off
{-# INLINE isOff #-}

-- | Returns true if on, false otherwise
isOn::Bit -> Bool
isOn (Bit flag) = flag == On
{-# INLINE isOn #-}

bitref'::Ptr Bit -> Ptr Word8
bitref' = castPtr
{-# INLINE bitref' #-}

bitstring::(Integral a) => a -> BitString
bitstring i = BitString $ bitstring' (quotRem i 2) []  where
    bitstring' (n,d) r = seq c $
        case n of
        0 -> r'
        _ -> bitstring' (quotRem n 2) r' 
        where
            c  = ifelse (d == 0) off on
            r' = c : r            

instance Disjunctive Flag where
    On || On = True
    On || Off = True
    Off || On = True
    Off || Off = False
    {-# INLINE (||) #-}

instance Conjunctive Flag where
    On && On = True
    On && Off = False
    Off && On = False
    Off && Off = False
    {-# INLINE (&&) #-}

instance Invertive Flag where
    not On = False
    not Off = True
    {-# INLINE not #-}
        
instance Logical Flag    

instance JoinSemiLattice Flag where
        (\/) x y = ifelse (x || y) On Off
        {-# INLINE (\/) #-}

instance MeetSemiLattice Flag where
        (/\) x y = ifelse (x && y) On Off
        {-# INLINE (/\) #-}

instance Lattice Flag where    
            
instance Formattable BitString where
    format (BitString bits) =  format <$> bits |> collapse |> prefix n
        where n =  List.length bits |> format |> parenthetical |> spaced

instance ToInteger BitString where
    integer = undefined

instance Show BitString where
    show = string . format

instance ToInt Bit where
    int (Bit flag) = ifelse (flag == On) 1 0
    {-# INLINE int #-}

instance ToWord Bit where
    word (Bit flag) = ifelse (flag == On) 1 0
    {-# INLINE word #-}

instance FromInt Bit where
    fromInt i = ifelse (i /= 0) on off
    {-# INLINE fromInt #-}

instance FromWord Bit where
    fromWord i = ifelse (i /= 0) on off
    {-# INLINE fromWord #-}

-- Follows the logic of "and"    
instance Multiplicative Bit where
    mul (Bit On) (Bit On) = on
    mul _ _ = off
    {-# INLINE mul #-}

instance Unital Bit where
    one = on
    {-# INLINE one #-}
    
-- Follows the logic of "or"
instance Additive Bit where
    add (Bit On) (Bit On) = on
    add (Bit On) (Bit Off) = on
    add (Bit Off) (Bit On) = on
    add (Bit Off) (Bit Off) = off
    {-# INLINE add #-}

instance Nullary Bit where    
    zero = off
    {-# INLINE zero #-}

instance Invertible Bit where 
    invert (Bit On) = off
    invert (Bit Off) = on
    
    {-# INLINE invert #-}
    
instance Semigroup Bit where
    (<>) = (+)
    {-# INLINE (<>) #-}    

instance Absolute Bit where 
    abs = id
    {-# INLINE abs #-}


instance Monoid Bit where 
    mempty = off
    {-# INLINE mempty #-}

instance Group Bit
instance Semiring Bit

instance Boolean Bit where
    bool (Bit On) = True
    bool (Bit Off) = False
    {-# INLINE bool #-}

instance ToBit Bool where
    bit True = on
    bit False = off
    {-# INLINE bit #-}    

instance Formattable Flag where
    format On = "1"
    format Off = "0"
    
instance Formattable Bit where
    format (Bit Off) = "0"
    format _ = "1"
        
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
    {-# INLINE fromEnum #-}

    toEnum = Bit . toEnum
    {-# INLINE toEnum #-}

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
    {-# INLINE (.&.) #-}

    (.|.) (Bit Off) (Bit Off) = off
    (.|.) _ _ = on
    {-# INLINE (.|.) #-}

    xor (Bit On) (Bit Off) = on
    xor (Bit Off) (Bit On) = on
    xor _ _ = off
    {-# INLINE xor #-}

    complement   = invert
    {-# INLINE complement #-}

    shift x i = x
    {-# INLINE shift #-}

    rotate x i = x
    {-# INLINE rotate #-}

    bit 0 = off
    bit 1 = on
    {-# INLINE bit #-}

    bitSize _     = 1

    bitSizeMaybe _ = Just 1

    isSigned     = const False
    
    testBit (Bit On) 0 = True
    testBit _ _ = False

    popCount (Bit On) = 1
    popCount (Bit Off) = 0
        

instance ToBitString Int where
    bits = bitstring
    {-# INLINE bits #-}
    
instance ToBitString Int8 where
    bits = bitstring
    {-# INLINE bits #-}

instance ToBitString Int16 where
    bits = bitstring
    {-# INLINE bits #-}

instance ToBitString Int32 where
    bits = bitstring
    {-# INLINE bits #-}
        
instance ToBitString Int64 where
    bits = bitstring
    {-# INLINE bits #-}

instance ToBitString Integer where
    bits = bitstring
    {-# INLINE bits #-}
    
instance ToBitString Word where
    bits = bitstring
    {-# INLINE bits #-}
    
instance ToBitString Word8 where
    bits = bitstring
    {-# INLINE bits #-}

instance ToBitString Word16 where
    bits = bitstring
    {-# INLINE bits #-}

instance ToBitString Word32 where
    bits = bitstring
    {-# INLINE bits #-}
        
instance ToBitString Word64 where
    bits = bitstring
    {-# INLINE bits #-}
        
instance ToBitString Natural where
    bits = bitstring
    {-# INLINE bits #-}
        
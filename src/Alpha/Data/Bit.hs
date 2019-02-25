-----------------------------------------------------------------------------
-- | 0 1
-- Copyright   :  (c) Chris Moore, 2018 + Contributors per license
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
module Alpha.Data.Bit 
(
    Bit(..),
    ToBit(..), 
    Flag(..),
    Bitfield(..),
    BitString,
    on, isOn,
    off, isOff,
    bitstring,
    bitcount, 
    lobyte,hibyte,bitsplat,

    (.&.), (.|.), xor,
)
where
import Alpha.Canonical
import Data.Bits(Bits(..))
import qualified Data.List as List
import qualified Algebra.Lattice as Lattice

-- | Taken from bool8
data {-# CTYPE "HsBool" #-} Flag = On | Off
    deriving (Eq, Enum, Ord, Generic, Data, Typeable, Read, Bounded)

newtype Bit = Bit Flag
    deriving (Eq, Ord, Generic, Data, Typeable, Read, Invertive, 
        Propositional, JoinSemiLattice,MeetSemiLattice,  Lattice
        )

-- | Encapsulates an ordered sequence of bits
newtype BitString = BitString [Bit]   
    deriving (Eq, Ord, Generic, Data, Typeable, Read)
instance Newtype (BitString)

    
type instance Individual Integer = Bit
type instance Individual Int = Bit
type instance Individual Int8 = Bit
type instance Individual Int16 = Bit
type instance Individual Int32 = Bit
type instance Individual Int64 = Bit
type instance Individual Natural = Bit
type instance Individual Word = Bit
type instance Individual Word8 = Bit
type instance Individual Word16 = Bit
type instance Individual Word32 = Bit
type instance Individual Word64 = Bit

class ToBit a where
    bit::a -> Bit    

class Bits a  => Bitfield a where

    -- | Infix synonym for 'xor'
    (.^.)::a -> a -> a
    (.^.) m n = xor m n
    {-# INLINE (.^.) #-}
    infixl 6 .^.
    
    -- | Returns 'True' if the bit at position 'i' of the value 'n' is set, 'False' otherwise
    (.?.):: a -> Int -> Bool
    (.?.) = testBit
    {-# INLINE (.?.) #-}
    infixl 5 .?.
    
    -- | Infix synonym for 'shiftL'    
    (.<.)::a -> Int -> a
    (.<.) n i = shiftL n i
    {-# INLINE (.<.) #-}
    infixl 8 .<.

    -- | Infix synonym for 'shiftR'    
    (.>.)::a -> Int -> a
    (.>.) n i = shiftR n i
    {-# INLINE (.>.) #-}
    infixl 8 .>.
        
class (Bitfield a, Bounded a) => FiniteBitfield a where
    bitcount:: a -> Int

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

-- | Returns the value of an identified bit
ibit::Bits a => a -> Int -> Bit
ibit n i = ifelse (testBit n i == True) on off
{-# INLINE ibit #-}


-- | Constrcts a 'Bitstring' value from an integer
bitstring::(IntegralDomain a, Integral a) => a -> BitString
bitstring i = BitString $ bitstring' (i /% 2) []  where
    bitstring' (n,d) r = seq c $
        case n of
        0 -> r'
        _ -> bitstring' (n /% 2) r' 
        where
            c  = ifelse (d == 0) off on
            r' = c : r            

-- bitcount :: (FiniteBits a) => a -> Int
-- bitcount a = finiteBitSize a

bitsplat::(Integral a, Bitfield a, Integral b, Bitfield b, Integral c) => c -> a -> a -> b
bitsplat n x y = left .|. right
        where 
            left = (fromIntegral x) .<. (fromIntegral n)
            right = fromIntegral y 
                    
lobyte :: Word16 -> Word8
lobyte x = x .&. 0xFF |> integral

-- | Extracts the high-order byte 
hibyte :: Word16 -> Word8
hibyte x  = (x .>.8) .&. 0xFF |> integral
            
instance Unsignable Bit

instance Disjunctive Bit where
    (Bit x) || (Bit y) = x || y
    {-# INLINE (||) #-}

instance XDisjunctive Bit where
    (Bit x) ^| (Bit y) = x ^| y
    {-# INLINE (^|) #-}    

instance Conjunctive Bit where
    (Bit x) && (Bit y) = x && y
    {-# INLINE (&&) #-}

instance Biconditional Bit where
    (Bit x) <=> (Bit y) = x <=> y
    {-# INLINE (<=>) #-}

instance Disjunctive Flag where
    On || On = True
    On || Off = True
    Off || On = True
    Off || Off = False
    {-# INLINE (||) #-}

instance XDisjunctive Flag where
    On ^| On = False
    On ^| Off = True
    Off ^| On = True
    Off ^| Off = False
    {-# INLINE (^|) #-}
    
instance Conjunctive Flag where
    On && On = True
    On && Off = False
    Off && On = False
    Off && Off = False
    {-# INLINE (&&) #-}

instance Implicative Flag where
    On ==> On = True
    On ==> Off = False
    Off ==> On = True
    Off ==> Off = True
    {-# INLINE (==>) #-}

instance Biconditional Flag where
    On <=> On = True
    On <=> Off = False
    Off <=> On = False
    Off <=> Off = True
    {-# INLINE (<=>) #-}
        
instance Invertive Flag where
    not On = False
    not Off = True
    {-# INLINE not #-}
        
instance Propositional Flag    

instance Lattice.JoinSemiLattice Flag where
    (\/) x y = ifelse (x || y) On Off
    {-# INLINE (\/) #-}

instance Lattice.MeetSemiLattice Flag where
    (/\) x y = ifelse (x && y) On Off
    {-# INLINE (/\) #-}

instance ToBit Flag where
    bit = Bit    
    
instance Lattice Flag where                

instance Universe Flag where
    inhabitants = set ivalues

instance Implicative Bit where
    (Bit a) ==> (Bit b) = a ==> b
        
instance ToInt Bit where
    int (Bit flag) = ifelse (flag == On) 1 0
    {-# INLINE int #-}

instance ToInteger Bit where
    integer (Bit flag) = ifelse (flag == On) 1 0
    {-# INLINE integer #-}

instance ToNatural Bit where
    natural (Bit flag) = ifelse (flag == On) 1 0
    {-# INLINE natural #-}
        
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

instance Negatable Bit where 
    negate (Bit On) = off
    negate (Bit Off) = on
    
    {-# INLINE negate #-}

instance Subtractive Bit where
    sub x = negate . add x
    {-# INLINE sub #-}
        
instance Semigroup Bit where
    (<>) = (+)
    {-# INLINE (<>) #-}    

instance Monoid Bit where 
    mempty = off
    {-# INLINE mempty #-}

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
    show = string . format

instance Bounded Bit where
    minBound = off
    maxBound = on
    
instance Enum Bit where
    fromEnum (Bit b) = fromEnum b
    {-# INLINE fromEnum #-}

    toEnum = Bit . toEnum
    {-# INLINE toEnum #-}

instance Universe Bit where
    inhabitants = set ivalues
    
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

    complement   = negate
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

instance Concatenable Word8 where
    concat x y = bitsplat 8 x y
            
instance Concatenable Word16 where    
    concat x y = bitsplat 16 x y

instance Concatenable Word32 where
    concat x y = bitsplat 32 x y

instance Concatenable Int8 where
    concat x y = bitsplat 8 x y
            
instance Concatenable Int16 where
    concat x y = bitsplat 16 x y

instance Concatenable Int32 where
    concat x y = bitsplat 32 x y    
    
instance Formattable BitString where
    format (BitString bits) =  format <$> bits |> collapse |> prefix n
        where n = ((length bits)::Int) |> format |> parenthetical |> pad
                
instance ToInteger BitString where
    integer = undefined

instance Show BitString where
    show = string . format

-------------------------------------------------------------------------------
-- **Indexable instances
-------------------------------------------------------------------------------    
instance Indexable Integer where
    idx = ibit
    {-# INLINE idx #-}

instance Indexable Natural where
    idx = ibit
    {-# INLINE idx #-}

instance Indexable Word where
    idx = ibit
    {-# INLINE idx #-}

instance Indexable Int where
    idx = ibit
    {-# INLINE idx #-}
        
instance Indexable Int8 where
    idx = ibit
    {-# INLINE idx #-}

instance Indexable Int16 where
    idx = ibit
    {-# INLINE idx #-}

instance Indexable Int32 where
    idx = ibit
    {-# INLINE idx #-}
        
instance Indexable Int64 where
    idx = ibit
    {-# INLINE idx #-}
        
instance Indexable Word8 where
    idx = ibit
    {-# INLINE idx #-}

instance Indexable Word16 where
    idx = ibit
    {-# INLINE idx #-}

instance Indexable Word32 where
    idx = ibit
    {-# INLINE idx #-}
        
instance Indexable Word64 where
    idx = ibit
    {-# INLINE idx #-}
    
-------------------------------------------------------------------------------
-- **Bitfield instances
-------------------------------------------------------------------------------    
instance Bitfield Integer
instance Bitfield Natural
instance Bitfield Word
instance Bitfield Int
instance Bitfield Int8
instance Bitfield Int16
instance Bitfield Int32
instance Bitfield Int64
instance Bitfield Word8
instance Bitfield Word16
instance Bitfield Word32
instance Bitfield Word64

-------------------------------------------------------------------------------
-- **FiniteBitfield instances
-------------------------------------------------------------------------------    
instance FiniteBitfield Word where
    bitcount = finiteBitSize
instance FiniteBitfield Int where
    bitcount = finiteBitSize
instance FiniteBitfield Int8 where
    bitcount = finiteBitSize
instance FiniteBitfield Int16 where
    bitcount = finiteBitSize
instance FiniteBitfield Int32 where
    bitcount = finiteBitSize
instance FiniteBitfield Int64 where
    bitcount = finiteBitSize
instance FiniteBitfield Word8 where
    bitcount = finiteBitSize
instance FiniteBitfield Word16 where
    bitcount = finiteBitSize
instance FiniteBitfield Word32 where
    bitcount = finiteBitSize
instance FiniteBitfield Word64 where
    bitcount = finiteBitSize
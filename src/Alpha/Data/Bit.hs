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
    BitString,
    on, isOn,
    off, isOff,
    bitstring,
)
where
import Alpha.Canonical
import Data.Bits(Bits(..))
import qualified Data.List as List

-- | Taken from bool8
data {-# CTYPE "HsBool" #-} Flag = On | Off
    deriving (Eq, Enum, Ord, Generic, Data, Typeable, Read, Bounded)

newtype Bit = Bit Flag
    deriving (Eq, Ord, Generic, Data, Typeable, Read, 
        Invertive, Implication, ExclusivelyDisjunct, Biconditional, 
        Propositional, JoinSemiLattice,MeetSemiLattice,  Lattice
        )

-- | Encapsulates an ordered sequence of bits
newtype BitString = BitString [Bit]   
    deriving (Eq, Ord, Generic, Data, Typeable, Read)
instance Newtype (BitString)
        
type instance Unsigned Bit = Bit
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
bitstring i = BitString $ bitstring' (quotRem i 2) []  where
    bitstring' (n,d) r = seq c $
        case n of
        0 -> r'
        _ -> bitstring' (quotRem n 2) r' 
        where
            c  = ifelse (d == 0) off on
            r' = c : r            


instance Unsignable Bit

instance Disjunctive (Bit) where
    (Bit x) || (Bit y) = x || y

instance Conjunctive (Bit) where
    (Bit x) && (Bit y) = x && y

instance Disjunctive Flag where
    On || On = True
    On || Off = True
    Off || On = True
    Off || Off = False
    {-# INLINE (||) #-}

instance ExclusivelyDisjunct Flag where
    lxor On On = False
    lxor On Off = True
    lxor Off On = True
    lxor Off Off = False
    {-# INLINE lxor #-}
    
instance Conjunctive Flag where
    On && On = True
    On && Off = False
    Off && On = False
    Off && Off = False
    {-# INLINE (&&) #-}

instance Implication Flag where
    implies On On = True
    implies On Off = False
    implies Off On = True
    implies Off Off = True
    {-# INLINE implies #-}

instance Biconditional Flag where
    iff On On = True
    iff On Off = False
    iff Off On = False
    iff Off Off = True
    {-# INLINE iff #-}
        
instance Invertive Flag where
    not On = False
    not Off = True
    {-# INLINE not #-}
        
instance Propositional Flag    

instance JoinSemiLattice Flag where
    (\/) x y = ifelse (x || y) On Off
    {-# INLINE (\/) #-}

instance MeetSemiLattice Flag where
    (/\) x y = ifelse (x && y) On Off
    {-# INLINE (/\) #-}

instance Lattice Flag where                

instance Universe Flag where
    inhabitants = FiniteSet (fromList enumerate)
    
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
    inhabitants = FiniteSet (fromList enumerate)
    
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

instance Formattable BitString where
    format (BitString bits) =  format <$> bits |> append |> prefix n
        where n =  ((length bits)::Int) |> format |> parenthetical |> pad
                
instance ToInteger BitString where
    integer = undefined

instance Show BitString where
    show = string . format
                
instance Indexable Integer where
    idx = ibit
    {-# INLINE idx #-}

instance Indexable Natural where
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
    
module Alpha.Canonical.Numeric.Conversion
(
    FromDouble(..), ToDouble(..),
    FromInt(..), ToInt(..),
    ToWord(..), FromWord(..),
    ToInteger(..),
    ToNatural(..), FromNatural(..),
    Doubly(..),
    ToIntegral(..),
    int8, int16, int32, int64,
    word8, word16, word32, word64,
    rational, fractional,

) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Numeric.Class
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations

import qualified Data.Ratio as DR

-- | Characterizies a type whose values can be converted to/from 'Double' values    
type Doubly a = (ToDouble a, FromDouble a)

-- | Characterizies a type whose values can be materialized from 'Double' values
class FromDouble d where
    -- | Converts a 'Double' value to a 'd' value
    fromDouble::Double -> d

-- | Characterizies a type whose values can be converted to 'Double' values
class ToDouble d where
    -- / Converts a 'd' value to a 'Double' value
    double::d -> Double

-- | Characterizies a type whose values can be converted to machine-sized 'Int' values
class ToInt d where
    int::d -> Int  

-- | Characterizies a type whose values can be materialized from machine-sized 'Int' values
class FromInt a where
    fromInt::Int -> a
    
class ToIntegral a where
    integral::(Integral b) => a -> b
        
-- | Characterizies a type whose values can be converted to arbitrary-sized 'Integer' values
class ToInteger d where
    integer::d -> Integer

-- | Characterizies a type whose values can be converted to machine-sized 'Word' values
class ToWord d where
    word::d -> Word

-- | Characterizies a type whose values can be materialized from machine-sized 'Word' values
class FromWord a where
    fromWord::Word -> a

-- | Characterizies a type whose values can be converted to 'Natural' values
class ToNatural d where
    natural::d -> Natural

-- | Characterizies a type whose values can be materialized from 'Natural' values
class FromNatural a where
    fromNatural::Natural -> a


-- | Constructs a list of Int values from a list of Integral values
ints::(ToInt n) => [n] -> [Int]
ints src = int <$> src

-- | Constructs a list of Word values from a list of Integral values
words::(ToWord n) => [n] -> [Word]
words src = word <$> src

-- | Constructs a list of Integer values from a list of Integral values
integers::(ToInteger n) => [n] -> [Integer]
integers src = integer <$> src

-- | Constructs a 'Int8' from an integral value
int8::(Integral n) => n -> Int8
int8 n = convert n

-- | Constructs a 'Int16' from an integral value
int16::(Integral n) => n -> Int16
int16 n = convert n

-- | Constructs a 'Int32' from an integral value
int32::(Integral n) => n -> Int32
int32 n = convert n

-- | Constructs a 'Int64' from an integral value
int64::(Integral n) => n -> Int64
int64 n = convert n

-- | Constructs a 'Word8' from an integral value
word8::(Integral n) => n -> Word8
word8 n = convert n

-- | Constructs a 'Word16' from an integral value
word16::(Integral n) => n -> Word16
word16 n = convert n

-- | Constructs a 'Word32' from an integral value
word32::(Integral n) => n -> Word32
word32 n = convert n

-- | Constructs a 'Word64' from an integral value
word64::(Integral n) => n -> Word64
word64 n = convert n
{-# INLINE word64 #-}


-- Forms a 'Rational' number from a 'Real' number
rational::(Real r) => r -> (Ratio Integer)
rational x = toRational' x
{-# INLINE rational #-} 

-- Produces a 'Fractional' number from a 'Real' number
fractional::(Real r, Fractional f) => r -> f
fractional = realToFrac'
{-# INLINE fractional #-} 
    
instance ToIntegral Int where
    integral = fromIntegral    
instance ToIntegral Int8 where
    integral = fromIntegral    
instance ToIntegral Int16 where
    integral = fromIntegral    
instance ToIntegral Int32 where
    integral = fromIntegral    
instance ToIntegral Int64 where
    integral = fromIntegral    
instance ToIntegral Integer where
    integral = fromIntegral    
instance ToIntegral Word where
    integral = fromIntegral    
instance ToIntegral Word8 where
    integral = fromIntegral    
instance ToIntegral Word16 where
    integral = fromIntegral    
instance ToIntegral Word32 where
    integral = fromIntegral    
instance ToIntegral Word64 where
    integral = fromIntegral    
instance ToIntegral Natural where
    integral = fromIntegral    

-- FromDouble
-------------------------------------------------------------------------------
instance FromDouble Natural where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Integer where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Int where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Int8 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Int16 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Int32 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Int64 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Word where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Word8 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Word16 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Word32 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Word64 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Float where 
    fromDouble = double2Float
    {-# INLINE fromDouble #-}    
instance FromDouble Double where 
    fromDouble = id
    {-# INLINE fromDouble #-}    
instance FromDouble CFloat where 
    fromDouble = realToFrac
    {-# INLINE fromDouble #-}    
instance FromDouble CDouble where 
    fromDouble = realToFrac
    {-# INLINE fromDouble #-}    

-- To Double
-------------------------------------------------------------------------------
instance ToDouble Natural where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Integer where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Int where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Int8 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Int16 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Int32 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Int64 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Word where 
    double= fromIntegral
    {-# INLINE double #-}    
instance ToDouble Word8 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Word16 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Word32 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Word64 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance (Integral a, ToDouble a) => ToDouble (Ratio a) where 
    double x = divf n d where
        n = double <| numerator' x
        d = double <| denominator' x        
    {-# INLINE double #-}        
instance ToDouble Float where 
    double = float2Double
    {-# INLINE double #-}    
instance ToDouble Double where 
    double = id
    {-# INLINE double #-}    
instance ToDouble CFloat where 
    double = realToFrac
    {-# INLINE double #-}    
instance ToDouble CDouble where 
    double = realToFrac
    {-# INLINE double #-}    

-- ToInt
-------------------------------------------------------------------------------
instance ToInt Natural where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Integer where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Int where 
    int = id
    {-# INLINE int #-}
instance ToInt Int8 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Int16 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Int32 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Int64 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Word where 
    int= fromIntegral
    {-# INLINE int #-}
instance ToInt Word8 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Word16 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Word32 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Word64 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Float where 
    int = truncate
    {-# INLINE int #-}
instance ToInt Double where 
    int = fromDouble
    {-# INLINE int #-}
instance ToInt CFloat where 
    int = truncate
    {-# INLINE int #-}
instance ToInt CDouble where 
    int = truncate
    {-# INLINE int #-}

-- FromInt
-------------------------------------------------------------------------------
instance FromInt Natural where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Integer where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Int where 
    fromInt = id
    {-# INLINE fromInt #-}
instance FromInt Int8 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Int16 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Int32 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Int64 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Word where 
    fromInt= fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Word8 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Word16 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Word32 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Word64 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Float where 
    fromInt = realToFrac
    {-# INLINE fromInt #-}
instance FromInt Double where 
    fromInt = realToFrac
    {-# INLINE fromInt #-}
instance FromInt CFloat where 
    fromInt = realToFrac
    {-# INLINE fromInt #-}
instance FromInt CDouble where 
    fromInt = realToFrac
    {-# INLINE fromInt #-}

-- ToInteger
-------------------------------------------------------------------------------
instance ToInteger Natural where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Integer where 
    integer = id
    {-# INLINE integer #-}
instance ToInteger Int where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Int8 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Int16 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Int32 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Int64 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Word where 
    integer= fromIntegral
    {-# INLINE integer #-}
instance ToInteger Word8 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Word16 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Word32 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Word64 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Float where 
    integer = truncate
    {-# INLINE integer #-}
instance ToInteger Double where 
    integer = fromDouble
    {-# INLINE integer #-}
instance ToInteger CFloat where 
    integer = truncate
    {-# INLINE integer #-}
instance ToInteger CDouble where 
    integer = truncate
    {-# INLINE integer #-}

-- ToWord
-------------------------------------------------------------------------------
instance ToWord Natural where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Integer where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Int where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Int8 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Int16 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Int32 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Int64 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Word where 
    word = id
    {-# INLINE word #-}
instance ToWord Word8 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Word16 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Word32 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Word64 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Float where 
    word = truncate
    {-# INLINE word #-}
instance ToWord Double where 
    word = fromDouble
    {-# INLINE word #-}
instance ToWord CFloat where 
    word = truncate
    {-# INLINE word #-}
instance ToWord CDouble where 
    word = truncate
    {-# INLINE word #-}

-- FromNatural
-------------------------------------------------------------------------------
instance FromNatural Natural where 
    fromNatural = id
    {-# INLINE fromNatural #-}
instance FromNatural Integer where 
    fromNatural = fromIntegral
    {-# INLINE fromNatural #-}
instance FromNatural Int where 
    fromNatural = fromIntegral
    {-# INLINE fromNatural #-}
instance FromNatural Int8 where 
    fromNatural = fromIntegral
    {-# INLINE fromNatural #-}
instance FromNatural Int16 where 
    fromNatural = fromIntegral
    {-# INLINE fromNatural #-}
instance FromNatural Int32 where 
    fromNatural = fromIntegral
    {-# INLINE fromNatural #-}
instance FromNatural Int64 where 
    fromNatural = fromIntegral
    {-# INLINE fromNatural #-}
instance FromNatural Word where 
    fromNatural = fromIntegral
    {-# INLINE fromNatural #-}
instance FromNatural Word8 where 
    fromNatural = fromIntegral
    {-# INLINE fromNatural #-}
instance FromNatural Word16 where 
    fromNatural = fromIntegral
    {-# INLINE fromNatural #-}
instance FromNatural Word32 where 
    fromNatural = fromIntegral
    {-# INLINE fromNatural #-}
instance FromNatural Word64 where 
    fromNatural = fromIntegral
    {-# INLINE fromNatural #-}
instance FromNatural Float where 
    fromNatural = realToFrac
    {-# INLINE fromNatural #-}
instance FromNatural Double where 
    fromNatural = realToFrac
    {-# INLINE fromNatural #-}
instance FromNatural CFloat where 
    fromNatural = realToFrac
    {-# INLINE fromNatural #-}
instance FromNatural CDouble where 
    fromNatural = realToFrac
    {-# INLINE fromNatural #-}

-- ToNatural
-------------------------------------------------------------------------------
instance ToNatural Natural where 
    natural = id
    {-# INLINE natural #-}
instance ToNatural Integer where 
    natural = fromIntegral
    {-# INLINE natural #-}
instance ToNatural Int where 
    natural = fromIntegral
    {-# INLINE natural #-}
instance ToNatural Int8 where 
    natural = fromIntegral
    {-# INLINE natural #-}
instance ToNatural Int16 where 
    natural = fromIntegral
    {-# INLINE natural #-}
instance ToNatural Int32 where 
    natural = fromIntegral
    {-# INLINE natural #-}
instance ToNatural Int64 where 
    natural = fromIntegral
    {-# INLINE natural #-}
instance ToNatural Word where 
    natural = fromIntegral
    {-# INLINE natural #-}
instance ToNatural Word8 where 
    natural = fromIntegral
    {-# INLINE natural #-}
instance ToNatural Word16 where 
    natural = fromIntegral
    {-# INLINE natural #-}
instance ToNatural Word32 where 
    natural = fromIntegral
    {-# INLINE natural #-}
instance ToNatural Word64 where 
    natural = fromIntegral
    {-# INLINE natural #-}
instance ToNatural Float where 
    natural = truncate
    {-# INLINE natural #-}
instance ToNatural Double where 
    natural = fromDouble
    {-# INLINE natural #-}
instance ToNatural CFloat where 
    natural = truncate
    {-# INLINE natural #-}
instance ToNatural CDouble where 
    natural = truncate
    {-# INLINE natural #-}

instance (Integral a, Num b) => Convertible a b where
    convert = fromIntegral
    
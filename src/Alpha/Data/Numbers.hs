-----------------------------------------------------------------------------
-- | Defines the elementary number-related API surface
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Alpha.Data.Numbers
(    
    SizedInt(..), 
    SizedWord(..),
    Integer,
    Integral(quot, rem, div, mod, quotRem, divMod),
    Double , Double#, Float, Float#,
    Num(abs, signum),
    FromDouble(..), ToDouble(..),

    --intn, wordn,    
    int, int8, int16, int32, int64,
    word, word8, word16, word32, word64,
    divides, modpart,
    fromIntegral,
    fromInteger,
    sub'

)
where
import Data.Word 
import Data.Int
import Data.Bits 
import Data.Kind(Type)
import qualified Data.Text as Text
import GHC.Num 
import GHC.Natural
import GHC.Real
import GHC.TypeLits
import GHC.Float(double2Float,float2Double)
import qualified GHC.Num as N
import qualified Data.List as L

import Numeric
import Alpha.Base hiding(zero)
import Alpha.Text.Format
import Alpha.Canonical hiding((+),(*),(-),range)

class FromDouble d where
    -- / Converts a 'Double' value to a 'd' value
    fromDouble::Double -> d

class ToDouble d where
    -- / Converts a 'd' value to a 'Double' value
    toDouble::d -> Double
    

type family Unsigned i :: * where
    Unsigned Int8 = Word8
    Unsigned Int16 = Word16
    Unsigned Int32 = Word32
    Unsigned Int64 = Word64
    Unsigned Word8 = Word8
    Unsigned Word16 = Word16
    Unsigned Word32 = Word32
    Unsigned Word64 = Word64

type family SizedInt (n::Nat) where
    SizedInt 8 = Int8
    SizedInt 16 = Int16
    SizedInt 32 = Int32
    SizedInt 64 = Int64

type family SizedWord (n::Nat) | n -> n where
    SizedWord 8 = Word8
    SizedWord 16 = Word16
    SizedWord 32 = Word32
    SizedWord 64 = Word64    
        -- | Canonical 'Int' constructor for machine-sized integers
int::(Integral n) => n -> Int
int n = convert n

-- | Canonical 'Integer' constructor for unbounded integers
integer::(Integral n) => n -> Integer
integer = fromIntegral

-- | Constructs a list of Int values from a list of Integral values
ints::(Integral n) => [n] -> [Int]
ints src = fmap int src

-- | Canonical 'Word' constructor for machine-sized words
word::(Integral n) => n -> Word
word n = convert n

-- | Constructs a list of Word values from a list of Integral values
words::(Integral n) => [n] -> [Word]
words src = fmap word src

-- | Constructs a list of Integer values from a list of Integral values
integers::(Integral n) => [n] -> [Integer]
integers src = fmap integer src

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

-- | Constructs an 'Unsigned' value
-- unsigned::(Integral i, Num (Unsigned i)) => i -> Unsigned i
-- unsigned = convert    

-- | Constructs a 'SizedWord' value
-- wordn::(Integral i, Num (SizedWord n)) => i -> SizedWord n
-- wordn = convert


-- intn::(Num (SizedInt n), Integral i) => i -> SizedInt n
-- intn = convert

-- Determines whether m is evenly divisible by n
divides::(Integral a) => a -> a -> Bool
divides m n = (m `mod` n) == 0

-- Calculates the points within the interval that are
-- divisible by n
modpart::(Integral a, Ix a) => (a, a) -> a -> [a]
modpart (min,max) n 
    = range (min, max) 
      |> fmap (\j -> case divides j n of True -> j; _ -> 0)
      |> L.filter (\j -> j /= 0)

-- | Constructs a Num-typed 0 value      
zed::(Num a) => a
zed = 0

-- | Constructs a Num-typed 1 value      
unity::(Num a) => a
unity = 1

sub'::(Num a) => a -> a -> a
sub' x y = x - y
      
instance (Integral a, Num b) => Convertible a b where
    convert = fromIntegral


-- instance Convertible Double Int where
--     convert = round

instance (SignedIntegral a) => Invertible a a where 
    invert x = zed - x

instance TotalOrder Natural
instance TotalOrder Integer
instance TotalOrder Int
instance TotalOrder Int8
instance TotalOrder Int16
instance TotalOrder Int32
instance TotalOrder Int64
instance TotalOrder Word
instance TotalOrder Word8
instance TotalOrder Word16
instance TotalOrder Word32
instance TotalOrder Word64
instance TotalOrder Float
instance TotalOrder Double

instance Subtractive Natural where sub = (-)
instance Subtractive Integer where sub = (-)
instance Subtractive Int where sub = (-)
instance Subtractive Int8 where sub = (-)
instance Subtractive Int16 where sub = (-)
instance Subtractive Int32 where sub = (-)
instance Subtractive Int64 where sub = (-)
instance Subtractive Word where sub = (-)
instance Subtractive Word8 where sub = (-)
instance Subtractive Word16 where sub = (-)
instance Subtractive Word32 where sub = (-)
instance Subtractive Word64 where sub = (-)
instance Subtractive Float where sub = (-)
instance Subtractive Double where sub = (-)

instance Additive Natural where add = (+)
instance Additive Integer where add = (+)
instance Additive Int where add = (+)
instance Additive Int8 where add = (+)
instance Additive Int16 where add = (+)
instance Additive Int32 where add = (+)
instance Additive Int64 where add = (+)
instance Additive Word where add = (+)
instance Additive Word8 where add = (+)
instance Additive Word16 where add = (+)
instance Additive Word32 where add = (+)
instance Additive Word64 where add = (+)
instance Additive Float where add = (+)
instance Additive Double where add = (+)

instance Semigroup Natural where (<>) = (+)
instance Semigroup Integer where (<>) = (+)
instance Semigroup Int where (<>) = (+)
instance Semigroup Int8 where (<>) = (+)
instance Semigroup Int16 where (<>) = (+)
instance Semigroup Int32 where (<>) = (+)
instance Semigroup Int64 where (<>) = (+)
instance Semigroup Word where (<>) = (+)
instance Semigroup Word8 where (<>) = (+)
instance Semigroup Word16 where (<>) = (+)
instance Semigroup Word32 where (<>) = (+)
instance Semigroup Word64 where (<>) = (+)
instance Semigroup Float where (<>) = (+)
instance Semigroup Double where (<>) = (+)

instance Multiplicative Natural where mul = (*)
instance Multiplicative Integer where mul = (*)
instance Multiplicative Int where mul = (*)
instance Multiplicative Int8 where mul = (*)
instance Multiplicative Int16 where mul = (*)
instance Multiplicative Int32 where mul = (*)
instance Multiplicative Int64 where mul = (*)
instance Multiplicative Word where mul = (*)
instance Multiplicative Word8 where mul = (*)
instance Multiplicative Word16 where mul = (*)
instance Multiplicative Word32 where mul = (*)
instance Multiplicative Word64 where mul = (*)
instance Multiplicative Float where mul = (*)
instance Multiplicative Double where mul = (*)

instance Nullary Natural where zero = zed
instance Nullary Integer where zero = zed
instance Nullary Int where zero = zed
instance Nullary Int8 where zero = zed
instance Nullary Int16 where zero = zed
instance Nullary Int32 where zero = zed
instance Nullary Int64 where zero = zed
instance Nullary Word where zero = zed
instance Nullary Word8 where zero = zed
instance Nullary Word16 where zero = zed
instance Nullary Word32 where zero = zed
instance Nullary Word64 where zero = zed
instance Nullary Float where zero = zed
instance Nullary Double where zero = zed

instance Monoid Natural where mempty = zero
instance Monoid Integer where mempty = zero
instance Monoid Int where mempty = zero
instance Monoid Int8 where mempty = zero
instance Monoid Int16 where mempty = zero
instance Monoid Int32 where mempty = zero
instance Monoid Int64 where mempty = zero
instance Monoid Word where mempty = zero
instance Monoid Word8 where mempty = zero
instance Monoid Word16 where mempty = zero
instance Monoid Word32 where mempty = zero
instance Monoid Word64 where mempty = zero
instance Monoid Float where mempty = zero
instance Monoid Double where mempty = zero

instance Unital Natural where one = unity
instance Unital Integer where one = unity
instance Unital Int where one = unity
instance Unital Int8 where one = unity
instance Unital Int16 where one = unity
instance Unital Int32 where one = unity
instance Unital Int64 where one = unity
instance Unital Word where one = unity
instance Unital Word8 where one = unity
instance Unital Word16 where one = unity
instance Unital Word32 where one = unity
instance Unital Word64 where one = unity
instance Unital Float where one = unity
instance Unital Double where one = unity

instance Group Integer
instance Group Int
instance Group Int8
instance Group Int16
instance Group Int32
instance Group Int64

instance Abelian Integer
instance Abelian Int
instance Abelian Int8
instance Abelian Int16
instance Abelian Int32
instance Abelian Int64

instance Ring Integer
instance Ring Int
instance Ring Int8
instance Ring Int16
instance Ring Int32
instance Ring Int64

instance Invertible Word Int where invert x = zero - (int x)
instance Invertible Word8 Int8 where invert x = zero - (int8 x)
instance Invertible Word16 Int16 where invert x = zero - (int16 x)
instance Invertible Word32 Int32 where invert x = zero - (int32 x)
instance Invertible Word64 Int64 where invert x = zero - (int64 x)
instance Invertible Float Float where invert x = zero - x
instance Invertible Double Double where invert x = zero - x

instance Negatable Natural Integer where negate x = zero - (integer x)
instance Negatable Integer Integer where negate x = zero - x
instance Negatable Int Int where negate x = zero - x
instance Negatable Int8 Int8 where negate x = zero - x
instance Negatable Int16 Int16 where negate x = zero - x
instance Negatable Int32 Int32 where negate x = zero - x
instance Negatable Int64 Int64 where negate x = zero - x
instance Negatable Word Int where negate x = zero- (int x)
instance Negatable Word8 Int8 where negate x = zero - (int8 x)
instance Negatable Word16 Int16 where negate x = zero - (int16 x)
instance Negatable Word32 Int32 where negate x = zero - (int32 x)
instance Negatable Word64 Int64 where negate x = zero- (int64 x)
instance Negatable Float Float where negate x = zero - x
instance Negatable Double Double where negate x = zero - x

instance SignedIntegral Integer
instance SignedIntegral Int
instance SignedIntegral Int8
instance SignedIntegral Int16
instance SignedIntegral Int32
instance SignedIntegral Int64

instance UnsignedIntegral Word
instance UnsignedIntegral Word8
instance UnsignedIntegral Word16
instance UnsignedIntegral Word32
instance UnsignedIntegral Word64

instance FromDouble Natural where fromDouble = truncate
instance FromDouble Integer where fromDouble = truncate
instance FromDouble Int where fromDouble = truncate
instance FromDouble Int8 where fromDouble = truncate
instance FromDouble Int16 where fromDouble = truncate
instance FromDouble Int32 where fromDouble = truncate
instance FromDouble Int64 where fromDouble = truncate
instance FromDouble Word where fromDouble = truncate
instance FromDouble Word8 where fromDouble = truncate
instance FromDouble Word16 where fromDouble = truncate
instance FromDouble Word32 where fromDouble = truncate
instance FromDouble Word64 where fromDouble = truncate
instance FromDouble Float where fromDouble = double2Float
instance FromDouble Double where fromDouble = id

instance ToDouble Natural where toDouble = fromIntegral
instance ToDouble Integer where toDouble = fromIntegral
instance ToDouble Int where toDouble = fromIntegral
instance ToDouble Int8 where toDouble = fromIntegral
instance ToDouble Int16 where toDouble = fromIntegral
instance ToDouble Int32 where toDouble = fromIntegral
instance ToDouble Int64 where toDouble = fromIntegral
instance ToDouble Word where toDouble = fromIntegral
instance ToDouble Word8 where toDouble = fromIntegral
instance ToDouble Word16 where toDouble = fromIntegral
instance ToDouble Word32 where toDouble = fromIntegral
instance ToDouble Word64 where toDouble = fromIntegral
instance ToDouble Float where toDouble = float2Double
instance ToDouble Double where toDouble = id

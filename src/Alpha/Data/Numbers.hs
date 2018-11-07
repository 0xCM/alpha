-----------------------------------------------------------------------------
-- | Defines the integer-related API surface
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
    BigInt,
    Base, 
    BasedInt,
    SizedInt(..), 
    SizedWord(..),
    Integer,
    Integral(quot, rem, div, mod, quotRem, divMod),
    Double , Double#, Float, Float#,
    Num(abs, signum),

    intn, wordn,
    based, base2, base10, base16,
    int, bigint, 
    int8, int16, int32, int64,
    word8, word16, word32, word64,
    divides, modpart,
    fromIntegral,
    fromInteger,
    zed, sub'
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
import qualified GHC.Num as N
import qualified Data.List as L

import Numeric
import Alpha.Base
import Alpha.Text.Format
import Alpha.Canonical hiding((+),(*),(-),range)
    
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
        
-- expandWord::SizedWord n -> SizedWord (n + n)
-- expandWord  = coerce 

-- | An integer of arbitrary size
type BigInt = Integer

data Base (n::Nat) = Base

data BasedInt (n::Nat) i = BasedInt !i

based::forall (n::Nat) i. (Integral i) => i -> BasedInt n i
based i = BasedInt i

base2 = based @2

base10 = based @10

base16 = based @16

-- | Canonical 'Int' constructor for machine-sized integers
int::(Integral n) => n -> Int
int n = convert n

-- | Constructs a list of Int values from a list of Integral values
ints::(Integral n) => [n] -> [Int]
ints src = fmap int src

-- | Canonical 'Word' constructor for machine-sized words
word::(Integral n) => n -> Word
word n = convert n

-- | Constructs a list of Word values from a list of Integral values
words::(Integral n) => [n] -> [Word]
words src = fmap word src

-- | Canonical 'Integer' constructor
integer::(Integral n) => n -> Integer
integer n = fromIntegral n

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

-- | Constructs a 'BigInt' from an integral value
bigint::(Integral n) => n -> BigInt
bigint n = convert n

unsigned::(Integral i, Num (Unsigned i)) => i -> Unsigned i
unsigned = convert    

wordn::(Integral i, Num (SizedWord n)) => i -> SizedWord n
wordn = convert

intn::(Num (SizedInt n), Integral i) => i -> SizedInt n
intn = convert

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

zed::(Num a) => a
zed = 0

sub'::(Num a) => a -> a -> a
sub' x y = x - y
      
instance (Integral a, Num b) => Convertible a b where
    convert = fromIntegral

instance Nullary Int where zero = 0    
instance Semigroup Int where (<>) = (+)
instance Monoid Int where mempty = zero
instance Invertible Int Int where invert x = zero - x
instance Additive Int where add = (+)
instance Multiplicative Int where mul = (*)
instance Subtractive Int where sub = (-)
instance Group Int
instance TotalOrder Int

instance Nullary Integer where zero = 0
instance Semigroup Integer where (<>) = (+)
instance Monoid Integer where mempty = zero   
instance Invertible Integer Integer where invert x = zero - x
instance Additive Integer where add = (+)
instance Multiplicative Integer where mul = (*)
instance Subtractive Integer where sub = (-)
instance Group Integer
instance TotalOrder Integer

instance Nullary Int8 where zero = 0
instance Semigroup Int8 where (<>) = (+)
instance Monoid Int8 where mempty = zero
instance Invertible Int8 Int8 where invert x = zero - x
instance Additive Int8 where add = (+)
instance Multiplicative Int8 where mul = (*)
instance Subtractive Int8 where sub = (-)
instance Group Int8
instance TotalOrder Int8

instance Nullary Int16 where zero = 0
instance Semigroup Int16 where (<>) = (+)
instance Monoid Int16 where mempty = zero
instance Invertible Int16 Int16 where invert x = zero - x
instance Additive Int16 where add = (+)
instance Multiplicative Int16 where mul = (*)
instance Subtractive Int16 where sub = (-)
instance Group Int16
instance TotalOrder Int16

instance Nullary Int32 where zero = 0
instance Semigroup Int32 where (<>) = (+)
instance Monoid Int32 where mempty = zero
instance Invertible Int32 Int32 where invert x = zero - x    
instance Additive Int32 where add = (+)
instance Multiplicative Int32 where mul = (*)
instance Subtractive Int32 where sub = (-)
instance Group Int32
instance TotalOrder Int32

instance Nullary Int64 where zero = 0
instance Semigroup Int64 where (<>) = (+)
instance Monoid Int64 where mempty = zero
instance Invertible Int64 Int64  where invert x = zero - x
instance Additive Int64 where add = (+)
instance Multiplicative Int64 where mul = (*)
instance Subtractive Int64 where sub = (-)
instance Group Int64
instance TotalOrder Int64

instance Nullary Word where zero = 0
instance Invertible Word Int where invert x = zero - (fromIntegral x)
instance Additive Word where add = (+)
instance Multiplicative Word where mul = (*)
instance Subtractive Word where sub = (-)
instance TotalOrder Word

instance Nullary Word8 where zero = 0
instance Invertible Word8 Int8 where invert x = zero - (fromIntegral x)
instance Additive Word8 where add = (+)
instance Multiplicative Word8 where mul = (*)
instance Subtractive Word8 where sub = (-)
instance TotalOrder Word8

instance Nullary Word16 where zero = 0
instance Invertible Word16 Int16 where invert x = zero - (fromIntegral x)
instance Additive Word16 where add = (+)
instance Multiplicative Word16 where mul = (*)
instance Subtractive Word16 where sub = (-)
instance TotalOrder Word16

instance Nullary Word32 where zero = 0
instance Invertible Word32 Int32 where invert x = zero - (fromIntegral x)
instance Additive Word32 where add = (+)
instance Multiplicative Word32 where mul = (*)
instance Subtractive Word32 where sub = (-)
instance TotalOrder Word32

instance Nullary Word64 where zero = 0
instance Invertible Word64 Int64 where invert x = zero - (fromIntegral x)
instance Additive Word64 where add = (+)
instance Multiplicative Word64 where mul = (*)
instance Subtractive Word64 where sub = (-)
instance TotalOrder Word64

instance Nullary Double where zero = 0
instance Invertible Double Double where invert x = zero - x
instance Additive Double where add = (+)
instance Multiplicative Double where mul = (*)
instance Subtractive Double where sub = (-)

instance Nullary Float where zero = 0
instance Invertible Float Float where invert x = zero - x
instance Additive Float where add = (+)
instance Multiplicative Float where mul = (*)
instance Subtractive Float where sub = (-)

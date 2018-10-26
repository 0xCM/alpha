{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Alpha.Data.Numbers
(    
    --module Numeric,
    -- *GHC.Float
    Double , Double#, Float, Float#,
    
    -- *GHC.Num
    Num(abs, signum),
    Integer,
    Integral(quot, rem, div, mod, quotRem, divMod),
    fromIntegral,
    
    BigInt,
    int, bigint, 
    int8, int16, int32, int64,
    word8, word16, word32, word64,

    SizedInt(..), SizedWord(..),
    int', word',
    zed, sub',
    divides, modpart
)
where

import Data.Word 
import Data.Int
import Data.Bits 
import Data.Kind(Type)
import qualified Data.Text as Text
import GHC.Num 
import GHC.Float
import GHC.Natural
import GHC.Real
import qualified GHC.Num as N
import qualified Data.List as L

import Numeric
import Alpha.Base
import Alpha.Text.Format
import Alpha.Canonical hiding((+),(*),(-))


-- | An integer of arbitrary size
type BigInt = Integer


-- convert :: (Integral a, Num b) => a -> b
-- convert = fromIntegral

instance (Integral a, Num b) => Convertible a b where
    convert = fromIntegral

-- | Canonical 'Int' constructor for machine-sized integers
int :: (Integral n) => n -> Int
int n = convert n

-- | Canonical 'Word' constructor for machine-sized words
word :: (Integral n) => n -> Word
word n = convert n


-- | Canonical 'Integer' constructor
integer::(Integral n) => n -> Integer
integer n = fromIntegral n

-- | Constructs a 'Int8' from an integral value
int8 :: (Integral n) => n -> Int8
int8 n = convert n

-- | Constructs a 'Int16' from an integral value
int16 :: (Integral n) => n -> Int16
int16 n = convert n

-- | Constructs a 'Int32' from an integral value
int32 :: (Integral n) => n -> Int32
int32 n = convert n

-- | Constructs a 'Int64' from an integral value
int64 :: (Integral n) => n -> Int64
int64 n = convert n

-- | Constructs a 'Word8' from an integral value
word8 :: (Integral n) => n -> Word8
word8 n = convert n

-- | Constructs a 'Word16' from an integral value
word16 :: (Integral n) => n -> Word16
word16 n = convert n

-- | Constructs a 'Word32' from an integral value
word32 :: (Integral n) => n -> Word32
word32 n = convert n

-- | Constructs a 'Word64' from an integral value
word64 :: (Integral n) => n -> Word64
word64 n = convert n

-- | Constructs a 'BigInt' from an integral value
bigint :: (Integral n) => n -> BigInt
bigint n = convert n

    
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

type family SizedWord (n::Nat) where
    SizedWord 8 = Word8
    SizedWord 16 = Word16
    SizedWord 32 = Word32
    SizedWord 64 = Word64    
        
unsigned :: (Integral i, Num (Unsigned i)) => i -> Unsigned i
unsigned = convert    

word' :: (Integral i, Num (SizedWord n)) => i -> SizedWord n
word' = convert

int' :: (Num (SizedInt n), Integral i) => i -> SizedInt n
int' = convert

zed::(Num a) => a
zed = 0

sub'::(Num a) => a -> a -> a
sub' x y = x - y

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


instance Semigroup Int where (<>) = (+)
instance Monoid Int where mempty = 0    
instance Invertible Int Int where invert x = 0 - x
instance Additive Int where add = (+)
instance Multiplicative Int where mul = (*)
instance Subtractive Int where sub = (-)

instance Semigroup Integer where (<>) = (+)
instance Monoid Integer where mempty = 0    
instance Invertible Integer Integer where invert x = 0 - x
instance Additive Integer where add = (+)
instance Multiplicative Integer where mul = (*)
instance Subtractive Integer where sub = (-)

instance Semigroup Int8 where (<>) = (+)
instance Monoid Int8 where mempty = 0    
instance Invertible Int8 Int8 where invert x = 0 - x
instance Additive Int8 where add = (+)
instance Multiplicative Int8 where mul = (*)
instance Subtractive Int8 where sub = (-)

instance Semigroup Int16 where (<>) = (+)
instance Monoid Int16 where mempty = 0    
instance Invertible Int16 Int16 where invert x = 0 - x
instance Additive Int16 where add = (+)
instance Multiplicative Int16 where mul = (*)
instance Subtractive Int16 where sub = (-)

instance Semigroup Int32 where (<>) = (+)
instance Monoid Int32 where mempty = 0           
instance Invertible Int32 Int32 where invert x = 0 - x    
instance Additive Int32 where add = (+)
instance Multiplicative Int32 where mul = (*)
instance Subtractive Int32 where sub = (-)

instance Semigroup Int64 where (<>) = (+)
instance Monoid Int64 where mempty = 0           
instance Invertible Int64 Int64  where invert x = 0 - x
instance Additive Int64 where add = (+)
instance Multiplicative Int64 where mul = (*)
instance Subtractive Int64 where sub = (-)

instance Invertible Word Int where invert x = 0 - (fromIntegral x)
instance Additive Word where add = (+)
instance Multiplicative Word where mul = (*)
instance Subtractive Word where sub = (-)

instance Invertible Word8 Int8 where invert x = 0 - (fromIntegral x)
instance Additive Word8 where add = (+)
instance Multiplicative Word8 where mul = (*)
instance Subtractive Word8 where sub = (-)

instance Invertible Word16 Int16 where invert x = 0 - (fromIntegral x)
instance Additive Word16 where add = (+)
instance Multiplicative Word16 where mul = (*)
instance Subtractive Word16 where sub = (-)

instance Invertible Word32 Int32 where invert x = 0 - (fromIntegral x)
instance Additive Word32 where add = (+)
instance Multiplicative Word32 where mul = (*)
instance Subtractive Word32 where sub = (-)

instance Invertible Word64 Int64 where invert x = 0 - (fromIntegral x)
instance Additive Word64 where add = (+)
instance Multiplicative Word64 where mul = (*)
instance Subtractive Word64 where sub = (-)

instance Invertible Double Double where invert x = 0 - x
instance Additive Double where add = (+)
instance Multiplicative Double where mul = (*)
instance Subtractive Double where sub = (-)


instance Group Int
instance Group Integer
instance Group Int8
instance Group Int16
instance Group Int32
instance Group Int64

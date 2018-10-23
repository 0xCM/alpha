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
    
    -- *GHC.Natural
    Natural, natural,
 
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
    int', word'
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
import Numeric
import Alpha.Base
import Alpha.Text.Format
import Alpha.Canonical

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

-- | Canonical 'Natural' constructor
natural::(Integral n) => n -> Natural
natural n = fromIntegral n

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

test::SizedInt 32
test  = int' @32 51
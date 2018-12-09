module Alpha.Canonical.Algebra.Numeric
(
    Numeric(..),
    SignedIntegral(..),
    UnsignedIntegral(..),
    FiniteIntegral(..)

) where
import Alpha.Base    

import Alpha.Base 
import Alpha.Canonical.Operators
import Alpha.Canonical.Algebra.Semigroup
import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Nullary
import Alpha.Canonical.Algebra.Unital
import Alpha.Canonical.Algebra.Monoid
import Alpha.Canonical.Algebra.Absolutist
import Alpha.Canonical.Algebra.Divisive
import Alpha.Canonical.Algebra.Exponential
import Alpha.Canonical.Algebra.Modular
import Alpha.Canonical.Algebra.Polarity

class (Ord a, Subtractive a, Semigroup a, Multiplicative a, Nullary a, Unital a, Monoid a, Absolutist a, Divisive a, Real a) 
    => Numeric a where
    num::a -> a
    num = id
    {-# INLINE num #-}

-- Identifies signed integral types
class (Signed i, Numeric i, Integral i) => SignedIntegral i where

-- Classifies usigned integral types    
class (Unsigned i, Numeric i, Integral i, NaturallyPowered i, Modular i)  => UnsignedIntegral i where
    
type FiniteIntegral n = (Integral n, FiniteBits n)

instance Numeric Natural
instance Numeric Integer
instance Numeric Int
instance Numeric Int8
instance Numeric Int16
instance Numeric Int32
instance Numeric Int64
instance Numeric Word
instance Numeric Word8
instance Numeric Word16
instance Numeric Word32
instance Numeric Word64
instance (Integral a, Ord a) => Numeric (Ratio a)
instance Numeric Float
instance Numeric Double
instance Numeric CFloat
instance Numeric CDouble
    
instance SignedIntegral Int
instance SignedIntegral Int8
instance SignedIntegral Int16
instance SignedIntegral Int32
instance SignedIntegral Int64
instance SignedIntegral Integer

instance UnsignedIntegral Word
instance UnsignedIntegral Word8
instance UnsignedIntegral Word16
instance UnsignedIntegral Word32
instance UnsignedIntegral Word64
instance UnsignedIntegral Natural


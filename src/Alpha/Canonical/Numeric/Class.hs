-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Numeric.Class
(
    Numeric(..),
    IntegralNumeric(..),
    NaturalNumeric(..),
    Rational(..),
) where
import Alpha.Base    
import Alpha.Native
import Alpha.Canonical.Common
import Alpha.Canonical.Functions
import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Divisive
import Alpha.Canonical.Algebra.Modular
import Alpha.Canonical.Algebra.Semiring
import Alpha.Canonical.Algebra.Exponential

import Alpha.Canonical.Numeric.Types
import Alpha.Canonical.Numeric.Powers
import Alpha.Canonical.Numeric.Operations
import Alpha.Canonical.Numeric.Signage


class (Ord a, Subtractive a, Semiring a, Multiplicative a, Additive a, Absolute a, Divisive a, Num a, Real a, Powered a) 
    => Numeric a where
    num::a -> a
    num = id
    {-# INLINE num #-}

class (Numeric a, Integral a, Modular a) => IntegralNumeric a where

-- | Classifies unsigned integral numeric values    
class (IntegralNumeric a, UnsignedIntegral a) => NaturalNumeric a where

class (IntegralNumeric a) => Rational a where
    numerator::Ratio a -> a
    denominator::Ratio a -> a


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


instance IntegralNumeric Natural
instance IntegralNumeric Integer
instance IntegralNumeric Int
instance IntegralNumeric Int8
instance IntegralNumeric Int16
instance IntegralNumeric Int32
instance IntegralNumeric Int64
instance IntegralNumeric Word
instance IntegralNumeric Word8
instance IntegralNumeric Word16
instance IntegralNumeric Word32
instance IntegralNumeric Word64
    
instance NaturalNumeric Word
instance NaturalNumeric Word8
instance NaturalNumeric Word16
instance NaturalNumeric Word32
instance NaturalNumeric Word64
instance NaturalNumeric Natural
    
instance (IntegralNumeric a) => Rational a  where
    numerator = numerator'
    denominator = denominator'
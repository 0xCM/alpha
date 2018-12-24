-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
module Alpha.Canonical.Algebra.Numeric
(
    module X,
    Numeric(..),
    IntegralNumeric(..),
    NaturalNumeric(..),
    Rational(..),
    IntegrallyPowered(..),
    ApproximatelyPowered(..),
    egcd,
) where
import Alpha.Canonical.Common
import Alpha.Canonical.Algebra.Field as X
import Alpha.Canonical.Algebra.Exponential as X

class (Fractional a) => IntegrallyPowered a where
    powi::(Integral p) => a -> p -> a

    (^^)::(Integral p) => a -> p -> a
    (^^) = powi
    {-# INLINE (^^) #-}
    infixr 8 ^^
    
class (Floating a) => ApproximatelyPowered a where
    powa::a -> a -> a

    (**)::a -> a -> a
    (**) = powa
    {-# INLINE (**) #-}
    infixr 8 **


class (Ord a, Subtractive a, Semiring a, Multiplicative a, Additive a, Absolute a, Divisive a, Num a, Real a, Powered a) 
    => Numeric a where
    num::a -> a
    num = id
    {-# INLINE num #-}

class (Numeric a, Integral a, IntegralDomain a) => IntegralNumeric a where

-- | Classifies unsigned integral numeric values    
class (IntegralNumeric a, UnsignedIntegral a) => NaturalNumeric a where

class (IntegralNumeric a) => Rational a where
    numerator::Ratio a -> a
    denominator::Ratio a -> a

-- | Extended gcd function in a Euclidean domain
-- Implementation taken from arithmoi
-- egcd a b = (s,t,d) where d = gcd(a,b) = sa + tb
egcd::(Eud a, Num a) => a -> a -> (a,a,a)
egcd a b = (d, x * signum a, y * signum b) 
    where
    (d, x, y) = eGCD 0 1 1 0 (abs a) (abs b)
    eGCD !n1 o1 !n2 o2 r s
        | s == 0    = (r, o1, o2)
        | otherwise = case r `quotRem` s of
                        (q, t) -> eGCD (o1 - q*n1) n1 (o2 - q*n2) n2 s t
    

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


instance (Show b, Show p) => Show (Exponential b p) where
    show (Exponential (b,p)) = (show b) <> "^" <> (show p)


-- ApproximatelyPowered
-------------------------------------------------------------------------------
instance ApproximatelyPowered Float where 
    powa = powa'
    {-# INLINE powa #-}
instance ApproximatelyPowered Double where 
    powa = powa'
    {-# INLINE powa #-}
instance ApproximatelyPowered CFloat where 
    powa = powa'
    {-# INLINE powa #-}
instance ApproximatelyPowered CDouble where 
    powa = powa'
    {-# INLINE powa #-}

-- IntegrallyPowered
-------------------------------------------------------------------------------
instance (Integral n) => IntegrallyPowered (Ratio n) where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegrallyPowered Float where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegrallyPowered Double where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegrallyPowered CFloat where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegrallyPowered CDouble where 
    powi = pow''
    {-# INLINE powi #-}

    
    
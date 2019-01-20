-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Rational
(
    module X,
    Rational(..),
    rational,
    numerator,
    denominator
)
where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Numeric as X
import Alpha.Canonical.Algebra.Negatable as X

-- | Represents a mathematically faithful rational number
newtype Rational = Rational (Ratio Integer)
    deriving (Eq, Ord, Enum, Num, Real, Fractional, RealFrac,
            Generic, Data, Typeable,
            Divisive, Power, 
            Additive, Nullary, 
            Subtractive, Negatable, 
            Multiplicative, Unital, 
            Distributive, LeftDistributive, RightDistributive)

            
instance Formattable Rational where
    format (Rational q) = format (numerator' q) <> FSlash <> format (denominator' q)
instance Show Rational where
    show = string . format

-- Forms a 'Rational' number from a 'Real' number
rational::(Real r) => r -> Rational
rational x = Rational $ toRational' x
{-# INLINE rational #-} 

-- numerator::(Integral b, Integral a) => Ratio a -> b
-- numerator = fromIntegral . numerator'

-- denominator::(Integral b, Integral a) => Ratio a -> b
-- denominator = fromIntegral . denominator'

numerator::Rational -> Integer
numerator (Rational r) = numerator' r

denominator::Rational -> Integer
denominator (Rational r) = denominator' r


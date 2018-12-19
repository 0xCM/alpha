-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE MagicHash #-}
module Alpha.Base.Numeric
(
    
    Natural,
    Integral, quot, quotRem, divMod, gcd,lcm,
    Floating, pi, exp, log, sin, cos, asin, acos, atan, sinh, cosh, asinh, acosh, atanh, sqrt, double2Float, float2Double,
    Fractional, recip, fromRational, divf,
    Ratio, frac,
    Real,fromIntegral,
    RealFrac, properFraction, truncate, round, ceiling, floor,realToFrac,
    RealFloat, floatRadix, floatDigits, floatRange, decodeFloat, encodeFloat, exponent, significand, scaleFloat, isNaN, isIEEE, isInfinite, isDenormalized, isNegativeZero, atan2,
    Num,Integer,fromInteger, signum,
    Int,Int8,Int16,Int32,Int64,
    Word,Word8,Word16,Word32,Word64,
    Double , Double#, Float, Float#,
    CDouble,CFloat,
    Interval, (+/-),width,

) where
import Data.Int(Int,Int8,Int16,Int32,Int64)
import Data.Word(Word,Word8,Word16,Word32,Word64)
import Data.Ratio(Ratio, (%))
import GHC.Num(Num,Integer,fromInteger,signum,Natural)
import GHC.Real(Fractional(..),RealFrac(..),Real, fromIntegral)
import GHC.Real(Integral(..), gcd, lcm, rem, quot, mod,  divMod, quotRem,)
import GHC.Float(Floating(..), RealFloat(..), Double, Double#, Float, Float#,double2Float,float2Double)
import GHC.Real(realToFrac)
import GHC.Enum(Bounded(..))
import Foreign.C(CDouble,CFloat)
import Numeric.Interval(Interval, (+/-),width)

-- | Infix alias for the base fractional division operator (/)
divf::(Fractional a) => a -> a -> a
divf = (/)
{-# INLINE divf #-}

-- Defines a rational number as a fraction
frac::(Integral n) => n -> n -> Ratio n
frac m n =  m % n
{-# INLINE frac #-}

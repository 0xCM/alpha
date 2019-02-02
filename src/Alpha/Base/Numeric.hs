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
    Integral, 
    Floating, sqrt, double2Float, float2Double, diva,
    Fractional, recip, fromRational, divf,
    Ratio, frac,
    Real,fromIntegral,
    RealFrac, properFraction, truncate, round, ceiling, floor, realToFrac,
    RealFloat, floatRadix, floatDigits, floatRange, decodeFloat, encodeFloat, exponent, significand, scaleFloat, isNaN, isIEEE, isInfinite, isDenormalized, isNegativeZero,
    Num,Integer,fromInteger, signum,
    Int,Int8,Int16,Int32,Int64,
    Word,Word8,Word16,Word32,Word64,
    Double , Double#, Float, Float#,
    Interval, (+/-),width,

    factorial, factorials,  zed

) where
import Data.Int(Int,Int8,Int16,Int32,Int64)
import Data.Word(Word,Word8,Word16,Word32,Word64)
import Data.Ratio(Ratio, (%))
import Data.Scientific(Scientific, scientific)
import GHC.Enum
import GHC.Num(Num,Integer,fromInteger,signum,Natural)
import GHC.Real(Fractional(..),Real, RealFrac, fromIntegral,properFraction, truncate, round, ceiling, floor)
import GHC.Float(floatRadix, floatDigits, floatRange, decodeFloat, encodeFloat, exponent, significand, scaleFloat, isNaN, isIEEE, isInfinite, isDenormalized, isNegativeZero)
import GHC.Real(Integral(..), gcd, lcm, rem, quot, mod,  divMod, quotRem,)
import GHC.Float(Floating(..), RealFloat(..), Double, Double#, Float, Float#,double2Float,float2Double)
import GHC.Real(realToFrac)
import GHC.Enum(Bounded(..))
import Numeric.Interval(Interval, (+/-),width)
import qualified Math.NumberTheory.Recurrencies.Linear as Arith
import qualified Data.List as List


factorials::(Num a, Enum a) => [a]
factorials = Arith.factorial

factorial::(Num a, Enum a,Integral a) => a -> a
factorial a = List.last (List.take (fromIntegral a) factorials)

-- | Infix alias for the base fractional division operator (/)
divf::(Fractional a) => a -> a -> a
divf = (/)
{-# INLINE divf #-}

diva::(Floating a) => a -> a -> a
diva = (/)
{-# INLINE diva #-}

-- Defines a rational number as a fraction
frac::(Integral n) => n -> n -> Ratio n
frac m n =  m % n
{-# INLINE frac #-}

-- | Returns the number 0
zed::forall a. Num a => a
zed = 0::a
{-# INLINE zed #-}

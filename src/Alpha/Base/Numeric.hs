{-# LANGUAGE NoStarIsType #-}
-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE MagicHash #-}

module Alpha.Base.Numeric
(
    Rational(..), Ratio(..),numerator, denominator,
    divMod, (/%),
    quotRem, (/%%),
    frac,

    Natural,

    Floating, pi, exp, log, sin, cos, asin, acos, atan, sinh, cosh, asinh, acosh, atanh, sqrt,
    double2Float, float2Double,
    Fractional, recip, fromRational, 
    Real,Integral, fromIntegral,
    RealFrac, properFraction, truncate, round, ceiling, floor,realToFrac,
    RealFloat, floatRadix, floatDigits, floatRange, decodeFloat, encodeFloat, exponent, significand, scaleFloat, isNaN, isIEEE, isInfinite, isDenormalized, isNegativeZero, atan2,
    Num,Integer,fromInteger, signum,
    Int,Int8,Int16,Int32,Int64,
    Word,Word8,Word16,Word32,Word64,
    Double , Double#, Float, Float#,
    CDouble,CFloat,
    Interval, (+/-),interval,width,

    KnownNat, SomeNat, Nat, 
    nat, natg, 
    type (+), type (-), type (*), Mod(..), type (%), Div(..), type (/), type (^),        
    

) where
import Data.Int(Int,Int8,Int16,Int32,Int64)
import Data.Word(Word,Word8,Word16,Word32,Word64)
import Data.Ratio(Ratio(..),Rational(..))
import GHC.Num(Num,Integer,fromInteger,signum,Natural)
import GHC.Real(Fractional(..),RealFrac(..),numerator,denominator,divMod,quotRem,Integral(..),Real, Integral, mod, fromIntegral)
import GHC.TypeLits(type (+), type (-), type (*), type (^), Mod(..), Div(..))
import GHC.TypeLits(KnownNat, SomeNat, Nat,natVal, natVal')
import GHC.Float
import GHC.Real(realToFrac)
import GHC.Float(double2Float,float2Double)
import Foreign.C(CDouble,CFloat)
import Numeric.Interval(Interval, (...), (+/-),interval,width)
import Data.Proxy

-- Modulus infix operator synonm
type (%) m n = Mod m n

-- Division infix operator synonm
type (/) m n = Div m n
infixl 7 /


(/%)::(Integral n) => n -> n -> (n,n)
(/%) = divMod
{-# INLINE (/%) #-}
infixl 5 /%

(/%%)::(Integral n) => n -> n -> (n,n)
(/%%) = quotRem
{-# INLINE (/%%) #-}
infixl 5 /%%

-- | Infix alias for the base fractional division operator (/)
frac::(Fractional a) => a -> a -> a
frac = (/)
{-# INLINE frac #-}

proxy:: Proxy n
proxy = Proxy
{-# INLINE proxy #-}

-- | Computes the 'Int' value corresponding to a type-level nat
nat::forall m. KnownNat m => Int
nat = fromIntegral (natVal (proxy @m))
{-# INLINE nat #-}

-- | Computes the (generic) integral value corresponding to a type-level nat
natg::forall m i. (KnownNat m, Integral i) => i
natg = fromIntegral (natVal (proxy @m))
{-# INLINE natg #-}


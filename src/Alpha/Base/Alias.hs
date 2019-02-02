module Alpha.Base.Alias
(
    FiniteInt(..),
    Set'(..),
    Bag(..),

    sub', add', div', negate', mul', abs', pow', pow'', powa',mod', flip',
    out', range',interval',union',intersect', rem',numerator',denominator', realToFrac', toRational',
    and',or',not', quotRem', divMod', gcd',lcm', quot', set',
    lt', gt', gteq', lteq', between',
    sin', cos', tan', asin', acos', atan', sinh', cosh', asinh', acosh', atanh',
    exp', log', logBase',
    powerset',
    floatToDigits',complement'

) where
import System.IO(IO,print)
import Data.Ix(Ix(..))
import Data.Ord
import Data.Maybe(fromJust)
import Data.Ratio(Ratio(..),Rational)
import Numeric.Interval(Interval, interval)
import Data.Bool(Bool(..), (&&), (||), not)
import Data.Set(Set(..),powerSet)
import Data.Bits(Bits(..), xor,bit)
import Data.Int(Int)
import GHC.Float(RealFloat,Floating,(**), sin, cos, tan, asin, acos, atan, sinh, cosh, asinh, acosh, atanh, exp, log, logBase,floatToDigits )
import GHC.Num(Num, (+),(-),(*),negate,abs)
import GHC.Real(div,(/),(^),(^^), mod, rem, quot, quotRem, divMod, lcm, gcd, Fractional, Integral, numerator, denominator, Real, realToFrac,toRational)
import GHC.Base(flip)
import GHC.Enum(Bounded(..))
import GHC.Show(Show)
import Data.Typeable(typeOf)
import Prelude(Integer)

import qualified Data.Set as Set
import qualified Data.Map.Lazy as LM
import qualified Data.MultiSet as MS

type FiniteInt a = (Integral a, Bounded a)
type Bag a = MS.MultiSet a
type Set' a = Set a


sub'::(Num a) => a -> a -> a
sub' = (-)
{-# INLINE sub' #-}

add'::(Num a) => a -> a -> a
add' = (+)
{-# INLINE add' #-}

div'::(Integral a) => a -> a -> a
div' = div
{-# INLINE div' #-}

lt'::(Ord a) => a -> a -> Bool
lt' = (<)
{-# INLINE lt' #-}

gt'::(Ord a) => a -> a -> Bool
gt' = (>)
{-# INLINE gt' #-}

gteq'::(Ord a) => a -> a -> Bool
gteq' = (>=)
{-# INLINE gteq' #-}

lteq'::(Ord a) => a -> a -> Bool
lteq' = (<=)
{-# INLINE lteq' #-}

between'::(Ord a) => a -> (a,a) -> Bool
between' x (a,b) = x >= a && x <= b
{-# INLINE between' #-}

negate'::(Num a) => a -> a
negate' = negate
{-# INLINE negate' #-}

mul'::(Num a) => a -> a -> a
mul' = (*)
{-# INLINE mul' #-}

abs'::(Num a) => a -> a
abs' = abs
{-# INLINE abs' #-}

pow'::(Num a, Integral b) => a -> b -> a
pow' = (^)
{-# INLINE pow' #-}

pow''::(Fractional a, Integral b) => a -> b -> a
pow'' = (^^)
{-# INLINE pow'' #-}

powa'::(Floating a) => a -> a -> a
powa' = (**)
{-# INLINE powa' #-}

mod'::Integral a => a -> a -> a
mod' = mod
{-# INLINE mod' #-}

quot'::(Integral a) => a -> a -> a
quot' = quot
{-# INLINE quot' #-}

lcm'::(Integral a) => a -> a -> a
lcm' = lcm
{-# INLINE lcm' #-}

gcd'::(Integral a) => a -> a -> a
gcd' = gcd
{-# INLINE gcd' #-}

flip'::(a -> b -> c) -> b -> a -> c
flip' = flip
{-# INLINE flip' #-}

quotRem'::(Integral a) => a -> a -> (a,a)
quotRem' = quotRem
{-# INLINE quotRem' #-}

divMod'::(Integral a) => a -> a -> (a,a)
divMod' = divMod
{-# INLINE divMod' #-}

-- | Renders a showable to standard out 
out'::Show s => s -> IO()
out' s = print s
{-# INLINE out' #-}

range'::(Ix a) => (a,a) -> [a]
range' = range
{-# INLINE range' #-}

interval'::(Ord a) => a -> a -> Interval a
interval' min max = fromJust (interval min max)
{-# INLINE interval' #-}

union'::(Ord a) => Set a -> Set a -> Set a
union' = Set.union
{-# INLINE union' #-}

intersect'::(Ord a) => Set a -> Set a -> Set a
intersect' = Set.intersection
{-# INLINE intersect' #-}

rem'::(Integral a) => a -> a -> a
rem' = rem
{-# INLINE rem' #-}

numerator'::(Integral a) => Ratio a -> a
numerator' = numerator
{-# INLINE numerator' #-}

denominator'::(Integral a) => Ratio a -> a
denominator' = denominator
{-# INLINE denominator' #-}

realToFrac'::(Real a, Fractional b) => a -> b
realToFrac' = realToFrac
{-# INLINE realToFrac' #-}

toRational'::(Real r) => r -> Rational
toRational' = toRational
{-# INLINE toRational' #-}

and'::Bool -> Bool -> Bool
and' = (&&)
{-# INLINE and' #-}

or'::Bool -> Bool -> Bool
or' = (&&)
{-# INLINE or' #-}

not'::Bool -> Bool
not' = not
{-# INLINE not' #-}

xor'::(Bits a) => a -> a -> a
xor' = xor
{-# INLINE xor' #-}

bit'::(Bits a) => Int -> a
bit' = bit
{-# INLINE bit' #-}

set'::(Ord a) => [a] -> Set a
set' = Set.fromList

-- | Calculates a set's powerset, modulo the empty set
powerset'::Set' a -> Set' (Set' a)
powerset' s =  Set.filter  (\x -> not (Set.null x)) (powerSet s)

sin'::(Floating a) => a -> a
sin' = sin
{-# INLINE sin' #-}

cos'::(Floating a) => a -> a 
cos' = cos
{-# INLINE cos' #-}

tan'::(Floating a) => a -> a
tan' = tan
{-# INLINE tan' #-}

asin'::(Floating a) => a -> a
asin' = asin
{-# INLINE asin' #-}

acos'::(Floating a) => a -> a
acos' = acos
{-# INLINE acos' #-}

atan'::(Floating a) => a -> a
atan' = atan
{-# INLINE atan' #-}

sinh'::(Floating a) => a -> a
sinh' = sinh
{-# INLINE sinh' #-}

cosh'::(Floating a) => a -> a
cosh' = cosh
{-# INLINE cosh' #-}

asinh'::(Floating a) => a -> a
asinh' = asinh
{-# INLINE asinh' #-}

acosh'::(Floating a) => a -> a
acosh' = acosh
{-# INLINE acosh' #-}

atanh'::(Floating a) => a -> a
atanh' = atanh
{-# INLINE atanh' #-}

exp'::(Floating a) => a -> a
exp' = exp
{-# INLINE exp' #-}

log'::Floating a => a -> a
log' = log
{-# INLINE log' #-}

logBase'::Floating a => a -> a -> a
logBase' = logBase
{-# INLINE logBase' #-}

floatToDigits'::RealFloat a => Integer -> a -> ([Int],Int)
floatToDigits' = floatToDigits
{-# INLINE floatToDigits' #-}

complement'::(Bits a) => a -> a
complement' = complement
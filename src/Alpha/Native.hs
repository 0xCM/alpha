module Alpha.Native
(
    sub', add', div', negate', mul', abs', pow', pow'', powa',mod', flip',
    out', range',interval',union',intersect', rem',numerator',denominator', realToFrac', toRational',
    and',or',not', lt', gt',quotRem', divMod', gcd',lcm', quot'

) where
--import Alpha.Base
import GHC.Num(Num, (+),(-),(*),negate,abs)
import GHC.Real(div,(/),(^),(^^), mod, rem, quot, quotRem, divMod, lcm, gcd, Fractional,Integral, numerator, denominator, Real, realToFrac,toRational)
import GHC.Float(Floating,(**))
import GHC.Base(flip)
import System.IO(IO,print)
import GHC.Show(Show)
import Data.Ix(Ix(..))
import Data.Ord
import Data.Maybe(fromJust)
import Data.Ratio(Ratio(..),Rational)
import Numeric.Interval(Interval, interval)
import Data.Bool(Bool(..), (&&), (||), not)
import Data.Set(Set)
import Data.Bits(Bits(..), xor,bit)
import Data.Int(Int)
import qualified Data.Set as Set

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

gt'::(Ord a) => a -> a -> Bool
gt' = (>)


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


module Alpha.Native
(
    sub', add', div', negate', mul', abs', pow', pow'', powa',mod', flip',
    out', range',interval',union',intersect', rem',numerator',denominator', realToFrac', toRational'

) where
--import Alpha.Base
import GHC.Num(Num, (+),(-),(*),negate,abs)
import GHC.Real(div,(/),(^),(^^), mod, rem, Fractional,Integral, numerator, denominator, Real, realToFrac,toRational)
import GHC.Float(Floating,(**))
import GHC.Base(flip)
import System.IO(IO,print)
import GHC.Show(Show)
import Data.Ix(Ix(..))
import Data.Ord(Ord)
import Data.Maybe(fromJust)
import Data.Ratio(Ratio(..),Rational)
import Numeric.Interval(Interval, interval)

import Data.Set(Set)
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

flip'::(a -> b -> c) -> b -> a -> c
flip' = flip
{-# INLINE flip' #-}

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
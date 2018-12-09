module Alpha.Native
(
    sub', add', div', divf', negate', mul', abs', pow', pow'', powa',mod', flip'

) where
--import Alpha.Base
import GHC.Num(Num, (+),(-),(*),negate,abs)
import GHC.Real(div,(/),(^),(^^), mod,Fractional,Integral)
import GHC.Float(Floating,(**))
import GHC.Base(flip)

sub'::(Num a) => a -> a -> a
sub' = (-)
{-# INLINE sub' #-}

add'::(Num a) => a -> a -> a
add' = (+)
{-# INLINE add' #-}

div'::(Integral a) => a -> a -> a
div' = div
{-# INLINE div' #-}

divf'::(Fractional a) => a -> a -> a
divf' = (/)
{-# INLINE divf' #-}

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


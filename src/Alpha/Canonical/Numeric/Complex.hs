-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Numeric.Complex
(
    Complex(..),
    complex,
    re,
    im,
    conjugate
) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Algebra
import Alpha.Canonical.Text.Asci

newtype Complex a = Complex (a,a)
    deriving(Eq, Additive, Subtractive)
    
complex::(a,a) -> Complex a
complex (r,i) = Complex (r,i)

re::Complex a -> a
re (Complex (r, _)) = r

im::Complex a -> a
im (Complex (_,i)) = i

conjugate::(Negatable a) => Complex a -> Complex a
conjugate (Complex (r,i)) = complex (r, negate i)

instance Formattable a => Formattable (Complex a) where
    format (Complex (r,i)) = format r <> spaced Plus <> format i <> Il

instance Formattable a => Show(Complex a) where
    show  = string . format

instance (Multiplicative a, Additive a, Subtractive a) => Multiplicative (Complex a) where
    mul (Complex (x,y)) (Complex (u,v)) = complex (x*u - y*v, x*v + y*u)


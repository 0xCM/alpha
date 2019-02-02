-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Complex
(
    Complex(..),
    Conjugatable(..),
    type (^*),
    complex,
    re,
    im,
    euler
) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Negatable
import Alpha.Canonical.Algebra.Action
import Alpha.Canonical.Algebra.Divisive
import Alpha.Canonical.Algebra.Reciprocative
import Alpha.Canonical.Common.Asci

-- | Represents a complex number
newtype Complex a = Complex (a,a)
    deriving(Eq, Additive, Subtractive, Negatable, Unital, Nullary)

type family (^*) a = r | r -> a where
    (^*) Int = Complex Int 
    (^*) Int8 = Complex Int8 
    (^*) Int16 = Complex Int16 
    (^*) Int32 = Complex Int32 
    (^*) Int64 = Complex Int64 
    (^*) Word = Complex Word
    (^*) Word8 = Complex Word8 
    (^*) Word16 = Complex Word16 
    (^*) Word32 = Complex Word32 
    (^*) Word64 = Complex Word64 
    (^*) Double = Complex Double 
    (^*) Float = Complex Float
    (^*) Natural = Complex Natural
    (^*) Integer = Complex Integer
    (^*) (Ratio a) = Complex (Ratio a)


-- | Characterizes types that support a notion of complex conjugation    
class Conjugatable a where
    conjugate::a -> a

    (^*)::a -> a
    (^*) = conjugate
        
complex::(a,a) -> Complex a
complex (r,i) = Complex (r,i)

re::Complex a -> a
re (Complex (r, _)) = r

im::Complex a -> a
im (Complex (_,i)) = i

-- | Evaluates Euler's formula for e^{ix} := cos x + i(sin x)
-- See https://en.wikipedia.org/wiki/Euler%27s_formula
euler::Trigonometric a => a -> Complex a
euler x = complex (cos x, sin x)

instance Negatable a => Conjugatable (Complex a) where
    conjugate (Complex (r,i)) = complex (r, negate i)

instance (Formattable a, Negatable a, Signable a) => Formattable (Complex a) where
    format (Complex (r,i)) = format r <> pad symbol <> format val <> Il where
        s = sign i
        (symbol, val) = (ifelse (s == Negative) Dash Plus, ifelse (s == Negative) (negate i) i)
        
instance (Negatable a, Formattable a, Signable a) => Show(Complex a) where
    show  = string . format

instance (Multiplicative a) => LeftAction a (Complex a) where
    k *. (Complex (x,y) ) = complex (k * x, k * y)

instance (Multiplicative a) => RightAction (Complex a) a where
    (Complex (x,y) ) .* k = complex (x * k, y * k)
    
instance (Multiplicative a, Additive a, Subtractive a) => Multiplicative (Complex a) where
    mul (Complex (a,b)) (Complex (c,d)) = complex (a*c - b*d, a*d + b*c)

instance (Multiplicative a, Additive a, Subtractive a, Divisive a) => Reciprocative (Complex a) where
    reciprocal (Complex (a,b)) = complex(a / bottom, b / bottom) where
        bottom = a*a + b*b

instance (Multiplicative a, Additive a, Subtractive a, Divisive a) => Divisive (Complex a) where
    div (Complex (a,b)) (Complex (c,d)) = complex ( (a*c + b*d)/bottom, (b*c - a*d) / bottom) where
        bottom = c*c + d*d
    
    
    
    
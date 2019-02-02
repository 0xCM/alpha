-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Transcendental
(    
    Trigonometric(..),
    Logarithmic(..),
    pattern PI,
    pattern E,
)
 where
import Alpha.Base
import Alpha.Canonical.Common.Root

pattern PI = 3.141592653589793238
pattern E =  2.718281828459045235

-- | Represents a sine computation
newtype Sin a = Sin a
    deriving(Eq,Ord,Generic,Data,Typeable)
instance Newtype (Sin a)    

-- | Represents a cosine computation
newtype Cos a = Cos a
    deriving(Eq,Ord,Generic,Data,Typeable)
instance Newtype (Cos a)    

-- | Represents a tangent computation
newtype Tan a = Tan a
    deriving(Eq,Ord,Generic,Data,Typeable)
instance Newtype (Tan a)    

class Floating a => Trigonometric a where
    sin::a -> a
    sin = sin'
    {-# INLINE sin #-}
    
    cos:: a -> a 
    cos = cos'
    {-# INLINE cos #-}
    
    tan:: a -> a
    tan = tan'
    {-# INLINE tan #-}
    
    asin:: a -> a
    asin = asin'
    {-# INLINE asin #-}
    
    acos:: a -> a
    acos = acos'
    {-# INLINE acos #-}
    
    atan:: a -> a
    atan = atan'
    {-# INLINE atan #-}
    
    sinh:: a -> a
    sinh = sinh'
    {-# INLINE sinh #-}
    
    cosh:: a -> a
    cosh = cosh'
    {-# INLINE cosh #-}
    
    asinh:: a -> a
    asinh = asinh'
    {-# INLINE asinh #-}
    
    acosh:: a -> a
    acosh = acosh'
    {-# INLINE acosh #-}
    
    atanh:: a -> a
    atanh = atanh'
    {-# INLINE atanh #-}    

class (Floating a) => Logarithmic a where
    -- | Computes the natural logarithm of a value
    ln::a -> a
    ln = log'
    {-# INLINE ln #-}    

    -- | Computes the logarithm of a value at a specified base
    log::a -> a -> a
    log = logBase'
    {-# INLINE log #-}    

    -- | Computes the value of 'E' raised to a specified power
    exp::a -> a
    exp = exp'
    {-# INLINE exp #-}    
    

instance Trigonometric Double    
instance Trigonometric Float

instance Logarithmic Double    
instance Logarithmic Float

instance Trigonometric a => Computable (Sin a) where
    type Computed (Sin a) = a
    compute (Sin x) = sin x

instance Trigonometric a => Computable (Cos a) where
    type Computed (Cos a) = a
    compute (Cos x) = cos x
    
instance Trigonometric a => Computable (Tan a) where
    type Computed (Tan a) = a
    compute (Tan x) = tan x
    
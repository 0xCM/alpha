-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Numeric.Powers
(    
    IntegrallyPowered(..),
    ApproximatelyPowered(..),
)
where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Algebra.Exponential
import Alpha.Canonical.Relations
import Alpha.Canonical.Numeric.Signage

    
class (Fractional a) => IntegrallyPowered a where
    powi::(Integral p) => a -> p -> a

    (^^)::(Integral p) => a -> p -> a
    (^^) = powi
    {-# INLINE (^^) #-}
    infixr 8 ^^
    
class (Floating a) => ApproximatelyPowered a where
    powa::a -> a -> a

    (**)::a -> a -> a
    (**) = powa
    {-# INLINE (**) #-}
    infixr 8 **
    

instance (Show b, Show p) => Show (Exponential b p) where
    show (Exponential (b,p)) = (show b) <> "^" <> (show p)


-- ApproximatelyPowered
-------------------------------------------------------------------------------
instance ApproximatelyPowered Float where 
    powa = powa'
    {-# INLINE powa #-}
instance ApproximatelyPowered Double where 
    powa = powa'
    {-# INLINE powa #-}
instance ApproximatelyPowered CFloat where 
    powa = powa'
    {-# INLINE powa #-}
instance ApproximatelyPowered CDouble where 
    powa = powa'
    {-# INLINE powa #-}

-- IntegrallyPowered
-------------------------------------------------------------------------------
instance (Integral n) => IntegrallyPowered (Ratio n) where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegrallyPowered Float where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegrallyPowered Double where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegrallyPowered CFloat where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegrallyPowered CDouble where 
    powi = pow''
    {-# INLINE powi #-}

    

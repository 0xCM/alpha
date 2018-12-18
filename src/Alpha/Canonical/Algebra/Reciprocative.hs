module Alpha.Canonical.Algebra.Reciprocative
(    
    Reciprocative(..),

) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Element
import Alpha.Canonical.Algebra.Divisive

-- | Characterizes types whose values are closed under 
-- mulitiplicative inversion 
class Reciprocative a where
    reciprocal::a -> a

-- Reciprocative 
-------------------------------------------------------------------------------
instance (Integral a) => Reciprocative (Ratio a) where
    reciprocal x = recip x
    {-# INLINE reciprocal #-}

instance Reciprocative Float where 
    reciprocal x = 1/x
    {-# INLINE reciprocal #-}

instance Reciprocative Double where 
    reciprocal x = 1/x
    {-# INLINE reciprocal #-}

instance Reciprocative CFloat where 
    reciprocal x = 1/x
    {-# INLINE reciprocal #-}

instance Reciprocative CDouble where 
    reciprocal x = 1/x
    {-# INLINE reciprocal #-}

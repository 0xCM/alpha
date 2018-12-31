module Alpha.Canonical.Algebra.Reciprocative
(    
    Reciprocative(..),

) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Divisive

-- | Characterizes types whose values are closed under 
-- mulitiplicative inversion 
class Reciprocative a where
    reciprocal::a -> a
        
-- Reciprocative 
-------------------------------------------------------------------------------
instance (Integral a) => Reciprocative (Ratio a) where
    reciprocal = recip
    {-# INLINE reciprocal #-}

instance Reciprocative Float where 
    reciprocal = recip
    {-# INLINE reciprocal #-}

instance Reciprocative Double where 
    reciprocal =  recip
    {-# INLINE reciprocal #-}

instance Reciprocative CFloat where 
    reciprocal = recip
    {-# INLINE reciprocal #-}

instance Reciprocative CDouble where 
    reciprocal = recip
    {-# INLINE reciprocal #-}
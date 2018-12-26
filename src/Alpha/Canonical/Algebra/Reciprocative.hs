module Alpha.Canonical.Algebra.Reciprocative
(    
    Reciprocative(..),
    Reciprocation(..), reciprocation,

) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Divisive

-- | Characterizes types whose values are closed under 
-- mulitiplicative inversion 
class Reciprocative a where
    reciprocal::a -> a


newtype Reciprocation a = Reciprocation (O1 a)
    deriving(Generic)
instance Newtype (Reciprocation a)


-- | Produces the canonical Reciprocation operator
reciprocation::(Reciprocative a) => Reciprocation a
reciprocation = Reciprocation reciprocal

        
-- Reciprocative 
-------------------------------------------------------------------------------
instance (Integral a) => Reciprocative (Ratio a) where
    reciprocal x = recip x
    {-# INLINE reciprocal #-}

instance Reciprocative Float where 
    reciprocal x =  (unwrap reciprocation) x
    {-# INLINE reciprocal #-}

instance Reciprocative Double where 
    reciprocal x =  (unwrap reciprocation) x
    {-# INLINE reciprocal #-}

instance Reciprocative CFloat where 
    reciprocal x =  (unwrap reciprocation) x
    {-# INLINE reciprocal #-}

instance Reciprocative CDouble where 
    reciprocal x =  (unwrap reciprocation) x
    {-# INLINE reciprocal #-}

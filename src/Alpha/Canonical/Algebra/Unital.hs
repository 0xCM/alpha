module Alpha.Canonical.Algebra.Unital
(
    Unital(..),    


 ) where
import Alpha.Base

-- Characterizes types or structures that are inhabited by the concept of singularity
class Unital (a::Type) where
    -- Specifies the canonical 1 for an element relative to a structure
    one::a

-- Unital
-------------------------------------------------------------------------------
instance Unital Natural where 
    one = 1
    {-# INLINE one #-}
instance Unital Integer where 
    one = 1
    {-# INLINE one #-}
instance Unital Int where 
    one = 1
    {-# INLINE one #-}
instance Unital Int8 where 
    one = 1
    {-# INLINE one #-}
instance Unital Int16 where 
    one = 1
    {-# INLINE one #-}
instance Unital Int32 where 
    one = 1
    {-# INLINE one #-}
instance Unital Int64 where 
    one = 1
    {-# INLINE one #-}
instance Unital Word where 
    one = 1
    {-# INLINE one #-}
instance Unital Word8 where 
    one = 1
    {-# INLINE one #-}
instance Unital Word16 where 
    one = 1
    {-# INLINE one #-}
instance Unital Word32 where 
    one = 1
    {-# INLINE one #-}
instance Unital Word64 where 
    one = 1
    {-# INLINE one #-}
instance (Integral a) => Unital (Ratio a) where 
    one = 1
    {-# INLINE one #-}    
instance Unital Float where 
    one = 1
    {-# INLINE one #-}
instance Unital Double where 
    one = 1
    {-# INLINE one #-}
instance Unital CFloat where 
    one = 1
    {-# INLINE one #-}
instance Unital CDouble where 
    one = 1
    {-# INLINE one #-}


module Alpha.Canonical.Numeric.Signage
(
    UnsignedIntegral(..),
    Signable(..),
    SignedIntegral(..)
)
where
import Alpha.Base
import Alpha.Canonical.Common
import Alpha.Canonical.Algebra.Orientation
import Alpha.Canonical.Numeric.Types


-- | Characterizes type for which signs may be computed
-- Alternately, characterizes types whose values may be 
-- partitioned into three disjoint subsets, one called 'Negative'
-- one 'Positive' the other 'Neutral'
class Signable a where
    sign::a -> Sign

-- | Classifies signed integral types    
class (Signable i, Integral i) => SignedIntegral i where

class (Unsignable i, Integral i) => UnsignedIntegral i where


instance UnsignedIntegral Word
instance UnsignedIntegral Word8
instance UnsignedIntegral Word16
instance UnsignedIntegral Word32
instance UnsignedIntegral Word64
instance UnsignedIntegral Natural
        

instance Signable Natural where 
    sign = orientation
    {-# INLINE sign #-}
instance Signable Integer where 
    sign = orientation
    {-# INLINE sign #-}
instance Signable Int where 
    sign = orientation
    {-# INLINE sign #-}
instance Signable Int8 where 
    sign = orientation
    {-# INLINE sign #-}
instance Signable Int16 where 
    sign = orientation
    {-# INLINE sign #-}
instance Signable Int32 where 
    sign = orientation
    {-# INLINE sign #-}
instance Signable Int64 where 
    sign = orientation
    {-# INLINE sign #-}
instance Signable Float where 
    sign = orientation
    {-# INLINE sign #-}
instance Signable Double where 
    sign = orientation
    {-# INLINE sign #-}
instance Signable CFloat where 
    sign = orientation
    {-# INLINE sign #-}
instance Signable CDouble where 
    sign = orientation
    {-# INLINE sign #-}
instance (Integral a) => Signable (Ratio a) where
    sign = orientation
    {-# INLINE sign #-}
        

instance SignedIntegral Int
instance SignedIntegral Int8
instance SignedIntegral Int16
instance SignedIntegral Int32
instance SignedIntegral Int64
instance SignedIntegral Integer
        

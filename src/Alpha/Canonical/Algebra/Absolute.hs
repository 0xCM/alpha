module Alpha.Canonical.Algebra.Absolute
(
    Absolute(..),

)
where
import Alpha.Canonical.Relations

    -- Characterizes types that support a notion of absolute/unsigned value
class Absolute a where
    abs::a -> a

instance Absolute Natural where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolute Integer where 
    abs = fromIntegral.abs'
    {-# INLINE abs #-}
instance Absolute Int where 
    abs = fromIntegral.abs'
    {-# INLINE abs #-}
instance Absolute Int8 where 
    abs = fromIntegral.abs'
    {-# INLINE abs #-}
instance Absolute Int16 where 
    abs = fromIntegral.abs'
    {-# INLINE abs #-}
instance Absolute Int32 where 
    abs = fromIntegral.abs'
    {-# INLINE abs #-}
instance Absolute Int64 where 
    abs = fromIntegral.abs'
    {-# INLINE abs #-}
instance Absolute Word where 
    abs = abs'
    {-# INLINE abs #-}    
instance Absolute Word8 where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolute Word16 where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolute Word32 where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolute Word64 where 
    abs = abs'
    {-# INLINE abs #-}
instance (Integral a) => Absolute (Ratio a) where
    abs = abs'
instance Absolute Float where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolute Double where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolute CFloat where 
    abs =  abs'     
    {-# INLINE abs #-}
instance Absolute CDouble where 
    abs  = abs'
    {-# INLINE abs #-}

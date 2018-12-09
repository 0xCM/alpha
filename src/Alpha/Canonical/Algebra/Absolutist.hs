module Alpha.Canonical.Algebra.Absolutist
(
    
    Absolutist(..)    

) where
import Alpha.Base
import Alpha.Native

class Absolutist a where
    abs::a -> a

instance Absolutist Natural where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist Integer where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist Int where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist Int8 where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist Int16 where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist Int32 where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist Int64 where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist Word where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist Word8 where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist Word16 where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist Word32 where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist Word64 where 
    abs = abs'
    {-# INLINE abs #-}
instance Integral a => Absolutist (Ratio a) where 
    abs = abs'
    {-# INLINE abs #-}    
instance Absolutist Float where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist Double where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist CFloat where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolutist CDouble where 
    abs = abs'
    {-# INLINE abs #-}

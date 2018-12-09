module Alpha.Canonical.Algebra.Semigroup
(
    AbelianSemigroup(..)
) where
import Alpha.Base 
import Alpha.Native

import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Additive

class (Semigroup a, Additive a, Subtractive a) 
    => AbelianSemigroup a where    

        
-- Semigroup
-------------------------------------------------------------------------------
instance Semigroup Natural where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup Integer where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup Int where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup Int8 where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup Int16 where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup Int32 where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup Int64 where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup Word where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup Word8 where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup Word16 where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup Word32 where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup Word64 where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Integral a => Semigroup (Ratio a) where 
    (<>) = add'
    {-# INLINE (<>) #-}    
instance Semigroup Float where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup Double where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup CFloat where 
    (<>) = add'
    {-# INLINE (<>) #-}
instance Semigroup CDouble where 
    (<>) = add'
    {-# INLINE (<>) #-}

instance AbelianSemigroup Int
instance AbelianSemigroup Int8
instance AbelianSemigroup Int16
instance AbelianSemigroup Int32
instance AbelianSemigroup Int64
instance AbelianSemigroup Integer
instance AbelianSemigroup Word
instance AbelianSemigroup Word8
instance AbelianSemigroup Word16
instance AbelianSemigroup Word32
instance AbelianSemigroup Word64
instance AbelianSemigroup Natural
instance (Integral a) => AbelianSemigroup (Ratio a)
    
module Alpha.Canonical.Algebra.Monoid
(
    alt    

) where
import Alpha.Base    
import Alpha.Canonical.Algebra.Semigroup
import qualified Data.Monoid as Monoid


-- Lifts the input into the Alt monoid
-- Example:
-- alt Nothing  <> alt (Just 4) <> alt (Just 7)
-- >> Alt {getAlt = Just 4}
alt::Monoid a => f a -> Monoid.Alt f a
alt = Monoid.Alt

-- Monoid
-------------------------------------------------------------------------------
instance Monoid Natural where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid Integer where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid Int where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid Int8 where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid Int16 where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid Int32 where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid Int64 where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid Word where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid Word8 where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid Word16 where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid Word32 where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid Word64 where 
    mempty = 0
    {-# INLINE mempty #-}
instance (Integral a) => Monoid (Ratio a) where 
    mempty = 0
    {-# INLINE mempty #-}    
instance Monoid Float where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid Double where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid CFloat where 
    mempty = 0
    {-# INLINE mempty #-}
instance Monoid CDouble where 
    mempty = 0
    {-# INLINE mempty #-}

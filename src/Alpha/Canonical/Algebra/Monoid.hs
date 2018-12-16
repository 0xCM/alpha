module Alpha.Canonical.Algebra.Monoid
(
    MonoidalAlt, MonoidalProduct, MonoidalSum,
    altM, sumM, prodM

) where
import Alpha.Base    
import Alpha.Canonical.Algebra.Semigroup
import qualified Data.Monoid as Monoid

type MonoidalAlt f a = Monoid.Alt f a

type MonoidalSum a = Monoid.Sum a

type MonoidalProduct a = Monoid.Product a

-- Lifts the input into the Alt monoid
-- Example:
-- alt Nothing  <> alt (Just 4) <> alt (Just 7)
-- >> Alt {getAlt = Just 4}
altM::Monoid a => f a -> MonoidalAlt f a
altM = Monoid.Alt

sumM::Monoid a => a -> MonoidalSum a
sumM = Monoid.Sum

prodM::Monoid a => a -> MonoidalProduct a
prodM = Monoid.Product
    
-- Monoid
-------------------------------------------------------------------------------
-- instance Monoid Natural where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid Integer where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid Int where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid Int8 where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid Int16 where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid Int32 where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid Int64 where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid Word where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid Word8 where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid Word16 where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid Word32 where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid Word64 where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance (Integral a) => Monoid (Ratio a) where 
--     mempty = 0
--     {-# INLINE mempty #-}    
-- instance Monoid Float where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid Double where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid CFloat where 
--     mempty = 0
--     {-# INLINE mempty #-}
-- instance Monoid CDouble where 
--     mempty = 0
--     {-# INLINE mempty #-}

        
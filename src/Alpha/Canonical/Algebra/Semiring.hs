module Alpha.Canonical.Algebra.Semiring
(
    Semiring(..)
    

) where
import Alpha.Base    
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Monoid
import Alpha.Canonical.Algebra.Unital
import Alpha.Canonical.Algebra.Nullary

-- | Almost A ring; elements are not required though to have an additive inverse
class (Additive a, Nullary a, Multiplicative a, Unital a) 
    => Semiring a where

-- Semiring (+) (*)
-------------------------------------------------------------------------------
instance Semiring Natural
instance Semiring Integer
instance Semiring Int
instance Semiring Int8
instance Semiring Int16
instance Semiring Int32
instance Semiring Int64
instance Semiring Word
instance Semiring Word8
instance Semiring Word16
instance Semiring Word32
instance Semiring Word64
instance (Integral a, Ord a) => Semiring (Ratio a)
instance Semiring Float
instance Semiring Double
instance Semiring CFloat
instance  Semiring CDouble

instance (Ord a, Unital a, Multiplicative a) => Semiring (ItemSet a)
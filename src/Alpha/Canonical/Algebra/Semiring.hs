module Alpha.Canonical.Algebra.Semiring
(
    Semiring(..)
    

) where
import Alpha.Canonical.Common    
import Alpha.Canonical.Algebra.Monoidal
import Alpha.Canonical.Algebra.Abelian
import Alpha.Canonical.Algebra.Distributive

-- | An additive monoid - via 'Abelian' with a multiplicative 
-- monoid - via 'Monoidal' such that multiplication distributes 
-- over addition - via 'Distributive'
-- The most elementary algebraic structure that supports both
-- addition and multiplication
-- See https://en.wikipedia.org/wiki/Semiring
class (Abelian a, Monoidal a, Distributive a) 
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


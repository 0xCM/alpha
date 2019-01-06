module Alpha.Canonical.Structures.Semiring
(
    module X,
    Semiring(..),
    interval
    

) where
import Alpha.Canonical.Algebra
import Alpha.Canonical.Structures.Monoid as X

-- | An additive monoid - via 'Abelian' with a multiplicative 
-- monoid - via 'Monoidal' such that multiplication distributes 
-- over addition - via 'Distributive'
-- The most elementary algebraic structure that supports both
-- addition and multiplication
-- See https://en.wikipedia.org/wiki/Semiring
class (SumMonoid a, ProductMonoid a, Distributive a) 
    => Semiring a where
        
--instance (Abelian a, Monoidal a, Distributive a) => Semiring a

interval::(Ord a, Semiring a) => a -> a -> Interval a
interval = interval'

instance Structure 1 Semiring

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
instance Semiring CDouble

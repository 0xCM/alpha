-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Structures.Semiring
(
    Semiring(..),
    OrdSemiring(..),
    ProductMonoid(..),
    AdditiveMonoid(..),
    interval
) where
import Alpha.Canonical.Algebra
import qualified Data.Monoid as Monoid

type MonoidalAlt f a = Monoid.Alt f a
type MonoidalSum a = Monoid.Sum a
type MonoidalProduct a = Monoid.Product a

-- | A multiplicative struture with an identity element    
type ProductMonoid a = (Unital a, Multiplicative a, Eq a) 

-- | A additive struture with an identity element    
type AdditiveMonoid a = (Nullary a, Additive a, Eq a) 
    
-- | An additive monoid - via 'Abelian' with a multiplicative 
-- monoid - via 'Monoidal' such that multiplication distributes 
-- over addition - via 'Distributive'
-- The most elementary algebraic structure that supports both
-- addition and multiplication
-- See https://en.wikipedia.org/wiki/Semiring
class (AdditiveMonoid a, ProductMonoid a, Distributive a) 
    => Semiring a where
        
-- | Synonym that joins the 'Ord' and 'Semiring' constraints
-- to form the concept of an ordered semiring
type OrdSemiring a = (Ord a, Semiring a)        

-- | Constructs a contiguous sequence of values in an ordered semiring
interval::(OrdSemiring a) => a -> a -> Interval a
interval = interval'

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

module Alpha.Canonical.Algebra.Group
(
    Group(..),
    AbelianGroup(..),
    commutator,
    MonoidalAlt, MonoidalProduct, MonoidalSum,
    altM, sumM, prodM

) where
import Alpha.Base
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Reciprocative
import Alpha.Canonical.Algebra.Negatable

import Alpha.Canonical.Relations
import Alpha.Canonical.Operators
import qualified Data.Monoid as Monoid

type MonoidalAlt f a = Monoid.Alt f a
type MonoidalSum a = Monoid.Sum a
type MonoidalProduct a = Monoid.Product a
    
class (Monoid a, Invertible a) => Group a where    

class Invertible a where
    invert::a -> a

-- | A group for which the related commutator is always satisfied
class (Semigroup a, Additive a, Negatable a) => AbelianGroup a where

-- | Constructs a commutator for a binary operator
-- See https://en.wikipedia.org/wiki/Commutator
commutator::(Reciprocative a) => O2 a -> O2 a
commutator o =  \x y ->  o (o (reciprocal x) (reciprocal y)) (o x y) where

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

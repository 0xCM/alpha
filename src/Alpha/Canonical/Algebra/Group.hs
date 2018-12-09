module Alpha.Canonical.Algebra.Group
(
    Invertible(..),
    Group(..),
    AbelianGroup(..),
    commutator,
    

) where
import Alpha.Base
import Alpha.Canonical.Algebra.Unital
import Alpha.Canonical.Algebra.Monoid
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Semigroup
import Alpha.Canonical.Algebra.Nullary

import Alpha.Canonical.Relations
import Alpha.Canonical.Operators

-- | Characterizes types whose values are closed under inversion with respect to multiplicative inversion
class Invertible a where
    invert::a -> a

class (Unital a, Invertible a, Monoid a) => Group a where    

-- | A group for which the related commutator is always satisfied
class (Semigroup a, Additive a, Subtractive a, Nullary a) => AbelianGroup a where

instance AbelianGroup Integer
instance AbelianGroup Int
instance AbelianGroup Int8
instance AbelianGroup Int16
instance AbelianGroup Int32
instance AbelianGroup Int64
instance (Integral a) => AbelianGroup (Ratio a)
    
-- | Constructs a commutator for a binary operator
-- See https://en.wikipedia.org/wiki/Commutator
commutator::(Invertible a) => BinaryOperator a -> (a -> a -> a)
commutator o =  \x y ->  o (o (invert x) (invert y)) (o x y) where

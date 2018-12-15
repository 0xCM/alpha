module Alpha.Canonical.Algebra.Group
(
    Group(..),
    AbelianGroup(..),
    commutator,
    

) where
import Alpha.Base
import Alpha.Canonical.Algebra.Monoid
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Semigroup
import Alpha.Canonical.Algebra.Negatable
import Alpha.Canonical.Algebra.Invertible

import Alpha.Canonical.Relations
import Alpha.Canonical.Operators


class (Multiplicative a, Invertible a, Monoid a) => Group a where    

type GroupSubset a = (Group a, Subset a (Group a))

class (GroupSubset a) => Subgroup a where

class Subgroup a => LeftCoset a where
    

--class ((Subset (Subgroup a) (Group a)), Group a) => Subgroup a where

-- | A group for which the related commutator is always satisfied
class (Semigroup a, Additive a, Negatable a) => AbelianGroup a where


-- | Constructs a commutator for a binary operator
-- See https://en.wikipedia.org/wiki/Commutator
commutator::(Invertible a) => BinaryOperator a -> (a -> a -> a)
commutator o =  \x y ->  o (o (invert x) (invert y)) (o x y) where

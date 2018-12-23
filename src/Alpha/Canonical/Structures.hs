module Alpha.Canonical.Structures 
(
    Multiplicative(..),
    Unital(..),
    Invertible(..),
    Additive(..),
    Nullary(..),
    Semigroup(..),
    Monoid(..),
    Group(..),
    DiscreteGroup(..),
    Subgroup(..),
    Semiring(..),
)
where

import Alpha.Canonical.Algebra hiding (Group,Multiplicative,Unital,Nullary,Additive,Semiring,Monoid,Semigroup)
import qualified Alpha.Canonical.Algebra as A
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map

import Alpha.Canonical.Text.Asci

-- Characterizes a structure whose elements are invertible
class Structure a => Invertible a where
    -- | Computes the inverse of an Individual
    invert::Individual a -> Individual a

-- | Characterizes structures for which element multiplication is defined
class Structure a => Multiplicative a where
    -- | Defines a structure's multiplication operator
    (*)::Individual a -> Individual a -> Individual a

class Multiplicative a => Semigroup a where        

class Semigroup a => Unital a where
    -- | Produces the multiplicative identity
    one::Individual a

class Unital a => Monoid a where    

    
class Structure a => Additive a where
    -- | Defines a structure's addition operator
    (+)::Individual a -> Individual a -> Individual a

class Additive a => Nullary a where
    -- | Produces the additive identity
    zero::Individual a
    

class Nullary a  => Negatable a where
    -- | Computes the additive inverse of an Individual
    negate::Individual a -> Individual a    


-- | Defines a group over a specified 'Monoid' and 'Invertible'
class (Invertible a) => Group a where

class (Discrete a, Group a) => DiscreteGroup a where

class (Group a, Subset (Individual a) a) => Subgroup a where
    
-- class (Substructure a) => DiscreteSubgroup a where
--     subgroup::a -> [Individual a]
--     subgroup = members

-- | Defines a semiring via the joined context
class (Unital a, Nullary a) => Semiring a where

newtype LeftCoset a = LeftCoset [Individual a]
type instance Element (LeftCoset a) = a



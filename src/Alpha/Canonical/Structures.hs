module Alpha.Canonical.Structures 
(
    Structure(..),
    Multiplicative(..),
    Unital(..),
    Invertible(..),
    Additive(..),
    Nullary(..),
    Semigroup(..),
    Monoid(..),
    Group(..),
    Semiring(..),
)
where
import Alpha.Base hiding (Semigroup, Monoid)

import Alpha.Canonical.Elementary

class Structure a where
    -- | Extracts the elements from a structure
    elements::a -> [Element a]

instance Set (Structure a)

class Subset a where
    -- | Extracts a subset of elements from a structure
    subset::a -> [Element a]

class Multiplicative a where
    -- | Defines a structure's multiplication operator
    (*)::Element a -> Element a -> Element a

class Unital a where
    -- | Produces the multiplicative identity
    one::Element a
    
class Invertible a where
    -- | Computes the multiplicative inverse of an element
    invert::Element a -> Element a    
        
class Additive a where
    -- | Defines a structure's addition operator
    (+)::Element a -> Element a -> Element a

class Nullary s where
    -- | Produces the additive identity
    zero::Element a
    
class Negatable a where
    -- | Computes the additive inverse of an element
    negate::Element a -> Element a    

-- | Defines a semigroup for a specified 'Multiplicative'
class (Structure a, Multiplicative a) => Semigroup a where
    
-- | Defines a monoid over a specified 'Semigroup' and 'Unitial'
class (Structure a, Multiplicative a, Unital a) => Monoid a where    

-- | Defines a group over a specified 'Monoid' and 'Invertible'
class (Structure a, Multiplicative a, Unital a, Invertible a) => Group a where

-- | Defines a subroup 
class (Group a, Subset a) => Subgroup a where
    subgroup::a -> [Element a]
    subgroup = subset

-- | Defines a semiring via the joined context
class (Multiplicative a, Unital a, Additive a, Nullary a) => Semiring a where

newtype LeftCoset a = LeftCoset [Element a]
type instance Element (LeftCoset a) = a



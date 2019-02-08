module Alpha.Canonical.Structures.Field
(
    module X,
    Field(..),
    Re(..), Cx(..), NumberField(..),
)
where
import Alpha.Canonical.Relations    
import Alpha.Canonical.Structures.Ring as X
import Alpha.Canonical.Structures.Domain as X


-- | Characterizes a field as a commutative division ring
class (Commutative a, DivisionRing a) => Field a where

-- | Characterizes a field with a finite number of elements
-- See https://en.wikipedia.org/wiki/Finite_field
class (Field a, FinitelyCountable a) => FiniteField a where

instance (Integral a) => Field (Ratio a) where 

-- The (approximate) reals
instance Field Float where 
instance Field Double where 
instance Field CFloat where 
instance Field CDouble where

-- | Represents the set of real numbers
data Re a = Re

-- | Represents the set of complex numbers
data Cx a = Cx

-- | Represents a disjoint union of number fields
data NumberField a =
      RealField (Re a)
    | CompField (Cx a)
        
    

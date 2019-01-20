module Alpha.Canonical.Structures.Field
(
    module X,
    Field(..),

)
where
import Alpha.Canonical.Relations    
import Alpha.Canonical.Structures.Ring as X
import Alpha.Canonical.Structures.Domain as X


-- | Characterizes a field as a commutative division ring
class (Commutative a, DivisionRing a) => Field a where

-- | Characterizes a field with a finite number of elements
-- See https://en.wikipedia.org/wiki/Finite_field
class (Field a, Finite a) => FiniteField a where

instance (Integral a) => Field (Ratio a) where 

-- The (approximate) reals
instance Field Float where 
instance Field Double where 
instance Field CFloat where 
instance Field CDouble where

        
    

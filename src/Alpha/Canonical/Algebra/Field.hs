module Alpha.Canonical.Algebra.Field
(
    module X,
    FieldAdd(..),
    FieldMul(..),
    FieldSub(..),
    FieldDiv(..),
    Field(..),

)
where
import Alpha.Canonical.Relations    
import Alpha.Canonical.Algebra.Ring as X
import Alpha.Canonical.Algebra.IntegralDomain as X

-- | Specifies field addition constraint
type FieldAdd a = (Additive a, Nullary a)

-- | Specifies field multiplication constraint
type FieldMul a = (Multiplicative a, Unital a)

-- | Specifies field subraction constraint
type FieldSub a = (Subtractive a, Negatable a)

-- | Specifies field division constraint
type FieldDiv a = (Divisive a, Reciprocative a)

-- | Specifies a field predicated on conforming division, multiplication, subtraction
-- and division operations
class (FieldAdd a, FieldSub a, FieldMul a, FieldDiv a, Distributive a) => Field a where


-- The rationals
instance (Integral a) => Field (Ratio a) where 

-- The (approximate) reals
instance Field Float where 
instance Field Double where 
instance Field CFloat where 
instance Field CDouble where 
        
    

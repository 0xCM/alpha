module Alpha.Canonical.Algebra.Divisive
(
    Divisive(..),
    Quotient(..), 
    Bidivisive(..),

) where
import Alpha.Base hiding(div)
import Alpha.Native
import Alpha.Canonical.Relations
import Alpha.Canonical.Operators
import qualified Data.List as List

-- | Represents a family of types that support a notion of (potentially) heterogenous division
-- where a type instance is the type of the result of applying a conforming quotient operator
type family Quotient a b     

-- / Characterizes a type that supports a notion of division
class Divisive a where
    -- | Divides the first operand by the second
    div::O2 a

    -- | Infix synonym for 'div'
    (/)::O2 a
    (/) = div
    {-# INLINE (/) #-}
    infixl 8 /    

newtype Division a = Divisive (O2 a)
    
type OpK f a = (f ~ Division a, Divisive a)    

data instance Operator 2 (Division a) 
    = Division (O2 a)

instance OpK f a => Operative 2 f a where
    type OpArgs 2 f a = (a,a)

    operator = Division div 
    {-# INLINE operator #-}        

    evaluate (a1,a2) = f a1 a2 where (Division f) = operator 
    {-# INLINE evaluate #-}        

-- | Characterizes pairs of types that support a notion of division
class Bidivisive a b where
    -- | Divides the first value by the second        
    bidiv::a -> b -> Quotient a b

    -- | Infix synonym for 'hdiv'
    (>/<)::a -> b -> Quotient a b
    (>/<) = bidiv
    {-# INLINE (>/<) #-}        
    infixl 8 >/<

instance Divisive Natural where 
    div = quot
    {-# INLINE div #-}
instance Divisive Integer where 
    div = quot
    {-# INLINE div #-}
instance Divisive Int where 
    div = quot
    {-# INLINE div #-}
instance Divisive Int8 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Int16 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Int32 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Int64 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Word where 
    div = quot
    {-# INLINE div #-}
instance Divisive Word8 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Word16 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Word32 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Word64 where 
    div = quot
    {-# INLINE div #-}
instance (Integral a) => Divisive (Ratio a) where 
    div = divf
    {-# INLINE div #-}    
instance Divisive Float where 
    div = divf
    {-# INLINE div #-}
instance Divisive Double where 
    div = divf
    {-# INLINE div #-}
instance Divisive CFloat where 
    div = divf
    {-# INLINE div #-}
instance Divisive CDouble where 
    div = divf
    {-# INLINE div #-}


module Alpha.Canonical.Algebra.Additive
(
    Bisum(..), Additive(..), HAdditive(..),    
    

) where
import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Native

-- | Represents a family of types that support a notion of (potentially) heterogenous addition
-- where a type instance is the addition result type
type family Bisum a b

-- / Characterizes a type that supports a notion of  *commutative* addition      
-- Thus, the following invariant must hold:
-- forall a b => add a b = add b a
class Additive a where
    -- | Adds the first operand with the second
    add::BinaryOperator a
    
    -- | Infix synonym for 'add'    
    (+)::BinaryOperator a
    (+) = add
    {-# INLINE (+) #-}
    infixl 6 +


-- | Characterizes pairs of types that support a notion multiplication and
-- such mutltiplication need not be commutative so, in general,
-- hadd a + b != b + a
class HAdditive a b where
    -- | Adds the first operand with the second
    hadd::a -> b -> Bisum a b

    -- | Infix synonym for 'hadd'
    (>+<)::a -> b -> Bisum a b
    (>+<) = hadd
    {-# INLINE (>+<) #-}
    infixl 6 >+<


instance Additive Natural where 
    add = add'
    {-# INLINE add #-}
instance Additive Integer where 
    add = add'
    {-# INLINE add #-}
instance Additive Int where 
    add = add'
    {-# INLINE add #-}
instance Additive Int8 where 
    add = add'
    {-# INLINE add #-}
instance Additive Int16 where 
    add = add'
    {-# INLINE add #-}
instance Additive Int32 where 
    add = add'
    {-# INLINE add #-}
instance Additive Int64 where 
    add = add'
    {-# INLINE add #-}
instance Additive Word where 
    add = add'
    {-# INLINE add #-}
instance Additive Word8 where 
    add = add'
    {-# INLINE add #-}
instance Additive Word16 where 
    add = add'
    {-# INLINE add #-}
instance Additive Word32 where 
    add = add'
    {-# INLINE add #-}
instance Additive Word64 where 
    add = add'
    {-# INLINE add #-}
instance (Integral a) => Additive (Ratio a) where 
    add = add'
    {-# INLINE add #-}    
instance Additive Float where 
    add = add'
    {-# INLINE add #-}
instance Additive Double where 
    add = add'
    {-# INLINE add #-}
instance Additive CFloat where 
    add = add'
    {-# INLINE add #-}
instance Additive CDouble where 
    add = add'
    {-# INLINE add #-}

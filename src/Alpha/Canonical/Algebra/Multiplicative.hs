module Alpha.Canonical.Algebra.Multiplicative
(
    Biproduct(..), Multiplicative(..), HMultiplicative(..), 
    

) where
import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Native


-- | Represents a family of types that support a notion of (potentially) heterogenous multiplication
-- where a type instance is the multiplication result type
type family Biproduct a b

-- / Characterizes a type that supports a notion of *associative* multiplication    
-- mul a b == mul b a
class Multiplicative a where
    -- | Multiplies the first operand by the second
    mul::BinaryOperator a

    -- | Infix synonym for 'mul'
    (*)::BinaryOperator a
    (*) = mul
    {-# INLINE (*) #-}
    infixl 7 *

-- | Characterizes pairs of types that support a notion multiplication
class HMultiplicative a b where
    -- | Multiplies the first operand by the second
    hmul::a -> b -> Biproduct a b

    -- | Infix synonym for 'hmul'
    (>*<)::a -> b -> Biproduct a b
    (>*<) = hmul
    {-# INLINE (>*<) #-}    
    infixl 7 >*<


-- Multiplicative
-------------------------------------------------------------------------------
instance Multiplicative Natural where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative Integer where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative Int where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative Int8 where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative Int16 where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative Int32 where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative Int64 where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative Word where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative Word8 where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative Word16 where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative Word32 where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative Word64 where 
    mul = mul'
    {-# INLINE mul #-}
instance (Integral a) => Multiplicative (Ratio a) where 
    mul = mul'
    {-# INLINE mul #-}    
instance Multiplicative Float where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative Double where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative CFloat where 
    mul = mul'
    {-# INLINE mul #-}
instance Multiplicative CDouble where 
    mul = mul'
    {-# INLINE mul #-}

module Alpha.Canonical.Algebra.Subtractive
(
    
    Delta(..), Subtractive(..), HSubtractive(..)
    
) where
import Alpha.Base hiding(div)
import Alpha.Canonical.Operators
import Alpha.Native


-- | Represents a family of types that support a notion of (potentially) heterogeneous 
-- subtraction where the instance type is the result type of applying a 
-- conforming subtraction operation
type family Delta a b

-- / Characterizes a type that supports a notion of subtraction
class Subtractive a where
    -- | Subracts the second value from the first
    sub::BinaryOperator a

    -- | Infix synonym for 'sub'    
    (-)::BinaryOperator a
    (-) = sub
    {-# INLINE (-) #-}
    infixl 6 -    

-- / Characterizes a pair of type that supports a notion of heterogenious subtraction
class HSubtractive a b where
    -- | Calculates the difference between the first value and the second
    hsub::a -> b -> Delta a b

    -- | Infix synonym for 'hsub'        
    (>-<)::a -> b -> Delta a b
    (>-<) = hsub
    infixl 6 >-<    
    
-- Subtractive
-------------------------------------------------------------------------------
instance Subtractive Natural where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Integer where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int8 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int16 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int32 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int64 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word8 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word16 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word32 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word64 where 
    sub = sub'
    {-# INLINE sub #-}
instance (Integral a) => Subtractive (Ratio a) where 
    sub = sub'
    {-# INLINE sub #-}    
instance Subtractive Float where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Double where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive CFloat where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive CDouble where 
    sub = sub'
    {-# INLINE sub #-}

